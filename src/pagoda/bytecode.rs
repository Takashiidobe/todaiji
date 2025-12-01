use std::io::Write;

use std::collections::HashMap;

use thiserror::Error;

use crate::pagoda::parser::BinOp;
use crate::pagoda::{CheckedProgram, Expr, Span, Stmt};

const WORD_SIZE: usize = 8;

#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("I/O while emitting assembly: {source}")]
    Io { source: std::io::Error, span: Span },
    #[error("unknown variable '{name}'")]
    UnknownVariable { name: String, span: Span },
    #[error("unknown function '{name}'")]
    UnknownFunction { name: String, span: Span },
}

impl BytecodeError {
    pub fn span(&self) -> &Span {
        match self {
            BytecodeError::Io { span, .. } => span,
            BytecodeError::UnknownVariable { span, .. } => span,
            BytecodeError::UnknownFunction { span, .. } => span,
        }
    }
}

/// Emit assembly that exits with the integer literal contained in `program`.
pub fn emit_exit_program(
    program: &CheckedProgram,
    mut writer: impl Write,
) -> Result<(), BytecodeError> {
    let mut function_labels: HashMap<String, String> = HashMap::new();
    for func in &program.functions {
        function_labels.insert(func.name.clone(), format!("fn_{}", func.name));
    }

    // Ensure execution starts at main by jumping over function bodies.
    writeln!(writer, "  jmp main").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    for func in &program.functions {
        emit_function(func, &mut writer, &function_labels)?;
    }

    writeln!(writer, "main:").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    let mut env: HashMap<String, VarSlot> = HashMap::new();
    let mut stack_depth_words: usize = 0;
    let needs_ret_label = program.stmts.iter().any(|s| stmt_contains_return(&s.stmt));

    for checked in &program.stmts {
        emit_stmt(
            &checked.stmt,
            &mut writer,
            &mut env,
            &mut stack_depth_words,
            if needs_ret_label { Some("ret_exit") } else { None },
            &function_labels,
        )?;
    }
    if needs_ret_label {
        writeln!(writer, "ret_exit:").map_err(|e| BytecodeError::Io {
            source: e,
            span: program.span.clone(),
        })?;
    }
    writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    stack_depth_words += 1;
    writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    stack_depth_words = stack_depth_words.saturating_sub(1);
    writeln!(writer, "  movi %r0, $60").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    let _ = stack_depth_words;
    Ok(())
}

#[derive(Debug, Clone)]
struct VarSlot {
    depth: usize, // number of pushes so far (stack_depth_words) when defined
}

fn stmt_contains_return(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Return { .. } => true,
        Stmt::Block { stmts, .. } => stmts.iter().any(stmt_contains_return),
        Stmt::If {
            then_branch,
            else_branch,
            ..
        } => {
            stmt_contains_return(then_branch)
                || else_branch
                    .as_ref()
                    .map(|b| stmt_contains_return(b))
                    .unwrap_or(false)
        }
        Stmt::For { init, body, .. } => {
            init.as_ref()
                .map(|s| stmt_contains_return(s))
                .unwrap_or(false)
                || stmt_contains_return(body)
        }
        _ => false,
    }
}

fn fresh_label() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("label_{id}")
}

fn emit_function(
    func: &crate::pagoda::CheckedFunction,
    writer: &mut impl Write,
    function_labels: &HashMap<String, String>,
) -> Result<(), BytecodeError> {
    let label = function_labels
        .get(&func.name)
        .cloned()
        .unwrap_or_else(|| format!("fn_{}", func.name));
    let ret_label = format!("{label}_ret");
    writeln!(writer, "{label}:").map_err(|e| BytecodeError::Io {
        source: e,
        span: func.span.clone(),
    })?;
    let mut env: HashMap<String, VarSlot> = HashMap::new();
    let mut stack_depth_words: usize = 0;
    emit_stmt(
        &func.body.stmt,
        writer,
        &mut env,
        &mut stack_depth_words,
        Some(ret_label.as_str()),
        function_labels,
    )?;
    emit_pop_all_locals(writer, stack_depth_words)?;
    writeln!(writer, "  jmp {ret_label}").map_err(|e| BytecodeError::Io {
        source: e,
        span: func.span.clone(),
    })?;
    writeln!(writer, "{ret_label}:").map_err(|e| BytecodeError::Io {
        source: e,
        span: func.span.clone(),
    })?;
    writeln!(writer, "  ret").map_err(|e| BytecodeError::Io {
        source: e,
        span: func.span.clone(),
    })
}

fn emit_stmt(
    stmt: &Stmt,
    writer: &mut impl Write,
    env: &mut HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    return_label: Option<&str>,
    function_labels: &HashMap<String, String>,
) -> Result<(), BytecodeError> {
    match stmt {
        Stmt::Expr { expr, .. } => emit_expr(expr, writer, env, stack_depth_words, function_labels),
        Stmt::Empty { .. } => Ok(()),
        Stmt::Let { name, expr, .. } => {
            emit_expr(expr, writer, env, stack_depth_words, function_labels)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: expr.span().clone(),
            })?;
            *stack_depth_words += 1;
            env.insert(
                name.clone(),
                VarSlot {
                    depth: *stack_depth_words,
                },
            );
            Ok(())
        }
        Stmt::Return { expr, .. } => {
            emit_expr(expr, writer, env, stack_depth_words, function_labels)?;
            emit_pop_all_locals(writer, *stack_depth_words)?;
            if let Some(label) = return_label {
                writeln!(writer, "  jmp {label}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: expr.span().clone(),
                })?;
            }
            Ok(())
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            span,
        } => {
            let depth_before = *stack_depth_words;
            emit_expr(cond, writer, env, stack_depth_words, function_labels)?;
            let else_label = fresh_label();
            let end_label = fresh_label();
            writeln!(writer, "  brz.w %r0, {else_label}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = depth_before;
            emit_stmt(
                then_branch,
                writer,
                env,
                stack_depth_words,
                return_label,
                function_labels,
            )?;
            if let Some(else_branch) = else_branch {
                writeln!(writer, "  jmp {end_label}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                writeln!(writer, "{else_label}:").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = depth_before;
                emit_stmt(
                    else_branch,
                    writer,
                    env,
                    stack_depth_words,
                    return_label,
                    function_labels,
                )?;
                writeln!(writer, "{end_label}:").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
            } else {
                writeln!(writer, "{else_label}:").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
            }
            *stack_depth_words = depth_before;
            Ok(())
        }
        Stmt::For {
            init,
            cond,
            post,
            body,
            span,
        } => {
            let saved_env = env.clone();
            let depth_before = *stack_depth_words;

            if let Some(init_stmt) = init {
                emit_stmt(
                    init_stmt,
                    writer,
                    env,
                    stack_depth_words,
                    return_label,
                    function_labels,
                )?;
            }

            let loop_label = fresh_label();
            let end_label = fresh_label();
            writeln!(writer, "{loop_label}:").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;

            let cond_depth = *stack_depth_words;
            if let Some(cond_expr) = cond {
                emit_expr(cond_expr, writer, env, stack_depth_words, function_labels)?;
                writeln!(writer, "  brz.w %r0, {end_label}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = cond_depth;
            }

            emit_stmt(
                body,
                writer,
                env,
                stack_depth_words,
                return_label,
                function_labels,
            )?;
            *stack_depth_words = cond_depth;

            if let Some(post_expr) = post {
                emit_expr(post_expr, writer, env, stack_depth_words, function_labels)?;
                *stack_depth_words = cond_depth;
            }

            writeln!(writer, "  jmp {loop_label}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "{end_label}:").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;

            let locals_to_pop = stack_depth_words.saturating_sub(depth_before);
            for _ in 0..locals_to_pop {
                writeln!(writer, "  pop.w %r15").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = stack_depth_words.saturating_sub(1);
            }

            *env = saved_env;
            Ok(())
        }
        Stmt::Block { stmts, span } => {
            emit_block(
                stmts,
                span,
                writer,
                env,
                stack_depth_words,
                return_label,
                function_labels,
            )
        }
    }
}

fn emit_pop_all_locals(writer: &mut impl Write, mut depth: usize) -> Result<(), BytecodeError> {
    while depth > 0 {
        writeln!(writer, "  pop.w %r15").map_err(|e| BytecodeError::Io {
            source: e,
            span: Span {
                start: 0,
                end: 0,
                literal: String::new(),
            },
        })?;
        depth -= 1;
    }
    Ok(())
}

fn emit_expr(
    expr: &Expr,
    writer: &mut impl Write,
    env: &HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    function_labels: &HashMap<String, String>,
) -> Result<(), BytecodeError> {
    match expr {
        Expr::IntLiteral { value, span } => writeln!(
            writer,
            "  load.w %r0, ${value}  # span {}..{} \"{}\"",
            span.start, span.end, span.literal
        )
        .map_err(|e| BytecodeError::Io {
            source: e,
            span: span.clone(),
        }),
        Expr::Var { name, span } => {
            let Some(slot) = env.get(name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            if *stack_depth_words < slot.depth {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let offset_words = *stack_depth_words - slot.depth;
            let disp_bytes = (offset_words * WORD_SIZE) as i64;
            writeln!(
                writer,
                "  load.w %r0, {}(%sp)  # span {}..{} \"{}\"",
                disp_bytes, span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::Unary { op, expr, span } => {
            emit_expr(expr, writer, env, stack_depth_words, function_labels)?;
            match op {
                crate::pagoda::parser::UnaryOp::Plus => Ok(()),
                crate::pagoda::parser::UnaryOp::Minus => writeln!(
                    writer,
                    "  neg.w %r0  # span {}..{} \"{}\"",
                    span.start, span.end, span.literal
                )
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                }),
                crate::pagoda::parser::UnaryOp::BitNot => writeln!(
                    writer,
                    "  not.w %r0  # span {}..{} \"{}\"",
                    span.start, span.end, span.literal
                )
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                }),
            }
        }
        Expr::Assign { name, value, span } => {
            emit_expr(value, writer, env, stack_depth_words, function_labels)?;
            let Some(slot) = env.get(name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            if *stack_depth_words < slot.depth {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let offset_words = *stack_depth_words - slot.depth;
            let disp_bytes = (offset_words * WORD_SIZE) as i64;
            writeln!(
                writer,
                "  store.w %r0, {}(%sp)  # span {}..{} \"{}\"",
                disp_bytes, span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::CompoundAssign {
            name,
            op,
            value,
            span,
        } => {
            emit_expr(value, writer, env, stack_depth_words, function_labels)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;

            let Some(slot) = env.get(name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            if *stack_depth_words < slot.depth {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let load_offset_words = *stack_depth_words - slot.depth;
            let load_disp_bytes = (load_offset_words * WORD_SIZE) as i64;
            writeln!(
                writer,
                "  load.w %r0, {}(%sp)  # span {}..{} \"{}\"",
                load_disp_bytes, span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);

            let op_instr = match op {
                BinOp::Add => "add.w",
                BinOp::Sub => "sub.w",
                BinOp::Mul => "mul.w",
                BinOp::Div => "divmod.w",
                BinOp::Shl => "shl.w",
                BinOp::Shr => "sar.w",
                BinOp::BitAnd => "and.w",
                BinOp::BitOr => "or.w",
                BinOp::BitXor => "xor.w",
                _ => unreachable!("compound assign only supports arithmetic ops"),
            };
            writeln!(
                writer,
                "  {op_instr} %r0, %r1  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;

            if *stack_depth_words < slot.depth {
                return Err(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let store_offset_words = *stack_depth_words - slot.depth;
            let store_disp_bytes = (store_offset_words * WORD_SIZE) as i64;
            writeln!(
                writer,
                "  store.w %r0, {}(%sp)  # span {}..{} \"{}\"",
                store_disp_bytes, span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::Call { name, span } => {
            let Some(label) = function_labels.get(name) else {
                return Err(BytecodeError::UnknownFunction {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            writeln!(writer, "  call {label}  # span {}..{} \"{}\"", span.start, span.end, span.literal)
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })
        }
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            // Evaluate right first, then left, to place operands in %r0 (left) and %r1 (right).
            emit_expr(right, writer, env, stack_depth_words, function_labels)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(left, writer, env, stack_depth_words, function_labels)?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);

            let op_instr = match op {
                BinOp::Add => "add.w",
                BinOp::Sub => "sub.w",
                BinOp::Mul => "mul.w",
                BinOp::Div => "divmod.w",
                BinOp::Shl => "shl.w",
                BinOp::Shr => "sar.w",
                BinOp::BitAnd => "and.w",
                BinOp::BitOr => "or.w",
                BinOp::BitXor => "xor.w",
                BinOp::Eq => "cmpeq.w",
                BinOp::Ne => "cmpne.w",
                BinOp::Lt => "cmplt.w",
                BinOp::Gt => "cmplt.w",
                BinOp::Le => "cmplt.w",
                BinOp::Ge => "cmplt.w",
            };
            match op {
                BinOp::Gt => {
                    // a > b -> cmplt.w b,a ; result in %r1 -> move to %r0
                    writeln!(
                        writer,
                        "  {op_instr} %r1, %r0  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    // move result back to r0
                    writeln!(writer, "  push.w %r1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words += 1;
                    writeln!(writer, "  pop.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words = stack_depth_words.saturating_sub(1);
                }
                BinOp::Le => {
                    // a <= b -> not (b < a)
                    writeln!(
                        writer,
                        "  {op_instr} %r1, %r0  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  not.w %r1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  push.w %r1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words += 1;
                    writeln!(writer, "  pop.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words = stack_depth_words.saturating_sub(1);
                }
                BinOp::Ge => {
                    // a >= b -> not (a < b)
                    writeln!(
                        writer,
                        "  {op_instr} %r0, %r1  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  not.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                }
                _ => {
                    writeln!(
                        writer,
                        "  {op_instr} %r0, %r1  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                }
            }
            Ok(())
        }
    }
}

fn emit_block(
    stmts: &[Stmt],
    span: &Span,
    writer: &mut impl Write,
    env: &mut HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    return_label: Option<&str>,
    function_labels: &HashMap<String, String>,
) -> Result<(), BytecodeError> {
    let saved_env = env.clone();
    let depth_before = *stack_depth_words;

    for stmt in stmts {
        emit_stmt(
            stmt,
            writer,
            env,
            stack_depth_words,
            return_label,
            function_labels,
        )?;
    }

    let locals_to_pop = stack_depth_words.saturating_sub(depth_before);
    for _ in 0..locals_to_pop {
        writeln!(writer, "  pop.w %r15").map_err(|e| BytecodeError::Io {
            source: e,
            span: span.clone(),
        })?;
        *stack_depth_words = stack_depth_words.saturating_sub(1);
    }

    *env = saved_env;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_snapshot;

    #[test]
    fn emits_exit_assembly() {
        let mut buffer = Vec::new();
        let span = crate::pagoda::Span {
            start: 0,
            end: 1,
            literal: "7".to_string(),
        };
        let program = crate::pagoda::CheckedProgram {
            span: span.clone(),
            functions: Vec::new(),
            stmts: vec![crate::pagoda::CheckedStmt {
                stmt: crate::pagoda::Stmt::Expr {
                    expr: Expr::IntLiteral {
                        value: 7,
                        span: span.clone(),
                    },
                    span: span.clone(),
                },
                ty: crate::pagoda::semantics::Type::Int,
            }],
        };

        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_exit_assembly", output);
    }

    #[test]
    fn emits_binary_expression() {
        let program = crate::pagoda::parse_source("{2+3}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_binary_expression", output);
    }

    #[test]
    fn emits_unary_minus() {
        let program = crate::pagoda::parse_source("{-5}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_unary_minus", output);
    }

    #[test]
    fn emits_parenthesized_expression() {
        let program = crate::pagoda::parse_source("{-(1+2)*3}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_parenthesized_expression", output);
    }

    #[test]
    fn emits_comparisons() {
        let program = crate::pagoda::parse_source("{1+2<=3*4}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_comparisons", output);
    }

    #[test]
    fn emits_statements() {
        let program = crate::pagoda::parse_source("{1;2+3;4}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_statements", output);
    }

    #[test]
    fn emits_variables() {
        let program = crate::pagoda::parse_source("{let x = 1+2; x*3}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_variables", output);
    }

    #[test]
    fn emits_return() {
        let program = crate::pagoda::parse_source("{1; return 2; 3}").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_return", output);
    }

    #[test]
    fn emits_if_else() {
        let program = crate::pagoda::parse_source("{ if (1) { 2 } else { 3 } }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_if_else", output);
    }

    #[test]
    fn emits_if_elseif_else() {
        let program =
            crate::pagoda::parse_source("{ if (1) { 2 } else if (0) { 3 } else { 4 } }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_if_elseif_else", output);
    }

    #[test]
    fn emits_for_loop() {
        let program =
            crate::pagoda::parse_source("{ for (let i = 0; i < 2; i = i + 1) { 1; }; 2 }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_for_loop", output);
    }

    #[test]
    fn emits_bitwise_ops() {
        let program = crate::pagoda::parse_source("{ let a = 1; a &= 3; ~a | (a ^ 4) }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_bitwise_ops", output);
    }

    #[test]
    fn emits_shifts() {
        let program = crate::pagoda::parse_source("{ let a = 1; a <<= 2; a >> 1 }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_shifts", output);
    }

    #[test]
    fn emits_function_call() {
        let program =
            crate::pagoda::parse_source("fn foo() { return 5; }; { foo() }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call", output);
    }
}
