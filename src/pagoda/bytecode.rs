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
}

impl BytecodeError {
    pub fn span(&self) -> &Span {
        match self {
            BytecodeError::Io { span, .. } => span,
            BytecodeError::UnknownVariable { span, .. } => span,
        }
    }
}

/// Emit assembly that exits with the integer literal contained in `program`.
pub fn emit_exit_program(
    program: &CheckedProgram,
    mut writer: impl Write,
) -> Result<(), BytecodeError> {
    writeln!(writer, "main:").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    let mut env: HashMap<String, VarSlot> = HashMap::new();
    let mut stack_depth_words: usize = 0;
    let mut needs_ret_label = false;

    for checked in &program.stmts {
        needs_ret_label |= matches!(checked.stmt, Stmt::Return { .. });
        emit_stmt(
            &checked.stmt,
            &mut writer,
            &mut env,
            &mut stack_depth_words,
            needs_ret_label,
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

fn fresh_label() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};
    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("label_{id}")
}

fn emit_stmt(
    stmt: &Stmt,
    writer: &mut impl Write,
    env: &mut HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    needs_ret_label: bool,
) -> Result<(), BytecodeError> {
    match stmt {
        Stmt::Expr { expr, .. } => emit_expr(expr, writer, env, stack_depth_words),
        Stmt::Empty { .. } => Ok(()),
        Stmt::Let { name, expr, .. } => {
            emit_expr(expr, writer, env, stack_depth_words)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: expr.span().clone(),
            })?;
            *stack_depth_words += 1;
            env.insert(name.clone(), VarSlot { depth: *stack_depth_words });
            Ok(())
        }
        Stmt::Return { expr, .. } => {
            emit_expr(expr, writer, env, stack_depth_words)?;
            emit_pop_all_locals(writer, stack_depth_words)?;
            if needs_ret_label {
                writeln!(writer, "  jmp ret_exit").map_err(|e| BytecodeError::Io {
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
            emit_expr(cond, writer, env, stack_depth_words)?;
            let else_label = fresh_label();
            let end_label = fresh_label();
            writeln!(writer, "  brz.w %r0, {else_label}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            emit_stmt(then_branch, writer, env, stack_depth_words, needs_ret_label)?;
            if let Some(else_branch) = else_branch {
                writeln!(writer, "  jmp {end_label}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                writeln!(writer, "{else_label}:").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                emit_stmt(else_branch, writer, env, stack_depth_words, needs_ret_label)?;
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
            Ok(())
        }
        Stmt::Block { stmts, span } => emit_block(
            stmts,
            span,
            writer,
            env,
            stack_depth_words,
            needs_ret_label,
        ),
    }
}

fn emit_pop_all_locals(
    writer: &mut impl Write,
    stack_depth_words: &mut usize,
) -> Result<(), BytecodeError> {
    while *stack_depth_words > 0 {
        writeln!(writer, "  pop.w %r15").map_err(|e| BytecodeError::Io {
            source: e,
            span: Span {
                start: 0,
                end: 0,
                literal: String::new(),
            },
        })?;
        *stack_depth_words -= 1;
    }
    Ok(())
}

fn emit_expr(
    expr: &Expr,
    writer: &mut impl Write,
    env: &HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
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
                return Err(BytecodeError::UnknownVariable { name: name.clone(), span: span.clone() });
            };
            if *stack_depth_words < slot.depth {
                return Err(BytecodeError::UnknownVariable { name: name.clone(), span: span.clone() });
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
            emit_expr(expr, writer, env, stack_depth_words)?;
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
            }
        }
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            // Evaluate right first, then left, to place operands in %r0 (left) and %r1 (right).
            emit_expr(right, writer, env, stack_depth_words)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(left, writer, env, stack_depth_words)?;
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
    needs_ret_label: bool,
) -> Result<(), BytecodeError> {
    let saved_env = env.clone();
    let depth_before = *stack_depth_words;

    for stmt in stmts {
        emit_stmt(stmt, writer, env, stack_depth_words, needs_ret_label)?;
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
}
