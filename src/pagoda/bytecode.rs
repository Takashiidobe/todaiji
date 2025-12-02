use std::io::Write;

use std::collections::HashMap;

use thiserror::Error;

use crate::pagoda::parser::BinOp;
use crate::pagoda::{CheckedProgram, Expr, Span, Stmt};

const WORD_SIZE: usize = 8;
const HEAP_REG: &str = "%r12";
const SCRATCH_REG: &str = "%r7"; // caller-saved and not aliased to SP/PC/FP

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

pub fn emit_exit_program(
    program: &CheckedProgram,
    mut writer: impl Write,
) -> Result<(), BytecodeError> {
    let mut labels = LabelGen::default();
    let mut function_labels: HashMap<String, (String, usize)> = HashMap::new();
    let mut struct_fields: HashMap<String, Vec<String>> = HashMap::new();
    for s in &program.structs {
        struct_fields.insert(
            s.name.clone(),
            s.fields.iter().map(|f| f.name.clone()).collect(),
        );
    }
    for func in &program.functions {
        function_labels.insert(
            func.name.clone(),
            (format!("fn_{}", func.name), func.params.len()),
        );
    }

    // runtime init: heap pointer from brk(0)
    writeln!(writer, "  movi %r0, $12").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  xor.w %r1, %r1").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  mov {HEAP_REG}, %r0").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    // Ensure execution starts at main by jumping over function bodies.
    writeln!(writer, "  jmp main").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    for func in &program.functions {
        emit_function(
            func,
            &mut writer,
            &function_labels,
            &struct_fields,
            &mut labels,
        )?;
    }

    writeln!(writer, "main:").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;

    // Check if there's a main function
    if let Some(main_func) = program.functions.iter().find(|f| f.name == "main") {
        // Validate main has no parameters
        if !main_func.params.is_empty() {
            return Err(BytecodeError::UnknownFunction {
                name: format!(
                    "main function must have zero parameters, found {}",
                    main_func.params.len()
                ),
                span: main_func.span.clone(),
            });
        }

        // Call main
        writeln!(writer, "  call fn_main").map_err(|e| BytecodeError::Io {
            source: e,
            span: program.span.clone(),
        })?;
    } else {
        // No main function, execute top-level statements (backwards compatible)
        let mut env: HashMap<String, VarSlot> = HashMap::new();
        let mut stack_depth_words: usize = 0;
        let needs_ret_label = program.stmts.iter().any(|s| stmt_contains_return(&s.stmt));

        for checked in &program.stmts {
            emit_stmt(
                &checked.stmt,
                &mut writer,
                &mut env,
                &mut stack_depth_words,
                if needs_ret_label {
                    Some("ret_exit")
                } else {
                    None
                },
                &function_labels,
                &struct_fields,
                &mut labels,
            )?;
        }
        if needs_ret_label {
            writeln!(writer, "ret_exit:").map_err(|e| BytecodeError::Io {
                source: e,
                span: program.span.clone(),
            })?;
        }
    }

    writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  movi %r0, $60").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    Ok(())
}

#[derive(Debug, Clone)]
struct VarSlot {
    depth: usize, // number of pushes so far (stack_depth_words) when defined
    struct_name: Option<String>,
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

#[derive(Default)]
struct LabelGen {
    counter: usize,
}

impl LabelGen {
    fn fresh(&mut self) -> String {
        let id = self.counter;
        self.counter += 1;
        format!("label_{id}")
    }
}

fn emit_function(
    func: &crate::pagoda::CheckedFunction,
    writer: &mut impl Write,
    function_labels: &HashMap<String, (String, usize)>,
    struct_fields: &HashMap<String, Vec<String>>,
    labels: &mut LabelGen,
) -> Result<(), BytecodeError> {
    let label = function_labels
        .get(&func.name)
        .map(|(l, _)| l.clone())
        .unwrap_or_else(|| format!("fn_{}", func.name));
    let ret_label = format!("{label}_ret");
    writeln!(writer, "{label}:").map_err(|e| BytecodeError::Io {
        source: e,
        span: func.span.clone(),
    })?;
    let mut env: HashMap<String, VarSlot> = HashMap::new();
    let mut stack_depth_words: usize = 0;
    let extra_stack_args = func.params.len().saturating_sub(8);
    if extra_stack_args > 0 {
        // Stack args arrive left-to-right; the last extra arg is at 0(%sp).
        // Load them in reverse so each subsequent load can use 0(%sp) after the prior push.
        for pname in func.params.iter().skip(8).rev() {
            writeln!(writer, "  load.w %r0, 0(%sp)  # load stack arg {}", pname).map_err(|e| {
                BytecodeError::Io {
                    source: e,
                    span: func.span.clone(),
                }
            })?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: func.span.clone(),
            })?;
            stack_depth_words += 1;
            env.insert(
                pname.clone(),
                VarSlot {
                    depth: stack_depth_words,
                    struct_name: None,
                },
            );
        }
    }
    // Bind register parameters by saving them to the stack in order.
    for (idx, pname) in func.params.iter().take(8).enumerate() {
        let reg = idx + 1; // r1..r8
        writeln!(writer, "  push.w %r{reg}").map_err(|e| BytecodeError::Io {
            source: e,
            span: func.span.clone(),
        })?;
        stack_depth_words += 1;
        env.insert(
            pname.clone(),
            VarSlot {
                depth: stack_depth_words,
                struct_name: None,
            },
        );
    }
    emit_stmt(
        &func.body.stmt,
        writer,
        &mut env,
        &mut stack_depth_words,
        Some(ret_label.as_str()),
        function_labels,
        struct_fields,
        labels,
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
    function_labels: &HashMap<String, (String, usize)>,
    struct_fields: &HashMap<String, Vec<String>>,
    labels: &mut LabelGen,
) -> Result<(), BytecodeError> {
    match stmt {
        Stmt::Expr { expr, .. } => emit_expr(
            expr,
            writer,
            env,
            stack_depth_words,
            function_labels,
            struct_fields,
            labels,
        ),
        Stmt::Empty { .. } => Ok(()),
        Stmt::Let { name, expr, .. } => {
            emit_expr(
                expr,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: expr.span().clone(),
            })?;
            *stack_depth_words += 1;
            let struct_name = expr_struct_type(expr, env);
            env.insert(
                name.clone(),
                VarSlot {
                    depth: *stack_depth_words,
                    struct_name,
                },
            );
            Ok(())
        }
        Stmt::Return { expr, .. } => {
            emit_expr(
                expr,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
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
            emit_expr(
                cond,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            let else_label = labels.fresh();
            let end_label = labels.fresh();
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
                struct_fields,
                labels,
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
                    struct_fields,
                    labels,
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
                    struct_fields,
                    labels,
                )?;
            }

            let loop_label = labels.fresh();
            let end_label = labels.fresh();
            writeln!(writer, "{loop_label}:").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;

            let cond_depth = *stack_depth_words;
            if let Some(cond_expr) = cond {
                emit_expr(
                    cond_expr,
                    writer,
                    env,
                    stack_depth_words,
                    function_labels,
                    struct_fields,
                    labels,
                )?;
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
                struct_fields,
                labels,
            )?;
            *stack_depth_words = cond_depth;

            if let Some(post_expr) = post {
                emit_expr(
                    post_expr,
                    writer,
                    env,
                    stack_depth_words,
                    function_labels,
                    struct_fields,
                    labels,
                )?;
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
                writeln!(writer, "  pop.w {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = stack_depth_words.saturating_sub(1);
            }

            *env = saved_env;
            Ok(())
        }
        Stmt::Block { stmts, span } => emit_block(
            stmts,
            span,
            writer,
            env,
            stack_depth_words,
            return_label,
            function_labels,
            struct_fields,
            labels,
        ),
    }
}

fn emit_pop_all_locals(writer: &mut impl Write, mut depth: usize) -> Result<(), BytecodeError> {
    while depth > 0 {
        writeln!(writer, "  pop.w {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
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

fn struct_field_offset(
    struct_fields: &HashMap<String, Vec<String>>,
    struct_name: &str,
    field_name: &str,
) -> Option<i64> {
    struct_fields.get(struct_name).and_then(|fields| {
        fields
            .iter()
            .position(|f| f == field_name)
            .map(|idx| (idx * WORD_SIZE) as i64)
    })
}

fn expr_struct_type(expr: &Expr, env: &HashMap<String, VarSlot>) -> Option<String> {
    match expr {
        Expr::StructLiteral { struct_name, .. } => Some(struct_name.clone()),
        Expr::Var { name, .. } => env.get(name).and_then(|v| v.struct_name.clone()),
        Expr::Assign { name, .. } => env.get(name).and_then(|v| v.struct_name.clone()),
        Expr::CompoundAssign { name, .. } => env.get(name).and_then(|v| v.struct_name.clone()),
        Expr::FieldAccess { base, .. } | Expr::FieldAssign { base, .. } => {
            expr_struct_type(base, env)
        }
        _ => None,
    }
}

fn emit_expr(
    expr: &Expr,
    writer: &mut impl Write,
    env: &HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    function_labels: &HashMap<String, (String, usize)>,
    struct_fields: &HashMap<String, Vec<String>>,
    labels: &mut LabelGen,
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
        Expr::ArrayLiteral { elements, span } => {
            let bytes = (elements.len() * WORD_SIZE) as i64;
            // r1 = base (old heap), r2 = new_brk
            writeln!(
                writer,
                "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r2, {HEAP_REG}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  load.w %r0, ${bytes}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  add.w %r2, %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r0, $12").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r1, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w {HEAP_REG}, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            for (i, el) in elements.iter().enumerate() {
                emit_expr(
                    el,
                    writer,
                    env,
                    stack_depth_words,
                    function_labels,
                    struct_fields,
                    labels,
                )?;
                let disp = (i * WORD_SIZE) as i64;
                writeln!(
                    writer,
                    "  store.w %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                    span.start, span.end, span.literal
                )
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
            }
            writeln!(writer, "  mov.w %r0, %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::StringLiteral { value, span } => {
            let bytes = value.len() as i64;
            writeln!(
                writer,
                "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r2, {HEAP_REG}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  load.w %r0, ${bytes}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  add.w %r2, %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  movi %r0, $12").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r1, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w {HEAP_REG}, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            for (i, ch) in value.bytes().enumerate() {
                writeln!(writer, "  load.w %r0, ${ch}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                let disp = i as i64;
                writeln!(
                    writer,
                    "  store.b %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                    span.start, span.end, span.literal
                )
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
            }
            writeln!(writer, "  mov.w %r0, %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::StructLiteral {
            struct_name,
            field_values,
            span,
        } => {
            let Some(fields) = struct_fields.get(struct_name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: format!("struct {struct_name}"),
                    span: span.clone(),
                });
            };
            let bytes = (fields.len() * WORD_SIZE) as i64;
            writeln!(
                writer,
                "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r2, {HEAP_REG}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  load.w %r0, ${bytes}").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  add.w %r2, %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  movi %r0, $12").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w %r1, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  mov.w {HEAP_REG}, %r2").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            for (field_name, field_expr) in field_values {
                let Some(disp) = struct_field_offset(struct_fields, struct_name, field_name) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: format!("{struct_name}.{field_name}"),
                        span: field_expr.span().clone(),
                    });
                };
                writeln!(writer, "  push.w %r1").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words += 1;
                emit_expr(
                    field_expr,
                    writer,
                    env,
                    stack_depth_words,
                    function_labels,
                    struct_fields,
                    labels,
                )?;
                writeln!(writer, "  pop.w {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = stack_depth_words.saturating_sub(1);
                writeln!(writer, "  mov.w %r1, {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                writeln!(
                    writer,
                    "  store.w %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                    span.start, span.end, span.literal
                )
                .map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
            }
            writeln!(writer, "  mov.w %r0, %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::Unary { op, expr, span } => {
            emit_expr(
                expr,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
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
                crate::pagoda::parser::UnaryOp::LogicalNot => {
                    // !expr -> brz %r0, true_label; load 0; jmp end; true_label: load 1; end:
                    let true_label = labels.fresh();
                    let end_label = labels.fresh();
                    writeln!(
                        writer,
                        "  brz.w %r0, {true_label}  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  load.w %r0, $0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  jmp {end_label}").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{true_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  load.w %r0, $1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{end_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })
                }
            }
        }
        Expr::Assign { name, value, span } => {
            emit_expr(
                value,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
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
        Expr::FieldAccess {
            base,
            field_name,
            span,
        } => {
            emit_expr(
                base,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            let Some(struct_name) = expr_struct_type(base, env) else {
                return Err(BytecodeError::UnknownVariable {
                    name: field_name.to_string(),
                    span: span.clone(),
                });
            };
            let Some(disp) = struct_field_offset(struct_fields, &struct_name, field_name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: format!("{struct_name}.{field_name}"),
                    span: span.clone(),
                });
            };
            writeln!(
                writer,
                "  load.w %r0, {disp}(%r0)  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::Index { base, index, span } => {
            emit_expr(
                index,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(
                base,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);
            writeln!(writer, "  muli.w %r1, $8").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  add.w %r0, %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(
                writer,
                "  load.w %r0, 0(%r0)  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::IndexAssign {
            base,
            index,
            value,
            span,
        } => {
            emit_expr(
                value,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(
                index,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(
                base,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);
            writeln!(writer, "  muli.w %r1, $8").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  add.w %r0, %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);
            writeln!(
                writer,
                "  store.w %r1, 0(%r0)  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
        }
        Expr::FieldAssign {
            base,
            field_name,
            value,
            span,
        } => {
            emit_expr(
                value,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words += 1;
            emit_expr(
                base,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
            let Some(struct_name) = expr_struct_type(base, env) else {
                return Err(BytecodeError::UnknownVariable {
                    name: field_name.clone(),
                    span: span.clone(),
                });
            };
            let Some(disp) = struct_field_offset(struct_fields, &struct_name, field_name) else {
                return Err(BytecodeError::UnknownVariable {
                    name: format!("{struct_name}.{field_name}"),
                    span: span.clone(),
                });
            };
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            *stack_depth_words = stack_depth_words.saturating_sub(1);
            writeln!(
                writer,
                "  store.w %r1, {disp}(%r0)  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
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
            emit_expr(
                value,
                writer,
                env,
                stack_depth_words,
                function_labels,
                struct_fields,
                labels,
            )?;
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

            match op {
                BinOp::Mod => {
                    writeln!(
                        writer,
                        "  divmod.w %r0, %r1  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  mov %r0, %r1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                }
                _ => {
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
                }
            }

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
        Expr::Call { name, args, span } => {
            let Some((label, arity)) = function_labels.get(name) else {
                return Err(BytecodeError::UnknownFunction {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            let extra_stack_args = args.len().saturating_sub(8);
            // Evaluate arguments left-to-right. First eight go into registers, the rest stay on the stack.
            for (idx, arg) in args.iter().enumerate() {
                emit_expr(
                    arg,
                    writer,
                    env,
                    stack_depth_words,
                    function_labels,
                    struct_fields,
                    labels,
                )?;
                if idx < 8 {
                    writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words += 1;
                    let reg = idx + 1;
                    writeln!(writer, "  pop.w %r{reg}").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words = stack_depth_words.saturating_sub(1);
                } else {
                    writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words += 1;
                }
            }
            if args.len() != *arity {
                return Err(BytecodeError::UnknownFunction {
                    name: format!("{} (arity mismatch)", name),
                    span: span.clone(),
                });
            }
            writeln!(
                writer,
                "  call {label}  # span {}..{} \"{}\"",
                span.start, span.end, span.literal
            )
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            for _ in 0..extra_stack_args {
                writeln!(writer, "  pop.w {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
                    source: e,
                    span: span.clone(),
                })?;
                *stack_depth_words = stack_depth_words.saturating_sub(1);
            }
            Ok(())
        }
        Expr::Binary {
            op,
            left,
            right,
            span,
        } => {
            match op {
                BinOp::LogicalAnd => {
                    // left && right: evaluate left; if false (0), skip right and result is 0; else evaluate right
                    let end_label = labels.fresh();
                    emit_expr(
                        left,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
                    writeln!(
                        writer,
                        "  brz.w %r0, {end_label}  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    emit_expr(
                        right,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
                    // Convert to boolean: if %r0 != 0, result is 1; else 0
                    let _true_label = labels.fresh();
                    let after_label = labels.fresh();
                    writeln!(writer, "  brz.w %r0, {after_label}").map_err(|e| {
                        BytecodeError::Io {
                            source: e,
                            span: span.clone(),
                        }
                    })?;
                    writeln!(writer, "  load.w %r0, $1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{after_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{end_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })
                }
                BinOp::LogicalOr => {
                    // left || right: evaluate left; if true (!= 0), skip right and result is 1; else evaluate right
                    let _true_label = labels.fresh();
                    let eval_right_label = labels.fresh();
                    let end_label = labels.fresh();
                    emit_expr(
                        left,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
                    writeln!(
                        writer,
                        "  brz.w %r0, {eval_right_label}  # span {}..{} \"{}\"",
                        span.start, span.end, span.literal
                    )
                    .map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    // Left was true, result is 1
                    writeln!(writer, "  load.w %r0, $1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "  jmp {end_label}").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{eval_right_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    emit_expr(
                        right,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
                    // Convert to boolean: if %r0 != 0, result is 1; else 0
                    let after_label = labels.fresh();
                    writeln!(writer, "  brz.w %r0, {after_label}").map_err(|e| {
                        BytecodeError::Io {
                            source: e,
                            span: span.clone(),
                        }
                    })?;
                    writeln!(writer, "  load.w %r0, $1").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{after_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    writeln!(writer, "{end_label}:").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })
                }
                _ => {
                    // For all other binary operators, evaluate right first, then left
                    emit_expr(
                        right,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
                    writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
                    *stack_depth_words += 1;
                    emit_expr(
                        left,
                        writer,
                        env,
                        stack_depth_words,
                        function_labels,
                        struct_fields,
                        labels,
                    )?;
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
                        BinOp::Mod => "divmod.w",
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
                        _ => unreachable!(),
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
                        BinOp::Mod => {
                            // divmod leaves the remainder in %r1; move it into %r0 for the result.
                            writeln!(
                                writer,
                                "  {op_instr} %r0, %r1  # span {}..{} \"{}\"",
                                span.start, span.end, span.literal
                            )
                            .map_err(|e| BytecodeError::Io {
                                source: e,
                                span: span.clone(),
                            })?;
                            writeln!(writer, "  mov %r0, %r1").map_err(|e| BytecodeError::Io {
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
    }
}

fn emit_block(
    stmts: &[Stmt],
    span: &Span,
    writer: &mut impl Write,
    env: &mut HashMap<String, VarSlot>,
    stack_depth_words: &mut usize,
    return_label: Option<&str>,
    function_labels: &HashMap<String, (String, usize)>,
    struct_fields: &HashMap<String, Vec<String>>,
    labels: &mut LabelGen,
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
            struct_fields,
            labels,
        )?;
    }

    let locals_to_pop = stack_depth_words.saturating_sub(depth_before);
    for _ in 0..locals_to_pop {
        writeln!(writer, "  pop.w {SCRATCH_REG}").map_err(|e| BytecodeError::Io {
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
            structs: Vec::new(),
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
    fn emits_string_literal() {
        let program = crate::pagoda::parse_source("{ \"hi\" }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_string_literal", output);
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
    fn emits_modulo() {
        let program = crate::pagoda::parse_source("{ let a = 7; a %= 4; a % 3 }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_modulo", output);
    }

    #[test]
    fn emits_function_call() {
        let program = crate::pagoda::parse_source("fn foo() { return 5; }; { foo() }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call", output);
    }

    #[test]
    fn emits_function_call_with_args() {
        let program = crate::pagoda::parse_source("fn add(a,b) { a + b }; { add(2,3) }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call_with_args", output);
    }

    #[test]
    fn emits_function_call_with_stack_args() {
        let program = crate::pagoda::parse_source(
            "fn sum(a,b,c,d,e,f,g,h,i,j) { a+b+c+d+e+f+g+h+i+j }; { sum(1,2,3,4,5,6,7,8,9,10) }",
        )
        .unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call_with_stack_args", output);
    }
}
