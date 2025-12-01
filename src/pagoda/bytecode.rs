use std::io::Write;

use thiserror::Error;

use crate::pagoda::parser::BinOp;
use crate::pagoda::{CheckedProgram, Expr, Span};

#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("I/O while emitting assembly: {source}")]
    Io { source: std::io::Error, span: Span },
}

impl BytecodeError {
    pub fn span(&self) -> &Span {
        match self {
            BytecodeError::Io { span, .. } => span,
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

    emit_expr(&program.expr.expr, &mut writer)?;
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

fn emit_expr(expr: &Expr, writer: &mut impl Write) -> Result<(), BytecodeError> {
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
        Expr::Unary { op, expr, span } => {
            emit_expr(expr, writer)?;
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
            emit_expr(right, writer)?;
            writeln!(writer, "  push.w %r0").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;
            emit_expr(left, writer)?;
            writeln!(writer, "  pop.w %r1").map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })?;

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
                    writeln!(writer, "  pop.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
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
                    writeln!(writer, "  pop.w %r0").map_err(|e| BytecodeError::Io {
                        source: e,
                        span: span.clone(),
                    })?;
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
            expr: crate::pagoda::CheckedExpr {
                expr: Expr::IntLiteral {
                    value: 7,
                    span: span.clone(),
                },
                ty: crate::pagoda::semantics::Type::Int,
            },
        };

        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_exit_assembly", output);
    }

    #[test]
    fn emits_binary_expression() {
        let program = crate::pagoda::parse_source("2+3").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_binary_expression", output);
    }

    #[test]
    fn emits_unary_minus() {
        let program = crate::pagoda::parse_source("-5").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_unary_minus", output);
    }

    #[test]
    fn emits_parenthesized_expression() {
        let program = crate::pagoda::parse_source("-(1+2)*3").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_parenthesized_expression", output);
    }

    #[test]
    fn emits_comparisons() {
        let program = crate::pagoda::parse_source("1+2<=3*4").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_comparisons", output);
    }
}
