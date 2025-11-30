use std::io::Write;

use thiserror::Error;

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
    let (exit_code, span) = match &program.expr.expr {
        Expr::IntLiteral { value, span } => (*value, span),
    };

    writeln!(writer, "main:").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  movi %r0, $60").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(
        writer,
        "  load.l %r1, ${exit_code}  # span {}..{} \"{}\"",
        span.start, span.end, span.literal
    )
    .map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    writeln!(writer, "  trap").map_err(|e| BytecodeError::Io {
        source: e,
        span: program.span.clone(),
    })?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

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

        let expected =
            "main:\n  movi %r0, $60\n  load.l %r1, $7  # span 0..1 \"7\"\n  trap\n";
        assert_eq!(output, expected);
    }
}
