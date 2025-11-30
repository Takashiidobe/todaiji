use thiserror::Error;

use crate::pagoda::{CheckedExpr, CheckedProgram, Expr, Program, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SemanticError {
    #[error("type mismatch: expected {expected}, found {found}")]
    TypeMismatch {
        expected: Type,
        found: Type,
        span: Span,
    },
    #[error("unsupported expression")]
    UnsupportedExpr { span: Span },
}

impl Type {
    pub fn as_str(&self) -> &'static str {
        match self {
            Type::Int => "int",
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl SemanticError {
    pub fn span(&self) -> &Span {
        match self {
            SemanticError::TypeMismatch { span, .. } => span,
            SemanticError::UnsupportedExpr { span } => span,
        }
    }
}

pub fn analyze_program(program: Program) -> Result<CheckedProgram, SemanticError> {
    let checked_expr = analyze_expr(&program.expr)?;
    Ok(CheckedProgram {
        expr: checked_expr,
        span: program.span,
    })
}

fn analyze_expr(expr: &Expr) -> Result<CheckedExpr, SemanticError> {
    match expr {
        Expr::IntLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::Int,
        }),
        Expr::Binary { op: _, left, right, span: _ } => {
            let left_checked = analyze_expr(left)?;
            let right_checked = analyze_expr(right)?;
            if left_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: left_checked.ty,
                    span: left_checked.expr.span().clone(),
                });
            }
            if right_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: right_checked.ty,
                    span: right_checked.expr.span().clone(),
                });
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Int,
            })
        }
    }
}
