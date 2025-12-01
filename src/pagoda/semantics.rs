use thiserror::Error;

use std::collections::HashMap;

use crate::pagoda::{CheckedExpr, CheckedProgram, CheckedStmt, Expr, Program, Span, Stmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
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
    #[error("unknown variable '{name}'")]
    UnknownVariable { name: String, span: Span },
    #[error("variable '{name}' already defined")]
    DuplicateVariable { name: String, span: Span },
    #[error("unknown function '{name}'")]
    UnknownFunction { name: String, span: Span },
    #[error("function '{name}' already defined")]
    DuplicateFunction { name: String, span: Span },
    #[error("function '{name}' expects {expected} args, found {found}")]
    ArityMismatch { name: String, expected: usize, found: usize, span: Span },
}

impl Type {
    pub fn as_str(&self) -> &'static str {
        match self {
            Type::Int => "int",
            Type::Bool => "bool",
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
            SemanticError::UnknownVariable { span, .. } => span,
            SemanticError::DuplicateVariable { span, .. } => span,
            SemanticError::UnknownFunction { span, .. } => span,
            SemanticError::DuplicateFunction { span, .. } => span,
            SemanticError::ArityMismatch { span, .. } => span,
        }
    }
}

pub fn analyze_program(program: Program) -> Result<CheckedProgram, SemanticError> {
    let mut scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
    let mut functions: HashMap<String, usize> = HashMap::new();
    let mut checked_functions = Vec::new();

    // collect function names
    for func in &program.functions {
        if func.params.len() > 8 {
            return Err(SemanticError::UnsupportedExpr {
                span: func.span.clone(),
            });
        }
        if functions.contains_key(&func.name) {
            return Err(SemanticError::DuplicateFunction {
                name: func.name.clone(),
                span: func.span.clone(),
            });
        }
        functions.insert(func.name.clone(), func.params.len());
    }

    for func in &program.functions {
        let mut fn_scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
        for pname in &func.params {
            fn_scopes
                .last_mut()
                .unwrap()
                .insert(pname.clone(), Type::Int);
        }
        let checked_body = analyze_stmt(&func.body, &mut fn_scopes, &functions)?;
        checked_functions.push(crate::pagoda::CheckedFunction {
            name: func.name.clone(),
            params: func.params.clone(),
            body: checked_body,
            span: func.span.clone(),
        });
    }

    let mut checked_stmts = Vec::new();
    for stmt in &program.stmts {
        let checked = analyze_stmt(stmt, &mut scopes, &functions)?;
        checked_stmts.push(checked);
    }
    Ok(CheckedProgram {
        functions: checked_functions,
        stmts: checked_stmts,
        span: program.span,
    })
}

fn analyze_stmt(
    stmt: &Stmt,
    scopes: &mut Vec<HashMap<String, Type>>,
    functions: &HashMap<String, usize>,
) -> Result<CheckedStmt, SemanticError> {
    match stmt {
        Stmt::Expr { expr, span: _ } => {
            let checked_expr = analyze_expr(expr, scopes, functions)?;
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: checked_expr.ty,
            })
        }
        Stmt::Empty { .. } => Ok(CheckedStmt {
            stmt: stmt.clone(),
            ty: Type::Int,
        }),
        Stmt::Let { name, expr, span } => {
            if scopes.last().unwrap().contains_key(name) {
                return Err(SemanticError::DuplicateVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let checked_expr = analyze_expr(expr, scopes, functions)?;
            scopes
                .last_mut()
                .unwrap()
                .insert(name.clone(), checked_expr.ty.clone());
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: checked_expr.ty,
            })
        }
        Stmt::Return { expr, span: _ } => {
            let checked_expr = analyze_expr(expr, scopes, functions)?;
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: checked_expr.ty,
            })
        }
        Stmt::For {
            init,
            cond,
            post,
            body,
            ..
        } => {
            scopes.push(HashMap::new());
            if let Some(init_stmt) = init {
                let _ = analyze_stmt(init_stmt, scopes, functions)?;
            }
            if let Some(cond_expr) = cond {
                let cond_checked = analyze_expr(cond_expr, scopes, functions)?;
                if cond_checked.ty != Type::Bool && cond_checked.ty != Type::Int {
                    scopes.pop();
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_checked.ty,
                        span: cond_checked.expr.span().clone(),
                    });
                }
            }
            let _body_checked = analyze_stmt(body, scopes, functions)?;
            if let Some(post_expr) = post {
                let _ = analyze_expr(post_expr, scopes, functions)?;
            }
            scopes.pop();
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: Type::Int,
            })
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            let cond_checked = analyze_expr(cond, scopes, functions)?;
            if cond_checked.ty != Type::Bool && cond_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Bool,
                    found: cond_checked.ty,
                    span: cond_checked.expr.span().clone(),
                });
            }
            let _then_checked = analyze_stmt(then_branch, scopes, functions)?;
            let else_ty = if let Some(else_branch) = else_branch {
                let else_checked = analyze_stmt(else_branch, scopes, functions)?;
                else_checked.ty
            } else {
                Type::Int
            };
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: else_ty,
            })
        }
        Stmt::Block { stmts, span: _ } => {
            scopes.push(HashMap::new());
            let mut last_ty = Type::Int;
            let mut last_checked: Option<CheckedStmt> = None;
            for inner in stmts {
                let checked = analyze_stmt(inner, scopes, functions)?;
                if !matches!(inner, Stmt::Empty { .. }) {
                    last_ty = checked.ty.clone();
                    last_checked = Some(checked);
                }
            }
            scopes.pop();
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: last_ty,
            })
        }
    }
}

fn analyze_expr(
    expr: &Expr,
    scopes: &Vec<HashMap<String, Type>>,
    functions: &HashMap<String, usize>,
) -> Result<CheckedExpr, SemanticError> {
    match expr {
        Expr::IntLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::Int,
        }),
        Expr::Assign { name, value, span } => {
            let mut found = None;
            for scope in scopes.iter().rev() {
                if let Some(ty) = scope.get(name) {
                    found = Some(ty.clone());
                    break;
                }
            }
            let Some(existing_ty) = found else {
                return Err(SemanticError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            let rhs_checked = analyze_expr(value, scopes, functions)?;
            if rhs_checked.ty != existing_ty {
                return Err(SemanticError::TypeMismatch {
                    expected: existing_ty,
                    found: rhs_checked.ty,
                    span: rhs_checked.expr.span().clone(),
                });
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: existing_ty,
            })
        }
        Expr::CompoundAssign {
            name,
            value,
            span,
            op: _,
        } => {
            let mut found = None;
            for scope in scopes.iter().rev() {
                if let Some(ty) = scope.get(name) {
                    found = Some(ty.clone());
                    break;
                }
            }
            let Some(existing_ty) = found else {
                return Err(SemanticError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            let rhs_checked = analyze_expr(value, scopes, functions)?;
            if rhs_checked.ty != existing_ty {
                return Err(SemanticError::TypeMismatch {
                    expected: existing_ty.clone(),
                    found: rhs_checked.ty,
                    span: rhs_checked.expr.span().clone(),
                });
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: existing_ty,
            })
        }
        Expr::Call { name, args, span } => {
            let Some(expected_arity) = functions.get(name) else {
                return Err(SemanticError::UnknownFunction {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            if *expected_arity != args.len() {
                return Err(SemanticError::ArityMismatch {
                    name: name.clone(),
                    expected: *expected_arity,
                    found: args.len(),
                    span: span.clone(),
                });
            }
            for arg in args {
                let _ = analyze_expr(arg, scopes, functions)?;
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Int,
            })
        }
        Expr::Var { name, span } => {
            let mut found = None;
            for scope in scopes.iter().rev() {
                if let Some(ty) = scope.get(name) {
                    found = Some(ty.clone());
                    break;
                }
            }
            let Some(ty) = found else {
                return Err(SemanticError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: ty.clone(),
            })
        }
        Expr::Unary { expr: inner, .. } => {
            let checked_inner = analyze_expr(inner, scopes, functions)?;
            if checked_inner.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: checked_inner.ty,
                    span: checked_inner.expr.span().clone(),
                });
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Int,
            })
        }
        Expr::Binary {
            op,
            left,
            right,
            span: _,
        } => {
            let left_checked = analyze_expr(left, scopes, functions)?;
            let right_checked = analyze_expr(right, scopes, functions)?;
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
            let result_ty = match op {
                crate::pagoda::parser::BinOp::Add
                | crate::pagoda::parser::BinOp::Sub
                | crate::pagoda::parser::BinOp::Mul
                | crate::pagoda::parser::BinOp::Div
                | crate::pagoda::parser::BinOp::Shl
                | crate::pagoda::parser::BinOp::Shr
                | crate::pagoda::parser::BinOp::BitAnd
                | crate::pagoda::parser::BinOp::BitOr
                | crate::pagoda::parser::BinOp::BitXor => Type::Int,
                crate::pagoda::parser::BinOp::Eq
                | crate::pagoda::parser::BinOp::Ne
                | crate::pagoda::parser::BinOp::Lt
                | crate::pagoda::parser::BinOp::Gt
                | crate::pagoda::parser::BinOp::Le
                | crate::pagoda::parser::BinOp::Ge => Type::Bool,
            };
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: result_ty,
            })
        }
    }
}
