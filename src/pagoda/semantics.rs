use thiserror::Error;

use std::collections::HashMap;

use crate::pagoda::{CheckedExpr, CheckedProgram, CheckedStmt, Expr, Program, Span, Stmt};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    String,
    Array(usize),
    Struct(String), // Struct name
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
    ArityMismatch {
        name: String,
        expected: usize,
        found: usize,
        span: Span,
    },
}

impl Type {
    pub fn as_str(&self) -> String {
        match self {
            Type::Int => "int".to_string(),
            Type::Bool => "bool".to_string(),
            Type::String => "string".to_string(),
            Type::Array(_) => "array".to_string(),
            Type::Struct(name) => format!("struct {}", name),
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.as_str())
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
    let mut structs: HashMap<String, HashMap<String, Type>> = HashMap::new();
    let mut checked_functions = Vec::new();

    // Validate and collect struct definitions
    for struct_def in &program.structs {
        let mut fields = HashMap::new();
        for field in &struct_def.fields {
            let field_ty = match field.ty.as_str() {
                "i64" => Type::Int,
                "string" => Type::String,
                _ => {
                    return Err(SemanticError::UnsupportedExpr {
                        span: field.span.clone(),
                    });
                }
            };
            // Check for duplicate fields
            if fields.contains_key(&field.name) {
                return Err(SemanticError::DuplicateVariable {
                    name: field.name.clone(),
                    span: field.span.clone(),
                });
            }
            fields.insert(field.name.clone(), field_ty);
        }
        // Check for duplicate struct names
        if structs.contains_key(&struct_def.name) {
            return Err(SemanticError::DuplicateFunction {
                name: struct_def.name.clone(),
                span: struct_def.span.clone(),
            });
        }
        structs.insert(struct_def.name.clone(), fields);
    }

    // collect function names
    for func in &program.functions {
        if func.params.len() > 255 {
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
        let checked_body = analyze_stmt(&func.body, &mut fn_scopes, &functions, &structs)?;
        checked_functions.push(crate::pagoda::CheckedFunction {
            name: func.name.clone(),
            params: func.params.clone(),
            body: checked_body,
            span: func.span.clone(),
        });
    }

    let mut checked_stmts = Vec::new();
    for stmt in &program.stmts {
        let checked = analyze_stmt(stmt, &mut scopes, &functions, &structs)?;
        checked_stmts.push(checked);
    }
    Ok(CheckedProgram {
        structs: program.structs.clone(),
        functions: checked_functions,
        stmts: checked_stmts,
        span: program.span,
    })
}

fn analyze_stmt(
    stmt: &Stmt,
    scopes: &mut Vec<HashMap<String, Type>>,
    functions: &HashMap<String, usize>,
    structs: &HashMap<String, HashMap<String, Type>>,
) -> Result<CheckedStmt, SemanticError> {
    match stmt {
        Stmt::Expr { expr, span: _ } => {
            let checked_expr = analyze_expr(expr, scopes, functions, structs)?;
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
            let checked_expr = analyze_expr(expr, scopes, functions, structs)?;
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
            let checked_expr = analyze_expr(expr, scopes, functions, structs)?;
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
                let _ = analyze_stmt(init_stmt, scopes, functions, structs)?;
            }
            if let Some(cond_expr) = cond {
                let cond_checked = analyze_expr(cond_expr, scopes, functions, structs)?;
                if cond_checked.ty != Type::Bool && cond_checked.ty != Type::Int {
                    scopes.pop();
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Bool,
                        found: cond_checked.ty,
                        span: cond_checked.expr.span().clone(),
                    });
                }
            }
            let _body_checked = analyze_stmt(body, scopes, functions, structs)?;
            if let Some(post_expr) = post {
                let _ = analyze_expr(post_expr, scopes, functions, structs)?;
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
            let cond_checked = analyze_expr(cond, scopes, functions, structs)?;
            if cond_checked.ty != Type::Bool && cond_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Bool,
                    found: cond_checked.ty,
                    span: cond_checked.expr.span().clone(),
                });
            }
            let _then_checked = analyze_stmt(then_branch, scopes, functions, structs)?;
            let else_ty = if let Some(else_branch) = else_branch {
                let else_checked = analyze_stmt(else_branch, scopes, functions, structs)?;
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
                let checked = analyze_stmt(inner, scopes, functions, structs)?;
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
    structs: &HashMap<String, HashMap<String, Type>>,
) -> Result<CheckedExpr, SemanticError> {
    match expr {
        Expr::IntLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::Int,
        }),
        Expr::StringLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::String,
        }),
        Expr::ArrayLiteral { elements, .. } => {
            let mut last_ty = Type::Int;
            for el in elements {
                let checked = analyze_expr(el, scopes, functions, structs)?;
                if checked.ty != Type::Int {
                    return Err(SemanticError::TypeMismatch {
                        expected: Type::Int,
                        found: checked.ty,
                        span: checked.expr.span().clone(),
                    });
                }
                last_ty = checked.ty;
            }
            let len = elements.len();
            let _ = last_ty;
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Array(len),
            })
        }
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
            let rhs_checked = analyze_expr(value, scopes, functions, structs)?;
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
            let rhs_checked = analyze_expr(value, scopes, functions, structs)?;
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
                let _ = analyze_expr(arg, scopes, functions, structs)?;
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
            let checked_inner = analyze_expr(inner, scopes, functions, structs)?;
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
            let left_checked = analyze_expr(left, scopes, functions, structs)?;
            let right_checked = analyze_expr(right, scopes, functions, structs)?;
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
                | crate::pagoda::parser::BinOp::Mod
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
        Expr::Index {
            base,
            index,
            span: _,
        } => {
            let base_checked = analyze_expr(base, scopes, functions, structs)?;
            let idx_checked = analyze_expr(index, scopes, functions, structs)?;
            if idx_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: idx_checked.ty,
                    span: idx_checked.expr.span().clone(),
                });
            }
            match base_checked.ty {
                Type::Array(_) => Ok(CheckedExpr {
                    expr: expr.clone(),
                    ty: Type::Int,
                }),
                other => Err(SemanticError::TypeMismatch {
                    expected: Type::Array(0),
                    found: other,
                    span: base_checked.expr.span().clone(),
                }),
            }
        }
        Expr::IndexAssign {
            base,
            index,
            value,
            span: _,
        } => {
            let base_checked = analyze_expr(base, scopes, functions, structs)?;
            let idx_checked = analyze_expr(index, scopes, functions, structs)?;
            if idx_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: idx_checked.ty,
                    span: idx_checked.expr.span().clone(),
                });
            }
            let val_checked = analyze_expr(value, scopes, functions, structs)?;
            if val_checked.ty != Type::Int {
                return Err(SemanticError::TypeMismatch {
                    expected: Type::Int,
                    found: val_checked.ty,
                    span: val_checked.expr.span().clone(),
                });
            }
            match base_checked.ty {
                Type::Array(_) => Ok(CheckedExpr {
                    expr: expr.clone(),
                    ty: Type::Int,
                }),
                other => Err(SemanticError::TypeMismatch {
                    expected: Type::Array(0),
                    found: other,
                    span: base_checked.expr.span().clone(),
                }),
            }
        }
        Expr::StructLiteral {
            struct_name,
            field_values,
            span,
        } => {
            // Check if struct exists
            let Some(struct_fields) = structs.get(struct_name) else {
                return Err(SemanticError::UnknownVariable {
                    name: format!("struct '{}' not defined", struct_name),
                    span: span.clone(),
                });
            };

            // Check all required fields are present and no extras
            let mut provided_fields = std::collections::HashSet::new();
            for (field_name, field_expr) in field_values {
                // Check field exists in struct
                if !struct_fields.contains_key(field_name) {
                    return Err(SemanticError::UnknownVariable {
                        name: format!(
                            "field '{}' not found in struct '{}'",
                            field_name, struct_name
                        ),
                        span: field_expr.span().clone(),
                    });
                }
                // Check for duplicate fields
                if !provided_fields.insert(field_name.clone()) {
                    return Err(SemanticError::DuplicateVariable {
                        name: field_name.clone(),
                        span: field_expr.span().clone(),
                    });
                }
                // Type check field value
                let expected_ty = &struct_fields[field_name];
                let checked = analyze_expr(field_expr, scopes, functions, structs)?;
                if &checked.ty != expected_ty {
                    return Err(SemanticError::TypeMismatch {
                        expected: expected_ty.clone(),
                        found: checked.ty,
                        span: checked.expr.span().clone(),
                    });
                }
            }

            // Check all required fields are provided
            for field_name in struct_fields.keys() {
                if !provided_fields.contains(field_name) {
                    return Err(SemanticError::UnknownVariable {
                        name: format!(
                            "missing field '{}' in struct '{}' literal",
                            field_name, struct_name
                        ),
                        span: span.clone(),
                    });
                }
            }

            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Struct(struct_name.clone()),
            })
        }
        Expr::FieldAccess {
            base,
            field_name,
            span,
        } => {
            let base_checked = analyze_expr(base, scopes, functions, structs)?;
            match &base_checked.ty {
                Type::Struct(struct_name) => {
                    let Some(struct_fields) = structs.get(struct_name) else {
                        return Err(SemanticError::UnknownVariable {
                            name: format!("struct '{}' not defined", struct_name),
                            span: span.clone(),
                        });
                    };
                    let Some(field_ty) = struct_fields.get(field_name) else {
                        return Err(SemanticError::UnknownVariable {
                            name: format!(
                                "field '{}' not found in struct '{}'",
                                field_name, struct_name
                            ),
                            span: span.clone(),
                        });
                    };
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: field_ty.clone(),
                    })
                }
                other => Err(SemanticError::TypeMismatch {
                    expected: Type::Struct("any".to_string()),
                    found: other.clone(),
                    span: base_checked.expr.span().clone(),
                }),
            }
        }
        Expr::FieldAssign {
            base,
            field_name,
            value,
            span,
        } => {
            let base_checked = analyze_expr(base, scopes, functions, structs)?;
            match &base_checked.ty {
                Type::Struct(struct_name) => {
                    let Some(struct_fields) = structs.get(struct_name) else {
                        return Err(SemanticError::UnknownVariable {
                            name: format!("struct '{}' not defined", struct_name),
                            span: span.clone(),
                        });
                    };
                    let Some(field_ty) = struct_fields.get(field_name) else {
                        return Err(SemanticError::UnknownVariable {
                            name: format!(
                                "field '{}' not found in struct '{}'",
                                field_name, struct_name
                            ),
                            span: span.clone(),
                        });
                    };
                    let value_checked = analyze_expr(value, scopes, functions, structs)?;
                    if &value_checked.ty != field_ty {
                        return Err(SemanticError::TypeMismatch {
                            expected: field_ty.clone(),
                            found: value_checked.ty,
                            span: value_checked.expr.span().clone(),
                        });
                    }
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: field_ty.clone(),
                    })
                }
                other => Err(SemanticError::TypeMismatch {
                    expected: Type::Struct("any".to_string()),
                    found: other.clone(),
                    span: base_checked.expr.span().clone(),
                }),
            }
        }
    }
}
