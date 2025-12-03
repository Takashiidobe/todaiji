use thiserror::Error;

use std::collections::HashMap;
use std::path::PathBuf;

use crate::pagoda::{CheckedExpr, CheckedProgram, CheckedStmt, Expr, Program, Span, Stmt};

#[derive(Debug, Clone)]
pub struct Module {
    pub name: String,
    pub file_path: PathBuf,
    pub program: Program,
    pub checked_program: CheckedProgram,
    pub public_functions: HashMap<String, FunctionSignature>,
    pub public_structs: HashMap<String, StructSignature>,
}

#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub param_types: Vec<Type>,
    pub param_count: usize,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct StructSignature {
    pub fields: HashMap<String, Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,    // i64
    Int32,  // i32
    Int16,  // i16
    Int8,   // i8
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
    #[error("module '{name}' not found")]
    ModuleNotFound { name: String, span: Span },
    #[error("circular import detected: {cycle}")]
    CircularImport { cycle: String, span: Span },
    #[error("function '{name}' is private in module '{module}'")]
    PrivateFunction {
        module: String,
        name: String,
        span: Span,
    },
    #[error("struct '{name}' is private in module '{module}'")]
    PrivateStruct {
        module: String,
        name: String,
        span: Span,
    },
    #[error("module '{module}' does not export function '{name}'")]
    UnknownModuleFunction {
        module: String,
        name: String,
        span: Span,
    },
    #[error("module '{module}' does not export struct '{name}'")]
    UnknownModuleStruct {
        module: String,
        name: String,
        span: Span,
    },
}

impl Type {
    pub fn as_str(&self) -> String {
        match self {
            Type::Int => "i64".to_string(),
            Type::Int32 => "i32".to_string(),
            Type::Int16 => "i16".to_string(),
            Type::Int8 => "i8".to_string(),
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

fn parse_type_name(name: &str) -> Type {
    match name {
        "int" => Type::Int,
        "i64" => Type::Int,
        "i32" => Type::Int32,
        "i16" => Type::Int16,
        "i8" => Type::Int8,
        "string" => Type::String,
        "bool" => Type::Bool,
        other => Type::Struct(other.to_string()),
    }
}

fn is_int_like(ty: &Type) -> bool {
    matches!(ty, Type::Int | Type::Int32 | Type::Int16 | Type::Int8)
}

fn types_compatible(expected: &Type, found: &Type) -> bool {
    expected == found || (is_int_like(expected) && is_int_like(found))
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
            SemanticError::ModuleNotFound { span, .. } => span,
            SemanticError::CircularImport { span, .. } => span,
            SemanticError::PrivateFunction { span, .. } => span,
            SemanticError::PrivateStruct { span, .. } => span,
            SemanticError::UnknownModuleFunction { span, .. } => span,
            SemanticError::UnknownModuleStruct { span, .. } => span,
        }
    }
}

pub fn analyze_program(program: Program) -> Result<CheckedProgram, SemanticError> {
    let mut scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
    let mut functions: HashMap<String, FunctionSignature> = HashMap::new();
    let mut structs: HashMap<String, HashMap<String, Type>> = HashMap::new();
    let mut checked_functions = Vec::new();

    // Validate and collect struct definitions
    for struct_def in &program.structs {
        let mut fields = HashMap::new();
        for field in &struct_def.fields {
            let field_ty = parse_type_name(&field.ty);
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
        let param_types: Vec<Type> = func
            .params
            .iter()
            .map(|p| {
                p.ty.as_ref()
                    .map(|t| parse_type_name(t))
                    .unwrap_or(Type::Int)
            })
            .collect();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| parse_type_name(t))
            .unwrap_or(Type::Int);
        functions.insert(
            func.name.clone(),
            FunctionSignature {
                param_types,
                param_count: func.params.len(),
                return_type,
            },
        );
    }

    for func in &program.functions {
        let mut fn_scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
        for pname in &func.params {
            // Parse parameter type, default to Int if not specified (type inference)
            let param_type = pname
                .ty
                .as_ref()
                .map(|t| parse_type_name(t))
                .unwrap_or(Type::Int);
            fn_scopes
                .last_mut()
                .unwrap()
                .insert(pname.name.clone(), param_type);
        }
        let checked_body = analyze_stmt(&func.body, &mut fn_scopes, &functions, &structs)?;
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| parse_type_name(t))
            .unwrap_or(Type::Int);
        checked_functions.push(crate::pagoda::CheckedFunction {
            name: func.name.clone(),
            params: func.params.clone(),
            body: checked_body,
            span: func.span.clone(),
            return_type,
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
    functions: &HashMap<String, FunctionSignature>,
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
        Stmt::Let { name, ty, expr, span } => {
            if scopes.last().unwrap().contains_key(name) {
                return Err(SemanticError::DuplicateVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let checked_expr = analyze_expr(expr, scopes, functions, structs)?;
            let annotated_type = ty.as_ref().map(|t| parse_type_name(t));
            let resolved_type = if let Some(explicit) = annotated_type.clone() {
                if !types_compatible(&explicit, &checked_expr.ty) {
                    return Err(SemanticError::TypeMismatch {
                        expected: explicit,
                        found: checked_expr.ty,
                        span: expr.span().clone(),
                    });
                }
                explicit
            } else {
                checked_expr.ty.clone()
            };
            scopes
                .last_mut()
                .unwrap()
                .insert(name.clone(), resolved_type.clone());
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: resolved_type,
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
                if cond_checked.ty != Type::Bool
                    && cond_checked.ty != Type::Int
                    && cond_checked.ty != Type::Int32
                {
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
            if cond_checked.ty != Type::Bool
                && cond_checked.ty != Type::Int
                && cond_checked.ty != Type::Int32
            {
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
    functions: &HashMap<String, FunctionSignature>,
    structs: &HashMap<String, HashMap<String, Type>>,
) -> Result<CheckedExpr, SemanticError> {
    match expr {
        Expr::IntLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::Int,
        }),
        Expr::BoolLiteral { .. } => Ok(CheckedExpr {
            expr: expr.clone(),
            ty: Type::Bool,
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
            if !types_compatible(&existing_ty, &rhs_checked.ty) {
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
            if !types_compatible(&existing_ty, &rhs_checked.ty) {
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
            let Some(sig) = functions.get(name) else {
                return Err(SemanticError::UnknownFunction {
                    name: name.clone(),
                    span: span.clone(),
                });
            };
            if sig.param_count != args.len() {
                return Err(SemanticError::ArityMismatch {
                    name: name.clone(),
                    expected: sig.param_count,
                    found: args.len(),
                    span: span.clone(),
                });
            }
            for (idx, arg) in args.iter().enumerate() {
                let checked = analyze_expr(arg, scopes, functions, structs)?;
                if let Some(expected_ty) = sig.param_types.get(idx) {
                    if !types_compatible(expected_ty, &checked.ty) {
                        return Err(SemanticError::TypeMismatch {
                            expected: expected_ty.clone(),
                            found: checked.ty,
                            span: checked.expr.span().clone(),
                        });
                    }
                }
            }
            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: sig.return_type.clone(),
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
        Expr::Unary {
            op, expr: inner, ..
        } => {
            let checked_inner = analyze_expr(inner, scopes, functions, structs)?;
            match op {
                crate::pagoda::parser::UnaryOp::LogicalNot => {
                    // Logical NOT accepts int or bool, returns bool
                    if checked_inner.ty != Type::Int
                        && checked_inner.ty != Type::Int32
                        && checked_inner.ty != Type::Bool
                    {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Int,
                            found: checked_inner.ty,
                            span: checked_inner.expr.span().clone(),
                        });
                    }
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: Type::Bool,
                    })
                }
                _ => {
                    // Other unary ops (Plus, Minus, BitNot) expect Int and return Int
                    if !is_int_like(&checked_inner.ty) {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Int,
                            found: checked_inner.ty,
                            span: checked_inner.expr.span().clone(),
                        });
                    }
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: checked_inner.ty,
                    })
                }
            }
        }
        Expr::Binary {
            op,
            left,
            right,
            span: _,
        } => {
            let left_checked = analyze_expr(left, scopes, functions, structs)?;
            let right_checked = analyze_expr(right, scopes, functions, structs)?;

            match op {
                crate::pagoda::parser::BinOp::LogicalAnd
                | crate::pagoda::parser::BinOp::LogicalOr => {
                    // Logical operators accept int or bool for both operands, return bool
                    if left_checked.ty != Type::Int
                        && left_checked.ty != Type::Int32
                        && left_checked.ty != Type::Int16
                        && left_checked.ty != Type::Bool
                    {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Int,
                            found: left_checked.ty,
                            span: left_checked.expr.span().clone(),
                        });
                    }
                    if right_checked.ty != Type::Int
                        && right_checked.ty != Type::Int32
                        && right_checked.ty != Type::Int16
                        && right_checked.ty != Type::Bool
                    {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Int,
                            found: right_checked.ty,
                            span: right_checked.expr.span().clone(),
                        });
                    }
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: Type::Bool,
                    })
                }
                _ => {
                    // Other binary operators expect integer operands of the same width
                    if !is_int_like(&left_checked.ty) {
                        return Err(SemanticError::TypeMismatch {
                            expected: Type::Int,
                            found: left_checked.ty,
                            span: left_checked.expr.span().clone(),
                        });
                    }
                    if left_checked.ty != right_checked.ty {
                        return Err(SemanticError::TypeMismatch {
                            expected: left_checked.ty,
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
                        | crate::pagoda::parser::BinOp::BitXor => left_checked.ty,
                        crate::pagoda::parser::BinOp::Eq
                        | crate::pagoda::parser::BinOp::Ne
                        | crate::pagoda::parser::BinOp::Lt
                        | crate::pagoda::parser::BinOp::Gt
                        | crate::pagoda::parser::BinOp::Le
                        | crate::pagoda::parser::BinOp::Ge => Type::Bool,
                        _ => unreachable!(),
                    };
                    Ok(CheckedExpr {
                        expr: expr.clone(),
                        ty: result_ty,
                    })
                }
            }
        }
        Expr::Index {
            base,
            index,
            span: _,
        } => {
            let base_checked = analyze_expr(base, scopes, functions, structs)?;
            let idx_checked = analyze_expr(index, scopes, functions, structs)?;
            if idx_checked.ty != Type::Int
                && idx_checked.ty != Type::Int32
                && idx_checked.ty != Type::Int16
            {
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
            if idx_checked.ty != Type::Int
                && idx_checked.ty != Type::Int32
                && idx_checked.ty != Type::Int16
            {
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
                if !types_compatible(expected_ty, &checked.ty) {
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
                    if !types_compatible(field_ty, &value_checked.ty) {
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
        Expr::QualifiedCall { span, .. } | Expr::QualifiedStructLiteral { span, .. } => {
            Err(SemanticError::UnsupportedExpr { span: span.clone() })
        }
    }
}

/// Analyze a program with module support
/// This function loads imported modules, checks visibility, and type-checks everything
pub fn analyze_program_with_modules(
    program: Program,
    base_dir: &std::path::Path,
) -> Result<HashMap<String, Module>, SemanticError> {
    let mut modules = HashMap::new();
    let mut loading_stack = Vec::new();

    // Recursively load and analyze all imported modules
    for import in &program.imports {
        load_module_recursive(
            &import.module_name,
            base_dir,
            &mut modules,
            &mut loading_stack,
            &import.span,
        )?;
    }

    // Now analyze the main program with access to imported modules
    let checked_main = analyze_program_with_imports(&program, &modules)?;

    // Add the main module to the modules map
    modules.insert(
        "main".to_string(),
        Module {
            name: "main".to_string(),
            file_path: PathBuf::from("main.pag"),
            program: program.clone(),
            checked_program: checked_main,
            public_functions: HashMap::new(), // main doesn't export anything
            public_structs: HashMap::new(),
        },
    );

    Ok(modules)
}

/// Recursively load a module and all its dependencies
fn load_module_recursive(
    module_name: &str,
    base_dir: &std::path::Path,
    modules: &mut HashMap<String, Module>,
    loading_stack: &mut Vec<String>,
    span: &Span,
) -> Result<(), SemanticError> {
    // Check for circular dependency
    if loading_stack.contains(&module_name.to_string()) {
        let cycle = format!("{} -> {}", loading_stack.join(" -> "), module_name);
        return Err(SemanticError::CircularImport {
            cycle,
            span: span.clone(),
        });
    }

    // Check if already loaded
    if modules.contains_key(module_name) {
        return Ok(());
    }

    // Add to loading stack
    loading_stack.push(module_name.to_string());

    // Load module file
    let module_file = base_dir.join(format!("{}.pag", module_name));

    if !module_file.exists() {
        loading_stack.pop();
        return Err(SemanticError::ModuleNotFound {
            name: module_name.to_string(),
            span: span.clone(),
        });
    }

    // Read and parse the imported module
    let source = std::fs::read_to_string(&module_file).map_err(|_| {
        loading_stack.pop();
        SemanticError::ModuleNotFound {
            name: module_name.to_string(),
            span: span.clone(),
        }
    })?;

    let tokens = crate::pagoda::tokenizer::tokenize(&source).map_err(|_| {
        loading_stack.pop();
        SemanticError::UnsupportedExpr { span: span.clone() }
    })?;

    let imported_program = crate::pagoda::parser::parse_program(&tokens).map_err(|_| {
        loading_stack.pop();
        SemanticError::UnsupportedExpr { span: span.clone() }
    })?;

    // Recursively load all dependencies of this module
    for import in &imported_program.imports {
        load_module_recursive(
            &import.module_name,
            base_dir,
            modules,
            loading_stack,
            &import.span,
        )?;
    }

    // Now analyze this module with its dependencies available
    let checked_program = analyze_program_with_imports(&imported_program, modules)?;

    // Extract public interface
    let mut public_functions = HashMap::new();
    for func in &imported_program.functions {
        if func.is_public {
            let param_types: Vec<Type> = func
                .params
                .iter()
                .map(|p| {
                    p.ty.as_ref()
                        .map(|t| parse_type_name(t))
                        .unwrap_or(Type::Int)
                })
                .collect();
            let return_type = func
                .return_type
                .as_ref()
                .map(|t| parse_type_name(t))
                .unwrap_or(Type::Int);
            public_functions.insert(
                func.name.clone(),
                FunctionSignature {
                    param_count: func.params.len(),
                    return_type,
                    param_types,
                },
            );
        }
    }

    let mut public_structs = HashMap::new();
    for struct_def in &imported_program.structs {
        if struct_def.is_public {
            let mut fields = HashMap::new();
            for field in &struct_def.fields {
                let field_type = parse_type_name(&field.ty);
                fields.insert(field.name.clone(), field_type);
            }
            public_structs.insert(struct_def.name.clone(), StructSignature { fields });
        }
    }

    // Add to modules map
    modules.insert(
        module_name.to_string(),
        Module {
            name: module_name.to_string(),
            file_path: module_file,
            program: imported_program,
            checked_program,
            public_functions,
            public_structs,
        },
    );

    // Remove from loading stack
    loading_stack.pop();

    Ok(())
}

/// Analyze a program that can reference imported modules
pub fn analyze_program_with_imports(
    program: &Program,
    imported_modules: &HashMap<String, Module>,
) -> Result<CheckedProgram, SemanticError> {
    let mut scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
    let mut functions: HashMap<String, FunctionSignature> = HashMap::new();
    let mut structs: HashMap<String, HashMap<String, Type>> = HashMap::new();
    let mut checked_functions = Vec::new();

    // Collect local struct definitions
    for struct_def in &program.structs {
        let mut fields = HashMap::new();
        for field in &struct_def.fields {
            let field_ty = parse_type_name(&field.ty);
            if fields.contains_key(&field.name) {
                return Err(SemanticError::DuplicateVariable {
                    name: field.name.clone(),
                    span: field.span.clone(),
                });
            }
            fields.insert(field.name.clone(), field_ty);
        }
        if structs.contains_key(&struct_def.name) {
            return Err(SemanticError::DuplicateFunction {
                name: struct_def.name.clone(),
                span: struct_def.span.clone(),
            });
        }
        structs.insert(struct_def.name.clone(), fields);
    }

    // Collect local function names
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
        let param_types: Vec<Type> = func
            .params
            .iter()
            .map(|p| {
                p.ty.as_ref()
                    .map(|t| parse_type_name(t))
                    .unwrap_or(Type::Int)
            })
            .collect();
        let return_type = func
            .return_type
            .as_ref()
            .map(|t| parse_type_name(t))
            .unwrap_or(Type::Int);
        functions.insert(
            func.name.clone(),
            FunctionSignature {
                param_types,
                param_count: func.params.len(),
                return_type,
            },
        );
    }

    // Type-check each function
    for func in &program.functions {
        let mut fn_scopes: Vec<HashMap<String, Type>> = vec![HashMap::new()];
        for param in &func.params {
            fn_scopes
                .last_mut()
                .unwrap()
                .insert(
                    param.name.clone(),
                    param
                        .ty
                        .as_ref()
                        .map(|t| parse_type_name(t))
                        .unwrap_or(Type::Int),
                );
        }

        let checked_body = analyze_stmt_with_imports(
            &func.body,
            &mut fn_scopes,
            &functions,
            &structs,
            imported_modules,
        )?;

        let return_type = func
            .return_type
            .as_ref()
            .map(|t| parse_type_name(t))
            .unwrap_or(Type::Int);
        checked_functions.push(crate::pagoda::CheckedFunction {
            name: func.name.clone(),
            params: func.params.clone(),
            body: checked_body,
            span: func.span.clone(),
            return_type,
        });
    }

    // Type-check top-level statements
    let mut checked_stmts = Vec::new();
    for stmt in &program.stmts {
        let checked =
            analyze_stmt_with_imports(stmt, &mut scopes, &functions, &structs, imported_modules)?;
        checked_stmts.push(checked);
    }

    Ok(CheckedProgram {
        structs: program.structs.clone(),
        functions: checked_functions,
        stmts: checked_stmts,
        span: program.span.clone(),
    })
}

/// Analyze a statement with access to imported modules
fn analyze_stmt_with_imports(
    stmt: &Stmt,
    scopes: &mut Vec<HashMap<String, Type>>,
    functions: &HashMap<String, FunctionSignature>,
    structs: &HashMap<String, HashMap<String, Type>>,
    imported_modules: &HashMap<String, Module>,
) -> Result<CheckedStmt, SemanticError> {
    match stmt {
        Stmt::Expr { expr, span: _ } => {
            let checked =
                analyze_expr_with_imports(expr, scopes, functions, structs, imported_modules)?;
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: checked.ty,
            })
        }
        Stmt::Empty { .. } => Ok(CheckedStmt {
            stmt: stmt.clone(),
            ty: Type::Int,
        }),
        Stmt::Let { name, ty, expr, span } => {
            let checked_expr =
                analyze_expr_with_imports(expr, scopes, functions, structs, imported_modules)?;
            if scopes.last().unwrap().contains_key(name) {
                return Err(SemanticError::DuplicateVariable {
                    name: name.clone(),
                    span: span.clone(),
                });
            }
            let annotated_type = ty.as_ref().map(|t| parse_type_name(t));
            let resolved_type = if let Some(explicit) = annotated_type.clone() {
                if !types_compatible(&explicit, &checked_expr.ty) {
                    return Err(SemanticError::TypeMismatch {
                        expected: explicit,
                        found: checked_expr.ty,
                        span: expr.span().clone(),
                    });
                }
                explicit
            } else {
                checked_expr.ty.clone()
            };
            scopes
                .last_mut()
                .unwrap()
                .insert(name.clone(), resolved_type.clone());
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: resolved_type,
            })
        }
        Stmt::Return { expr, .. } => {
            let checked =
                analyze_expr_with_imports(expr, scopes, functions, structs, imported_modules)?;
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: checked.ty,
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
                analyze_stmt_with_imports(init_stmt, scopes, functions, structs, imported_modules)?;
            }
            if let Some(cond_expr) = cond {
                analyze_expr_with_imports(cond_expr, scopes, functions, structs, imported_modules)?;
            }
            if let Some(post_expr) = post {
                analyze_expr_with_imports(post_expr, scopes, functions, structs, imported_modules)?;
            }
            analyze_stmt_with_imports(body, scopes, functions, structs, imported_modules)?;
            scopes.pop();
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: Type::Int,
            })
        }
        Stmt::Block { stmts, .. } => {
            scopes.push(HashMap::new());
            let mut last_ty = Type::Int;
            for s in stmts {
                let checked =
                    analyze_stmt_with_imports(s, scopes, functions, structs, imported_modules)?;
                last_ty = checked.ty;
            }
            scopes.pop();
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: last_ty,
            })
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
            ..
        } => {
            analyze_expr_with_imports(cond, scopes, functions, structs, imported_modules)?;
            analyze_stmt_with_imports(then_branch, scopes, functions, structs, imported_modules)?;
            if let Some(else_br) = else_branch {
                analyze_stmt_with_imports(else_br, scopes, functions, structs, imported_modules)?;
            }
            Ok(CheckedStmt {
                stmt: stmt.clone(),
                ty: Type::Int,
            })
        }
    }
}

/// Analyze an expression with access to imported modules
fn analyze_expr_with_imports(
    expr: &Expr,
    scopes: &Vec<HashMap<String, Type>>,
    functions: &HashMap<String, FunctionSignature>,
    structs: &HashMap<String, HashMap<String, Type>>,
    imported_modules: &HashMap<String, Module>,
) -> Result<CheckedExpr, SemanticError> {
    match expr {
        // Handle qualified function calls: module::function(args)
        Expr::QualifiedCall {
            module,
            name,
            args,
            span,
        } => {
            // Look up the module
            let imported_module =
                imported_modules
                    .get(module)
                    .ok_or_else(|| SemanticError::ModuleNotFound {
                        name: module.clone(),
                        span: span.clone(),
                    })?;

            // Look up the function in that module's public interface
            let func_sig = imported_module.public_functions.get(name).ok_or_else(|| {
                SemanticError::UnknownModuleFunction {
                    module: module.clone(),
                    name: name.clone(),
                    span: span.clone(),
                }
            })?;

            // Type-check arguments
            if args.len() != func_sig.param_count {
                return Err(SemanticError::ArityMismatch {
                    name: format!("{}::{}", module, name),
                    expected: func_sig.param_count,
                    found: args.len(),
                    span: span.clone(),
                });
            }

            for (idx, arg) in args.iter().enumerate() {
                let checked =
                    analyze_expr_with_imports(arg, scopes, functions, structs, imported_modules)?;
                if let Some(expected_ty) = func_sig.param_types.get(idx) {
                    if !types_compatible(expected_ty, &checked.ty) {
                        return Err(SemanticError::TypeMismatch {
                            expected: expected_ty.clone(),
                            found: checked.ty,
                            span: checked.expr.span().clone(),
                        });
                    }
                }
            }

            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: func_sig.return_type.clone(),
            })
        }

        // Handle qualified struct literals: module::Struct { fields }
        Expr::QualifiedStructLiteral {
            module,
            struct_name,
            field_values,
            span,
        } => {
            // Look up the module
            let imported_module =
                imported_modules
                    .get(module)
                    .ok_or_else(|| SemanticError::ModuleNotFound {
                        name: module.clone(),
                        span: span.clone(),
                    })?;

            // Look up the struct in that module's public interface
            let struct_sig = imported_module
                .public_structs
                .get(struct_name)
                .ok_or_else(|| SemanticError::UnknownModuleStruct {
                    module: module.clone(),
                    name: struct_name.clone(),
                    span: span.clone(),
                })?;

            // Type-check field values
            for (field_name, field_expr) in field_values {
                let field_type = struct_sig.fields.get(field_name).ok_or_else(|| {
                    SemanticError::UnknownVariable {
                        name: field_name.clone(),
                        span: field_expr.span().clone(),
                    }
                })?;

                let checked_field = analyze_expr_with_imports(
                    field_expr,
                    scopes,
                    functions,
                    structs,
                    imported_modules,
                )?;

                if !types_compatible(field_type, &checked_field.ty) {
                    return Err(SemanticError::TypeMismatch {
                        expected: field_type.clone(),
                        found: checked_field.ty,
                        span: field_expr.span().clone(),
                    });
                }
            }

            Ok(CheckedExpr {
                expr: expr.clone(),
                ty: Type::Struct(format!("{}::{}", module, struct_name)),
            })
        }

        // All other expressions: delegate to existing analyze_expr
        _ => analyze_expr(expr, scopes, functions, structs),
    }
}
