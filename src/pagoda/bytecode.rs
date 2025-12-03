use std::io::Write;

use std::collections::HashMap;

use thiserror::Error;

use crate::pagoda::parser::{BinOp, UnaryOp};
use crate::pagoda::semantics::{FunctionSignature, Type};
use crate::pagoda::{CheckedProgram, Expr, Span, Stmt};

macro_rules! emit_line {
    ($emitter:expr, $span:expr, $($arg:tt)*) => {
        $emitter.write_line($span, format_args!($($arg)*))
    };
}

const I8_SIZE: usize = 1;
const I16_SIZE: usize = 2;
const I32_SIZE: usize = 4;
const I64_SIZE: usize = 8;
const PTR_SIZE: usize = 8;
const WORD_SIZE: usize = I64_SIZE;
const HEAP_REG: &str = "%r12";
const SCRATCH_REG: &str = "%r7"; // caller-saved and not aliased to SP/PC/FP

#[derive(Debug, Clone)]
struct StructLayout {
    fields: Vec<FieldLayout>,
    size: usize,
}

#[derive(Debug, Clone)]
struct FieldLayout {
    name: String,
    ty: Type,
    offset: usize,
}

#[derive(Debug, Clone)]
struct FnInfo {
    label: String,
    sig: FunctionSignature,
}

#[derive(Debug, Error)]
pub enum BytecodeError {
    #[error("I/O while emitting assembly: {source}")]
    Io { source: std::io::Error, span: Span },
    #[error("unknown variable '{name}'")]
    UnknownVariable { name: String, span: Span },
    #[error("unknown function '{name}'")]
    UnknownFunction { name: String, span: Span },
    #[error("unsupported expression (module system not yet implemented in codegen)")]
    UnsupportedExpr { span: Span },
}

impl BytecodeError {
    pub fn span(&self) -> &Span {
        match self {
            BytecodeError::Io { span, .. } => span,
            BytecodeError::UnknownVariable { span, .. } => span,
            BytecodeError::UnknownFunction { span, .. } => span,
            BytecodeError::UnsupportedExpr { span } => span,
        }
    }
}

fn type_size_bytes(ty: &Type) -> usize {
    match ty {
        Type::Int => I64_SIZE,
        Type::Int32 => I32_SIZE,
        Type::Int16 => I16_SIZE,
        Type::Int8 => I8_SIZE,
        Type::UInt => I64_SIZE,
        Type::UInt32 => I32_SIZE,
        Type::UInt16 => I16_SIZE,
        Type::UInt8 => I8_SIZE,
        Type::Bool => I64_SIZE,
        Type::String => PTR_SIZE,
        Type::Array(_) => PTR_SIZE,
        Type::Struct(_) => PTR_SIZE,
    }
}

fn type_suffix(ty: &Type) -> &'static str {
    match ty {
        Type::Int16 => "s",
        Type::Int32 => "l",
        Type::Int8 => "b",
        Type::UInt => "w",
        Type::UInt32 => "l",
        Type::UInt16 => "s",
        Type::UInt8 => "b",
        _ => "w",
    }
}

fn parse_type_name(name: &str) -> Type {
    match name {
        "int" => Type::Int,
        "i64" => Type::Int,
        "i32" => Type::Int32,
        "i16" => Type::Int16,
        "i8" => Type::Int8,
        "u64" => Type::UInt,
        "u32" => Type::UInt32,
        "u16" => Type::UInt16,
        "u8" => Type::UInt8,
        "bool" => Type::Bool,
        "string" => Type::String,
        other => Type::Struct(other.to_string()),
    }
}

fn build_struct_layouts(program: &CheckedProgram) -> HashMap<String, StructLayout> {
    let mut layouts = HashMap::new();
    for s in &program.structs {
        let mut offset = 0usize;
        let mut fields = Vec::new();
        for field in &s.fields {
            let ty = parse_type_name(&field.ty);
            fields.push(FieldLayout {
                name: field.name.clone(),
                ty: ty.clone(),
                offset,
            });
            offset += type_size_bytes(&ty);
        }
        layouts.insert(
            s.name.clone(),
            StructLayout {
                fields,
                size: offset,
            },
        );
    }
    layouts
}

fn struct_field_layout<'a>(
    layouts: &'a HashMap<String, StructLayout>,
    struct_name: &str,
    field_name: &str,
) -> Option<&'a FieldLayout> {
    layouts
        .get(struct_name)
        .and_then(|layout| layout.fields.iter().find(|f| f.name == field_name))
}

fn build_function_info(program: &CheckedProgram) -> HashMap<String, FnInfo> {
    let mut map = HashMap::new();
    for func in &program.functions {
        let param_types: Vec<Type> = func
            .params
            .iter()
            .map(|p| {
                p.ty.as_ref()
                    .map(|t| parse_type_name(t))
                    .unwrap_or(Type::Int)
            })
            .collect();
        let sig = FunctionSignature {
            param_count: func.params.len(),
            return_type: func.return_type.clone(),
            param_types,
        };
        map.insert(
            func.name.clone(),
            FnInfo {
                label: format!("fn_{}", func.name),
                sig,
            },
        );
    }
    map
}

impl<'a, W: Write> BytecodeEmitter<'a, W> {
    fn infer_expr_type(&mut self, expr: &Expr) -> Result<Type, BytecodeError> {
        match expr {
            Expr::IntLiteral { .. } => Ok(Type::Int),
            Expr::BoolLiteral { .. } => Ok(Type::Bool),
            Expr::StringLiteral { .. } => Ok(Type::String),
            Expr::Var { name, span } => {
                self.env
                    .get(name)
                    .map(|v| v.ty.clone())
                    .ok_or(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    })
            }
            Expr::Unary { op, expr, .. } => {
                let inner = self.infer_expr_type(expr)?;
                match op {
                    UnaryOp::LogicalNot => Ok(Type::Bool),
                    _ => Ok(inner),
                }
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                let left_ty = self.infer_expr_type(left)?;
                let _right_ty = self.infer_expr_type(right)?;
                match op {
                    BinOp::Eq
                    | BinOp::Ne
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::Le
                    | BinOp::Ge
                    | BinOp::LogicalAnd
                    | BinOp::LogicalOr => Ok(Type::Bool),
                    _ => Ok(left_ty),
                }
            }
            Expr::Assign { name, span, .. } | Expr::CompoundAssign { name, span, .. } => self
                .env
                .get(name)
                .map(|v| v.ty.clone())
                .ok_or(BytecodeError::UnknownVariable {
                    name: name.clone(),
                    span: span.clone(),
                }),
            Expr::Call { name, span, .. } => self
                .function_labels
                .get(name)
                .map(|f| f.sig.return_type.clone())
                .ok_or(BytecodeError::UnknownFunction {
                    name: name.clone(),
                    span: span.clone(),
                }),
            Expr::QualifiedCall {
                module, name, span, ..
            } => {
                let mangled = format!("{}_{}", module, name);
                self.function_labels
                    .get(&mangled)
                    .map(|f| f.sig.return_type.clone())
                    .ok_or(BytecodeError::UnknownFunction {
                        name: format!("{module}::{name}"),
                        span: span.clone(),
                    })
            }
            Expr::ArrayLiteral { elements, .. } => Ok(Type::Array(elements.len())),
            Expr::Index { .. } | Expr::IndexAssign { .. } => Ok(Type::Int),
            Expr::StructLiteral { struct_name, .. } => Ok(Type::Struct(struct_name.clone())),
            Expr::QualifiedStructLiteral {
                module,
                struct_name,
                ..
            } => Ok(Type::Struct(format!("{module}::{struct_name}"))),
            Expr::FieldAccess {
                base,
                field_name,
                span,
            }
            | Expr::FieldAssign {
                base,
                field_name,
                span,
                ..
            } => {
                let base_ty = self.infer_expr_type(base)?;
                let Type::Struct(struct_name) = base_ty else {
                    return Err(BytecodeError::UnknownVariable {
                        name: field_name.clone(),
                        span: span.clone(),
                    });
                };
                struct_field_layout(&self.struct_layouts, &struct_name, field_name)
                    .map(|f| f.ty.clone())
                    .ok_or(BytecodeError::UnknownVariable {
                        name: format!("{struct_name}.{field_name}"),
                        span: span.clone(),
                    })
            }
        }
    }
}

pub fn emit_exit_program(
    program: &CheckedProgram,
    mut writer: impl Write,
) -> Result<(), BytecodeError> {
    let mut emitter = BytecodeEmitter::new(program, &mut writer);
    emitter.emit_program(program)
}

#[derive(Debug, Clone)]
struct VarSlot {
    depth_bytes: usize, // total bytes on stack when defined
    ty: Type,
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

struct BytecodeEmitter<'a, W: Write> {
    writer: &'a mut W,
    function_labels: HashMap<String, FnInfo>,
    struct_layouts: HashMap<String, StructLayout>,
    labels: LabelGen,
    stack_depth_bytes: usize,
    env: HashMap<String, VarSlot>,
}

impl<'a, W: Write> BytecodeEmitter<'a, W> {
    fn new(program: &CheckedProgram, writer: &'a mut W) -> Self {
        Self {
            writer,
            function_labels: build_function_info(program),
            struct_layouts: build_struct_layouts(program),
            labels: LabelGen::default(),
            stack_depth_bytes: 0,
            env: HashMap::new(),
        }
    }
}

impl<'a, W: Write> BytecodeEmitter<'a, W> {
    fn write_line(
        &mut self,
        span: &Span,
        args: std::fmt::Arguments<'_>,
    ) -> Result<(), BytecodeError> {
        self.writer
            .write_fmt(format_args!("{args}\n"))
            .map_err(|e| BytecodeError::Io {
                source: e,
                span: span.clone(),
            })
    }

    fn emit_block(
        &mut self,
        stmts: &[Stmt],
        _span: &Span,
        return_label: Option<&str>,
    ) -> Result<(), BytecodeError> {
        let saved_env = self.env.clone();
        let depth_before = self.stack_depth_bytes;

        for stmt in stmts {
            self.emit_stmt(stmt, return_label)?;
        }

        let locals_to_pop = self.stack_depth_bytes.saturating_sub(depth_before);
        self.emit_pop_all_locals(locals_to_pop)?;
        self.stack_depth_bytes = depth_before;

        self.env = saved_env;
        Ok(())
    }

    fn emit_program(&mut self, program: &CheckedProgram) -> Result<(), BytecodeError> {
        // runtime init: heap pointer from brk(0)
        emit_line!(self, &program.span, "  movi %r0, $12")?;
        emit_line!(self, &program.span, "  xor.w %r1, %r1")?;
        emit_line!(self, &program.span, "  trap")?;
        emit_line!(self, &program.span, "  mov {HEAP_REG}, %r0")?;

        // Ensure execution starts at main by jumping over function bodies.
        emit_line!(self, &program.span, "  jmp main")?;

        for func in &program.functions {
            self.emit_function(func)?;
        }

        emit_line!(self, &program.span, "main:")?;

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
            emit_line!(self, &program.span, "  call fn_main")?;
        }

        if !program.stmts.is_empty() {
            let ret_label = program
                .stmts
                .iter()
                .any(|stmt| stmt_contains_return(&stmt.stmt))
                .then_some("ret_exit");
            self.env.clear();
            self.stack_depth_bytes = 0;
            for stmt in &program.stmts {
                self.emit_stmt(&stmt.stmt, ret_label)?;
            }
            self.emit_pop_all_locals(self.stack_depth_bytes)?;
            if let Some(label) = ret_label {
                emit_line!(self, &program.span, "{label}:")?;
            }
        }

        emit_line!(self, &program.span, "  push.w %r0")?;
        emit_line!(self, &program.span, "  pop.w %r1")?;
        emit_line!(self, &program.span, "  movi %r0, $60")?;
        emit_line!(self, &program.span, "  trap")
    }

    fn emit_function(
        &mut self,
        func: &crate::pagoda::CheckedFunction,
    ) -> Result<(), BytecodeError> {
        let label = self
            .function_labels
            .get(&func.name)
            .map(|info| info.label.clone())
            .unwrap_or_else(|| format!("fn_{}", func.name));
        let ret_label = format!("{label}_ret");
        emit_line!(self, &func.span, "{label}:")?;
        self.env.clear();
        self.stack_depth_bytes = 0;
        let extra_stack_args = func.params.len().saturating_sub(8);
        if extra_stack_args > 0 {
            // Stack args arrive left-to-right; the last extra arg is at 0(%sp).
            // Load them in reverse so each subsequent load can use the current SP offset.
            for pname in func.params.iter().skip(8).rev() {
                let param_ty = pname
                    .ty
                    .as_ref()
                    .map(|t| parse_type_name(t))
                    .unwrap_or(Type::Int);
                let suffix = type_suffix(&param_ty);
                let disp = self.stack_depth_bytes as i64;
                emit_line!(
                    self,
                    &func.span,
                    "  load.{suffix} %r0, {disp}(%sp)  # load stack arg {}",
                    pname.name
                )?;
                emit_line!(self, &func.span, "  push.{suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&param_ty);
                self.env.insert(
                    pname.name.clone(),
                    VarSlot {
                        depth_bytes: self.stack_depth_bytes,
                        ty: param_ty.clone(),
                        struct_name: None,
                    },
                );
            }
        }
        // Bind register parameters by saving them to the stack in order.
        for (idx, pname) in func.params.iter().take(8).enumerate() {
            let reg = idx + 1; // r1..r8
            let ty = pname
                .ty
                .as_ref()
                .map(|t| parse_type_name(t))
                .unwrap_or(Type::Int);
            let suffix = type_suffix(&ty);
            emit_line!(self, &func.span, "  push.{suffix} %r{reg}")?;
            self.stack_depth_bytes += type_size_bytes(&ty);
            self.env.insert(
                pname.name.clone(),
                VarSlot {
                    depth_bytes: self.stack_depth_bytes,
                    ty,
                    struct_name: None,
                },
            );
        }
        self.emit_stmt(&func.body.stmt, Some(ret_label.as_str()))?;
        self.emit_pop_all_locals(self.stack_depth_bytes)?;
        emit_line!(self, &func.span, "  jmp {ret_label}")?;
        emit_line!(self, &func.span, "{ret_label}:")?;
        emit_line!(self, &func.span, "  ret")
    }

    fn emit_stmt(&mut self, stmt: &Stmt, return_label: Option<&str>) -> Result<(), BytecodeError> {
        match stmt {
            Stmt::Expr { expr, .. } => self.emit_expr(expr, None),
            Stmt::Empty { .. } => Ok(()),
            Stmt::Let { name, ty, expr, .. } => {
                let value_ty = self.infer_expr_type(expr)?;
                let declared_ty = ty.as_ref().map(|t| parse_type_name(t));
                let slot_ty = declared_ty.clone().unwrap_or_else(|| value_ty.clone());
                self.emit_expr(expr, Some(slot_ty.clone()))?;
                let suffix = type_suffix(&slot_ty);
                emit_line!(self, &expr.span().clone(), "  push.{suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&slot_ty);
                let struct_name = match &slot_ty {
                    Type::Struct(name) => Some(name.clone()),
                    _ => self.expr_struct_type(expr),
                };
                self.env.insert(
                    name.clone(),
                    VarSlot {
                        depth_bytes: self.stack_depth_bytes,
                        ty: slot_ty,
                        struct_name,
                    },
                );
                Ok(())
            }
            Stmt::Return { expr, .. } => {
                self.emit_expr(expr, None)?;
                self.emit_pop_all_locals(self.stack_depth_bytes)?;
                if let Some(label) = return_label {
                    emit_line!(self, &expr.span().clone(), "  jmp {label}")?;
                }
                Ok(())
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                span,
            } => {
                let depth_before = self.stack_depth_bytes;
                self.emit_expr(cond, None)?;
                let else_label = self.labels.fresh();
                let end_label = self.labels.fresh();
                emit_line!(self, span, "  brz.w %r0, {else_label}")?;
                self.stack_depth_bytes = depth_before;
                self.emit_stmt(then_branch, return_label)?;
                if let Some(else_branch) = else_branch {
                    emit_line!(self, span, "  jmp {end_label}")?;
                    emit_line!(self, span, "{else_label}:")?;
                    self.stack_depth_bytes = depth_before;
                    self.emit_stmt(else_branch, return_label)?;
                    emit_line!(self, span, "{end_label}:")?;
                } else {
                    emit_line!(self, span, "{else_label}:")?;
                }
                self.stack_depth_bytes = depth_before;
                Ok(())
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
                span,
            } => {
                let saved_env = self.env.clone();
                let depth_before = self.stack_depth_bytes;

                if let Some(init_stmt) = init {
                    self.emit_stmt(init_stmt, return_label)?;
                }

                let loop_label = self.labels.fresh();
                let end_label = self.labels.fresh();
                emit_line!(self, span, "{loop_label}:")?;

                let cond_depth = self.stack_depth_bytes;
                if let Some(cond_expr) = cond {
                    self.emit_expr(cond_expr, None)?;
                    emit_line!(self, span, "  brz.w %r0, {end_label}")?;
                    self.stack_depth_bytes = cond_depth;
                }

                self.emit_stmt(body, return_label)?;
                self.stack_depth_bytes = cond_depth;

                if let Some(post_expr) = post {
                    self.emit_expr(post_expr, None)?;
                    self.stack_depth_bytes = cond_depth;
                }

                emit_line!(self, span, "  jmp {loop_label}")?;
                emit_line!(self, span, "{end_label}:")?;

                let locals_to_pop = self.stack_depth_bytes.saturating_sub(depth_before);
                self.emit_pop_all_locals(locals_to_pop)?;
                self.stack_depth_bytes = depth_before;

                self.env = saved_env;
                Ok(())
            }
            Stmt::Block { stmts, span } => self.emit_block(stmts, span, return_label),
        }
    }

    fn emit_pop_all_locals(&mut self, depth_bytes: usize) -> Result<(), BytecodeError> {
        if depth_bytes == 0 {
            return Ok(());
        }
        let empty_span = Span {
            start: 0,
            end: 0,
            literal: String::new(),
        };
        emit_line!(self, &empty_span, "  load.w {SCRATCH_REG}, ${depth_bytes}")?;
        emit_line!(self, &empty_span, "  add.w %sp, {SCRATCH_REG}")
    }

    fn struct_field_offset(&self, struct_name: &str, field_name: &str) -> Option<i64> {
        struct_field_layout(&self.struct_layouts, struct_name, field_name)
            .map(|field| field.offset as i64)
    }

    fn struct_field_type(&self, struct_name: &str, field_name: &str) -> Option<Type> {
        struct_field_layout(&self.struct_layouts, struct_name, field_name).map(|f| f.ty.clone())
    }

    fn expr_struct_type(&self, expr: &Expr) -> Option<String> {
        match expr {
            Expr::StructLiteral { struct_name, .. } => Some(struct_name.clone()),
            Expr::Var { name, .. } => self.env.get(name).and_then(|v| v.struct_name.clone()),
            Expr::Assign { name, .. } => self.env.get(name).and_then(|v| v.struct_name.clone()),
            Expr::CompoundAssign { name, .. } => {
                self.env.get(name).and_then(|v| v.struct_name.clone())
            }
            Expr::FieldAccess { base, .. } | Expr::FieldAssign { base, .. } => {
                self.expr_struct_type(base)
            }
            Expr::QualifiedCall { .. } | Expr::QualifiedStructLiteral { .. } => None,
            _ => None,
        }
    }

    fn emit_expr(&mut self, expr: &Expr, expected_ty: Option<Type>) -> Result<(), BytecodeError> {
        match expr {
            Expr::IntLiteral { value, span } => {
                let hint_ty = expected_ty
                    .or_else(|| self.infer_expr_type(expr).ok())
                    .unwrap_or(Type::Int);
                let suffix = type_suffix(&hint_ty);
                emit_line!(
                    self,
                    span,
                    "  load.{suffix} %r0, ${value}  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::BoolLiteral { value, span } => {
                let int_value = if *value { 1 } else { 0 };
                emit_line!(
                    self,
                    span,
                    "  load.w %r0, ${}  # span {}..{} \"{}\"",
                    int_value,
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::Var { name, span } => {
                let Some(slot) = self.env.get(name) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                };
                if self.stack_depth_bytes < slot.depth_bytes {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }
                let offset_bytes = self.stack_depth_bytes - slot.depth_bytes;
                let disp_bytes = offset_bytes as i64;
                emit_line!(
                    self,
                    span,
                    "  load.{suffix} %r0, {}(%sp)  # span {}..{} \"{}\"",
                    disp_bytes,
                    span.start,
                    span.end,
                    span.literal,
                    suffix = type_suffix(&slot.ty)
                )
            }
            Expr::ArrayLiteral { elements, span } => {
                let bytes = (elements.len() * WORD_SIZE) as i64;
                // r1 = base (old heap), r2 = new_brk
                emit_line!(
                    self,
                    span,
                    "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )?;
                emit_line!(self, span, "  mov.w %r2, {HEAP_REG}")?;
                emit_line!(self, span, "  load.w %r0, ${bytes}")?;
                emit_line!(self, span, "  add.w %r2, %r0")?;
                emit_line!(self, span, "  movi %r0, $12")?;
                emit_line!(self, span, "  mov.w %r1, %r2")?;
                emit_line!(self, span, "  trap")?;
                emit_line!(self, span, "  mov.w {HEAP_REG}, %r2")?;
                for (i, el) in elements.iter().enumerate() {
                    self.emit_expr(el, None)?;
                    let disp = (i * WORD_SIZE) as i64;
                    emit_line!(
                        self,
                        span,
                        "  store.w %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                        span.start,
                        span.end,
                        span.literal
                    )?;
                }
                emit_line!(self, span, "  mov.w %r0, %r1")
            }
            Expr::StringLiteral { value, span } => {
                let bytes = value.len() as i64;
                emit_line!(
                    self,
                    span,
                    "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )?;
                emit_line!(self, span, "  mov.w %r2, {HEAP_REG}")?;
                emit_line!(self, span, "  load.w %r0, ${bytes}")?;
                emit_line!(self, span, "  add.w %r2, %r0")?;
                emit_line!(self, span, "  movi %r0, $12")?;
                emit_line!(self, span, "  mov.w %r1, %r2")?;
                emit_line!(self, span, "  trap")?;
                emit_line!(self, span, "  mov.w {HEAP_REG}, %r2")?;
                for (i, ch) in value.bytes().enumerate() {
                    emit_line!(self, span, "  load.w %r0, ${ch}")?;
                    let disp = i as i64;
                    emit_line!(
                        self,
                        span,
                        "  store.b %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                        span.start,
                        span.end,
                        span.literal
                    )?;
                }
                emit_line!(self, span, "  mov.w %r0, %r1")
            }
            Expr::StructLiteral {
                struct_name,
                field_values,
                span,
            } => {
                let Some(fields) = self.struct_layouts.get(struct_name) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: format!("struct {struct_name}"),
                        span: span.clone(),
                    });
                };
                let bytes = fields.size as i64;
                emit_line!(
                    self,
                    span,
                    "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )?;
                emit_line!(self, span, "  mov.w %r2, {HEAP_REG}")?;
                emit_line!(self, span, "  load.w %r0, ${bytes}")?;
                emit_line!(self, span, "  add.w %r2, %r0")?;
                emit_line!(self, span, "  movi %r0, $12")?;
                emit_line!(self, span, "  mov.w %r1, %r2")?;
                emit_line!(self, span, "  trap")?;
                emit_line!(self, span, "  mov.w {HEAP_REG}, %r2")?;
                for (field_name, field_expr) in field_values {
                    let Some(disp) = self.struct_field_offset(struct_name, field_name) else {
                        return Err(BytecodeError::UnknownVariable {
                            name: format!("{struct_name}.{field_name}"),
                            span: field_expr.span().clone(),
                        });
                    };
                    let field_ty = self
                        .struct_field_type(struct_name, field_name)
                        .unwrap_or(Type::Int);
                    let suffix = type_suffix(&field_ty);
                    emit_line!(self, span, "  push.w %r1")?;
                    self.stack_depth_bytes += PTR_SIZE;
                    self.emit_expr(field_expr, Some(field_ty.clone()))?;
                    emit_line!(self, span, "  pop.w {SCRATCH_REG}")?;
                    self.stack_depth_bytes = self.stack_depth_bytes.saturating_sub(PTR_SIZE);
                    emit_line!(self, span, "  mov.w %r1, {SCRATCH_REG}")?;
                    emit_line!(
                        self,
                        span,
                        "  store.{suffix} %r0, {disp}(%r1)  # span {}..{} \"{}\"",
                        span.start,
                        span.end,
                        span.literal
                    )?;
                }
                emit_line!(self, span, "  mov.w %r0, %r1")
            }
            Expr::Unary { op, expr, span } => {
                let unary_ty = expected_ty
                    .clone()
                    .or_else(|| self.infer_expr_type(expr).ok())
                    .unwrap_or(Type::Int);
                let suffix = type_suffix(&unary_ty);
                self.emit_expr(expr, Some(unary_ty.clone()))?;
                match op {
                    UnaryOp::Plus => Ok(()),
                    UnaryOp::Minus => emit_line!(
                        self,
                        span,
                        "  neg.{suffix} %r0  # span {}..{} \"{}\"",
                        span.start,
                        span.end,
                        span.literal
                    ),
                    UnaryOp::BitNot => emit_line!(
                        self,
                        span,
                        "  not.{suffix} %r0  # span {}..{} \"{}\"",
                        span.start,
                        span.end,
                        span.literal
                    ),
                    UnaryOp::LogicalNot => {
                        // !expr -> brz %r0, true_label; load 0; jmp end; true_label: load 1; end:
                        let true_label = self.labels.fresh();
                        let end_label = self.labels.fresh();
                        emit_line!(
                            self,
                            span,
                            "  brz.w %r0, {true_label}  # span {}..{} \"{}\"",
                            span.start,
                            span.end,
                            span.literal
                        )?;
                        emit_line!(self, span, "  load.w %r0, $0")?;
                        emit_line!(self, span, "  jmp {end_label}")?;
                        emit_line!(self, span, "{true_label}:")?;
                        emit_line!(self, span, "  load.w %r0, $1")?;
                        emit_line!(self, span, "{end_label}:")
                    }
                }
            }
            Expr::Assign { name, value, span } => {
                let Some(slot) = self.env.get(name).cloned() else {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                };
                self.emit_expr(value, Some(slot.ty.clone()))?;
                if self.stack_depth_bytes < slot.depth_bytes {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }
                let offset_bytes = self.stack_depth_bytes - slot.depth_bytes;
                let disp_bytes = offset_bytes as i64;
                let suffix = type_suffix(&slot.ty);
                emit_line!(
                    self,
                    span,
                    "  store.{suffix} %r0, {}(%sp)  # span {}..{} \"{}\"",
                    disp_bytes,
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::FieldAccess {
                base,
                field_name,
                span,
            } => {
                self.emit_expr(base, None)?;
                let Some(struct_name) = self.expr_struct_type(base) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: field_name.to_string(),
                        span: span.clone(),
                    });
                };
                let Some(disp) = self.struct_field_offset(&struct_name, field_name) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: format!("{struct_name}.{field_name}"),
                        span: span.clone(),
                    });
                };
                let field_ty = self
                    .struct_field_type(&struct_name, field_name)
                    .unwrap_or(Type::Int);
                let suffix = type_suffix(&field_ty);
                emit_line!(
                    self,
                    span,
                    "  load.{suffix} %r0, {disp}(%r0)  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::Index { base, index, span } => {
                let idx_ty = self.infer_expr_type(index).unwrap_or(Type::Int);
                let idx_suffix = type_suffix(&idx_ty);
                self.emit_expr(index, Some(idx_ty.clone()))?;
                emit_line!(self, span, "  push.{idx_suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&idx_ty);
                self.emit_expr(base, None)?;
                emit_line!(self, span, "  pop.{idx_suffix} %r1")?;
                self.stack_depth_bytes = self
                    .stack_depth_bytes
                    .saturating_sub(type_size_bytes(&idx_ty));
                emit_line!(self, span, "  muli.w %r1, $8")?;
                emit_line!(self, span, "  add.w %r0, %r1")?;
                emit_line!(
                    self,
                    span,
                    "  load.w %r0, 0(%r0)  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::IndexAssign {
                base,
                index,
                value,
                span,
            } => {
                let value_ty = self.infer_expr_type(value).unwrap_or(Type::Int);
                let idx_ty = self.infer_expr_type(index).unwrap_or(Type::Int);
                let val_suffix = type_suffix(&value_ty);
                let idx_suffix = type_suffix(&idx_ty);
                self.emit_expr(value, Some(value_ty.clone()))?;
                emit_line!(self, span, "  push.{val_suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&value_ty);
                self.emit_expr(index, Some(idx_ty.clone()))?;
                emit_line!(self, span, "  push.{idx_suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&idx_ty);
                self.emit_expr(base, None)?;
                emit_line!(self, span, "  pop.{idx_suffix} %r1")?;
                self.stack_depth_bytes = self
                    .stack_depth_bytes
                    .saturating_sub(type_size_bytes(&idx_ty));
                emit_line!(self, span, "  muli.w %r1, $8")?;
                emit_line!(self, span, "  add.w %r0, %r1")?;
                emit_line!(self, span, "  pop.{val_suffix} %r1")?;
                self.stack_depth_bytes = self
                    .stack_depth_bytes
                    .saturating_sub(type_size_bytes(&value_ty));
                emit_line!(
                    self,
                    span,
                    "  store.{val_suffix} %r1, 0(%r0)  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::FieldAssign {
                base,
                field_name,
                value,
                span,
            } => {
                let struct_name =
                    self.expr_struct_type(base)
                        .ok_or_else(|| BytecodeError::UnknownVariable {
                            name: field_name.clone(),
                            span: span.clone(),
                        })?;
                let field_ty = self
                    .struct_field_type(&struct_name, field_name)
                    .unwrap_or(Type::Int);
                let suffix = type_suffix(&field_ty);
                self.emit_expr(value, Some(field_ty.clone()))?;
                emit_line!(self, span, "  push.{suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&field_ty);
                self.emit_expr(base, None)?;
                let Some(disp) = self.struct_field_offset(&struct_name, field_name) else {
                    return Err(BytecodeError::UnknownVariable {
                        name: format!("{struct_name}.{field_name}"),
                        span: span.clone(),
                    });
                };
                emit_line!(self, span, "  pop.{suffix} %r1")?;
                self.stack_depth_bytes = self
                    .stack_depth_bytes
                    .saturating_sub(type_size_bytes(&field_ty));
                emit_line!(
                    self,
                    span,
                    "  store.{suffix} %r1, {disp}(%r0)  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::CompoundAssign {
                name,
                op,
                value,
                span,
            } => {
                self.emit_expr(value, self.env.get(name).map(|s| s.ty.clone()))?;
                let Some(slot) = self.env.get(name).cloned() else {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                };
                let suffix = type_suffix(&slot.ty);
                emit_line!(self, span, "  push.{suffix} %r0")?;
                self.stack_depth_bytes += type_size_bytes(&slot.ty);

                if self.stack_depth_bytes < slot.depth_bytes {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }
                let load_offset_bytes = self.stack_depth_bytes - slot.depth_bytes;
                let load_disp_bytes = load_offset_bytes as i64;
                emit_line!(
                    self,
                    span,
                    "  load.{suffix} %r0, {}(%sp)  # span {}..{} \"{}\"",
                    load_disp_bytes,
                    span.start,
                    span.end,
                    span.literal
                )?;
                emit_line!(self, span, "  pop.{suffix} %r1")?;
                self.stack_depth_bytes = self
                    .stack_depth_bytes
                    .saturating_sub(type_size_bytes(&slot.ty));

                match op {
                    BinOp::Mod => {
                        emit_line!(
                            self,
                            span,
                            "  divmod.{suffix} %r0, %r1  # span {}..{} \"{}\"",
                            span.start,
                            span.end,
                            span.literal
                        )?;
                        emit_line!(self, span, "  mov %r0, %r1")?;
                    }
                    _ => {
                        let op_instr = match op {
                            BinOp::Add => "add",
                            BinOp::Sub => "sub",
                            BinOp::Mul => "mul",
                            BinOp::Div => "divmod",
                            BinOp::Shl => "shl",
                            BinOp::Shr => "sar",
                            BinOp::BitAnd => "and",
                            BinOp::BitOr => "or",
                            BinOp::BitXor => "xor",
                            _ => unreachable!("compound assign only supports arithmetic ops"),
                        };
                        emit_line!(
                            self,
                            span,
                            "  {op_instr}.{suffix} %r0, %r1  # span {}..{} \"{}\"",
                            span.start,
                            span.end,
                            span.literal
                        )?;
                    }
                }

                if self.stack_depth_bytes < slot.depth_bytes {
                    return Err(BytecodeError::UnknownVariable {
                        name: name.clone(),
                        span: span.clone(),
                    });
                }
                let store_offset_bytes = self.stack_depth_bytes - slot.depth_bytes;
                let store_disp_bytes = store_offset_bytes as i64;
                emit_line!(
                    self,
                    span,
                    "  store.{suffix} %r0, {}(%sp)  # span {}..{} \"{}\"",
                    store_disp_bytes,
                    span.start,
                    span.end,
                    span.literal
                )
            }
            Expr::Call { name, args, span } => {
                let Some(info) = self.function_labels.get(name).cloned() else {
                    return Err(BytecodeError::UnknownFunction {
                        name: name.clone(),
                        span: span.clone(),
                    });
                };
                let extra_stack_args = args.len().saturating_sub(8);
                // Evaluate arguments left-to-right. First eight go into registers, the rest stay on the stack.
                for (idx, arg) in args.iter().enumerate() {
                    let param_ty = info
                        .sig
                        .param_types
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.infer_expr_type(arg).unwrap_or(Type::Int));
                    self.emit_expr(arg, Some(param_ty.clone()))?;
                    if idx < 8 {
                        let suffix = type_suffix(&param_ty);
                        emit_line!(self, span, "  push.{suffix} %r0")?;
                        self.stack_depth_bytes += type_size_bytes(&param_ty);
                        let reg = idx + 1;
                        emit_line!(self, span, "  pop.{suffix} %r{reg}")?;
                        self.stack_depth_bytes = self
                            .stack_depth_bytes
                            .saturating_sub(type_size_bytes(&param_ty));
                    } else {
                        let suffix = type_suffix(&param_ty);
                        emit_line!(self, span, "  push.{suffix} %r0")?;
                        self.stack_depth_bytes += type_size_bytes(&param_ty);
                    }
                }
                if args.len() != info.sig.param_count {
                    return Err(BytecodeError::UnknownFunction {
                        name: format!("{} (arity mismatch)", name),
                        span: span.clone(),
                    });
                }
                emit_line!(
                    self,
                    span,
                    "  call {}  # span {}..{} \"{}\"",
                    info.label,
                    span.start,
                    span.end,
                    span.literal
                )?;
                if extra_stack_args > 0 {
                    for ty in info.sig.param_types.iter().skip(8).rev() {
                        let suffix = type_suffix(ty);
                        emit_line!(self, span, "  pop.{suffix} {SCRATCH_REG}")?;
                        self.stack_depth_bytes =
                            self.stack_depth_bytes.saturating_sub(type_size_bytes(ty));
                    }
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
                        let end_label = self.labels.fresh();
                        self.emit_expr(left, Some(Type::Bool))?;
                        emit_line!(
                            self,
                            span,
                            "  brz.w %r0, {end_label}  # span {}..{} \"{}\"",
                            span.start,
                            span.end,
                            span.literal
                        )?;
                        self.emit_expr(right, Some(Type::Bool))?;
                        // Convert to boolean: if %r0 != 0, result is 1; else 0
                        let _true_label = self.labels.fresh();
                        let after_label = self.labels.fresh();
                        emit_line!(self, span, "  brz.w %r0, {after_label}")?;
                        emit_line!(self, span, "  load.w %r0, $1")?;
                        emit_line!(self, span, "{after_label}:")?;
                        emit_line!(self, span, "{end_label}:")
                    }
                    BinOp::LogicalOr => {
                        // left || right: evaluate left; if true (!= 0), skip right and result is 1; else evaluate right
                        let _true_label = self.labels.fresh();
                        let eval_right_label = self.labels.fresh();
                        let end_label = self.labels.fresh();
                        self.emit_expr(left, Some(Type::Bool))?;
                        emit_line!(
                            self,
                            span,
                            "  brz.w %r0, {eval_right_label}  # span {}..{} \"{}\"",
                            span.start,
                            span.end,
                            span.literal
                        )?;
                        // Left was true, result is 1
                        emit_line!(self, span, "  load.w %r0, $1")?;
                        emit_line!(self, span, "  jmp {end_label}")?;
                        emit_line!(self, span, "{eval_right_label}:")?;
                        self.emit_expr(right, Some(Type::Bool))?;
                        // Convert to boolean: if %r0 != 0, result is 1; else 0
                        let after_label = self.labels.fresh();
                        emit_line!(self, span, "  brz.w %r0, {after_label}")?;
                        emit_line!(self, span, "  load.w %r0, $1")?;
                        emit_line!(self, span, "{after_label}:")?;
                        emit_line!(self, span, "{end_label}:")
                    }
                    _ => {
                        // For all other binary operators, evaluate right first, then left
                        let bin_ty = self.infer_expr_type(expr).unwrap_or(Type::Int);
                        let suffix = type_suffix(&bin_ty);
                        let bin_size = type_size_bytes(&bin_ty);
                        self.emit_expr(right, Some(bin_ty.clone()))?;
                        emit_line!(self, span, "  push.{suffix} %r0")?;
                        self.stack_depth_bytes += bin_size;
                        self.emit_expr(left, Some(bin_ty.clone()))?;
                        emit_line!(self, span, "  pop.{suffix} %r1")?;
                        self.stack_depth_bytes = self.stack_depth_bytes.saturating_sub(bin_size);

                        let op_instr = match op {
                            BinOp::Add => "add",
                            BinOp::Sub => "sub",
                            BinOp::Mul => "mul",
                            BinOp::Div | BinOp::Mod => "divmod",
                            BinOp::Shl => "shl",
                            BinOp::Shr => "sar",
                            BinOp::BitAnd => "and",
                            BinOp::BitOr => "or",
                            BinOp::BitXor => "xor",
                            BinOp::Eq => "cmpeq",
                            BinOp::Ne => "cmpne",
                            BinOp::Lt => "cmplt",
                            BinOp::Gt => "cmplt",
                            BinOp::Le => "cmplt",
                            BinOp::Ge => "cmplt",
                            _ => unreachable!(),
                        };
                        match op {
                            BinOp::Gt => {
                                // a > b -> cmplt.w b,a ; result in %r1 -> move to %r0
                                emit_line!(
                                    self,
                                    span,
                                    "  {op_instr}.{suffix} %r1, %r0  # span {}..{} \"{}\"",
                                    span.start,
                                    span.end,
                                    span.literal
                                )?;
                                // move result back to r0
                                emit_line!(self, span, "  push.{suffix} %r1")?;
                                self.stack_depth_bytes += bin_size;
                                emit_line!(self, span, "  pop.{suffix} %r0")?;
                                self.stack_depth_bytes =
                                    self.stack_depth_bytes.saturating_sub(bin_size);
                                Ok(())
                            }
                            BinOp::Le => {
                                // a <= b -> not (b < a)
                                emit_line!(
                                    self,
                                    span,
                                    "  {op_instr}.{suffix} %r1, %r0  # span {}..{} \"{}\"",
                                    span.start,
                                    span.end,
                                    span.literal
                                )?;
                                emit_line!(self, span, "  not.{suffix} %r1")?;
                                emit_line!(self, span, "  push.{suffix} %r1")?;
                                self.stack_depth_bytes += bin_size;
                                emit_line!(self, span, "  pop.{suffix} %r0")?;
                                self.stack_depth_bytes =
                                    self.stack_depth_bytes.saturating_sub(bin_size);
                                Ok(())
                            }
                            BinOp::Ge => {
                                // a >= b -> not (a < b)
                                emit_line!(
                                    self,
                                    span,
                                    "  {op_instr}.{suffix} %r0, %r1  # span {}..{} \"{}\"",
                                    span.start,
                                    span.end,
                                    span.literal
                                )?;
                                emit_line!(self, span, "  not.{suffix} %r0")
                            }
                            BinOp::Mod => {
                                // divmod leaves the remainder in %r1; move it into %r0 for the result.
                                emit_line!(
                                    self,
                                    span,
                                    "  {op_instr}.{suffix} %r0, %r1  # span {}..{} \"{}\"",
                                    span.start,
                                    span.end,
                                    span.literal
                                )?;
                                emit_line!(self, span, "  mov %r0, %r1")
                            }
                            _ => emit_line!(
                                self,
                                span,
                                "  {op_instr}.{suffix} %r0, %r1  # span {}..{} \"{}\"",
                                span.start,
                                span.end,
                                span.literal
                            ),
                        }
                    }
                }
            }
            Expr::QualifiedCall {
                module,
                name,
                args,
                span,
            } => {
                // Generate code for qualified function call: module::function(args)
                // Look up the mangled function name in the &self.function_labels map
                let mangled_name = format!("{}_{}", module, name);
                let Some(info) = self.function_labels.get(&mangled_name).cloned() else {
                    return Err(BytecodeError::UnknownFunction {
                        name: format!("{}::{}", module, name),
                        span: span.clone(),
                    });
                };

                // Validate argument count
                if args.len() != info.sig.param_count {
                    return Err(BytecodeError::UnknownFunction {
                        name: format!(
                            "{}::{} expects {} arguments, got {}",
                            module,
                            name,
                            info.sig.param_count,
                            args.len()
                        ),
                        span: span.clone(),
                    });
                }

                let extra_stack_args = args.len().saturating_sub(8);

                // Evaluate arguments left-to-right. First eight go into registers, the rest stay on the stack.
                for (idx, arg) in args.iter().enumerate() {
                    let param_ty = info
                        .sig
                        .param_types
                        .get(idx)
                        .cloned()
                        .unwrap_or_else(|| self.infer_expr_type(arg).unwrap_or(Type::Int));
                    let suffix = type_suffix(&param_ty);
                    self.emit_expr(arg, Some(param_ty.clone()))?;
                    if idx < 8 {
                        emit_line!(self, span, "  push.{suffix} %r0")?;
                        self.stack_depth_bytes += type_size_bytes(&param_ty);
                        let reg = idx + 1;
                        emit_line!(self, span, "  pop.{suffix} %r{reg}")?;
                        self.stack_depth_bytes = self
                            .stack_depth_bytes
                            .saturating_sub(type_size_bytes(&param_ty));
                    } else {
                        emit_line!(self, span, "  push.{suffix} %r0")?;
                        self.stack_depth_bytes += type_size_bytes(&param_ty);
                    }
                }

                emit_line!(
                    self,
                    span,
                    "  call {}  # span {}..{} \"{}\"",
                    info.label,
                    span.start,
                    span.end,
                    span.literal
                )?;

                if extra_stack_args > 0 {
                    for ty in info.sig.param_types.iter().skip(8).rev() {
                        let suffix = type_suffix(ty);
                        emit_line!(self, span, "  pop.{suffix} {SCRATCH_REG}")?;
                        self.stack_depth_bytes =
                            self.stack_depth_bytes.saturating_sub(type_size_bytes(ty));
                    }
                }

                Ok(())
            }
            Expr::QualifiedStructLiteral {
                module,
                struct_name,
                field_values,
                span,
            } => {
                let qualified_name = format!("{}::{}", module, struct_name);
                let (layout, _struct_key) =
                    if let Some(layout) = self.struct_layouts.get(&qualified_name).cloned() {
                        (layout, qualified_name)
                    } else if let Some(layout) = self.struct_layouts.get(struct_name).cloned() {
                        (layout, struct_name.to_string())
                    } else {
                        return Err(BytecodeError::UnknownFunction {
                            name: qualified_name.clone(),
                            span: span.clone(),
                        });
                    };

                let bytes = layout.size as i64;

                emit_line!(
                    self,
                    span,
                    "  mov %r1, {HEAP_REG}  # span {}..{} \"{}\"",
                    span.start,
                    span.end,
                    span.literal
                )?;
                emit_line!(self, span, "  mov.w %r2, {HEAP_REG}")?;
                emit_line!(self, span, "  load.w %r0, ${bytes}")?;
                emit_line!(self, span, "  add.w %r2, %r0")?;
                emit_line!(self, span, "  movi %r0, $12")?;
                emit_line!(self, span, "  mov.w %r1, %r2")?;
                emit_line!(self, span, "  trap")?;
                emit_line!(self, span, "  mov.w {HEAP_REG}, %r2")?;

                for field in &layout.fields {
                    let field_value = field_values
                        .iter()
                        .find(|(fname, _)| fname == &field.name)
                        .map(|(_, v)| v)
                        .ok_or_else(|| BytecodeError::UnknownFunction {
                            name: format!("missing field {}", field.name),
                            span: span.clone(),
                        })?;

                    let offset = field.offset as i64;
                    let suffix = type_suffix(&field.ty);
                    emit_line!(self, span, "  push.w %r1")?;
                    self.stack_depth_bytes += PTR_SIZE;
                    self.emit_expr(field_value, Some(field.ty.clone()))?;
                    emit_line!(self, span, "  store.{suffix} %r0, {offset}(%r1)")?;
                    emit_line!(self, span, "  pop.w %r1")?;
                    self.stack_depth_bytes = self.stack_depth_bytes.saturating_sub(PTR_SIZE);
                }

                emit_line!(self, span, "  mov.w %r0, %r1")?;

                Ok(())
            }
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
        let program =
            crate::pagoda::parse_source("fn add(a: i64, b: i64) { a + b }; { add(2,3) }").unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call_with_args", output);
    }

    #[test]
    fn emits_function_call_with_stack_args() {
        let program = crate::pagoda::parse_source(
            "fn sum(a: i64, b: i64, c: i64, d: i64, e: i64, f: i64, g: i64, h: i64, i: i64, j: i64) { a+b+c+d+e+f+g+h+i+j }; { sum(1,2,3,4,5,6,7,8,9,10) }",
        )
        .unwrap();
        let mut buffer = Vec::new();
        emit_exit_program(&program, &mut buffer).unwrap();
        let output = String::from_utf8(buffer).unwrap();

        assert_snapshot!("emits_function_call_with_stack_args", output);
    }
}
