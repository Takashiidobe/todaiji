pub mod bytecode;
pub mod parser;
pub mod semantics;
pub mod tokenizer;

use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub literal: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Program {
    pub imports: Vec<Import>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Import {
    pub module_name: String,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub is_public: bool,
    pub name: String,
    pub fields: Vec<StructField>,
    pub is_tuple_struct: bool,  // true if defined as struct Name(T1, T2, ...)
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: String, // For now, only "i64"
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub is_public: bool,
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub data: Option<String>, // Optional associated data type
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionParam {
    pub name: String,
    pub ty: Option<String>, // Type annotation is now optional for inference
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub is_public: bool,
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub return_type: Option<String>,
    pub body: Stmt,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedExpr {
    pub expr: Expr,
    pub ty: semantics::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedProgram {
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
    pub functions: Vec<CheckedFunction>,
    pub stmts: Vec<CheckedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedFunction {
    pub name: String,
    pub params: Vec<FunctionParam>,
    pub body: CheckedStmt,
    pub span: Span,
    pub return_type: semantics::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedStmt {
    pub stmt: Stmt,
    pub ty: semantics::Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr {
        expr: Expr,
        span: Span,
    },
    Empty {
        span: Span,
    },
    Let {
        name: String,
        ty: Option<String>,
        expr: Expr,
        span: Span,
    },
    Return {
        expr: Expr,
        span: Span,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        post: Option<Expr>,
        body: Box<Stmt>,
        span: Span,
    },
    Block {
        stmts: Vec<Stmt>,
        span: Span,
    },
    If {
        cond: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
}

impl Stmt {
    pub fn span(&self) -> &Span {
        match self {
            Stmt::Expr { span, .. } => span,
            Stmt::Empty { span } => span,
            Stmt::Let { span, .. } => span,
            Stmt::Return { span, .. } => span,
            Stmt::For { span, .. } => span,
            Stmt::Block { span, .. } => span,
            Stmt::If { span, .. } => span,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    IntLiteral {
        value: i64,
        span: Span,
    },
    BoolLiteral {
        value: bool,
        span: Span,
    },
    Var {
        name: String,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    Unary {
        op: parser::UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: parser::BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Assign {
        name: String,
        value: Box<Expr>,
        span: Span,
    },
    CompoundAssign {
        name: String,
        op: parser::BinOp,
        value: Box<Expr>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    ArrayLiteral {
        elements: Vec<Expr>,
        span: Span,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    IndexAssign {
        base: Box<Expr>,
        index: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    StructLiteral {
        struct_name: String,
        field_values: Vec<(String, Expr)>, // (field_name, value)
        span: Span,
    },
    FieldAccess {
        base: Box<Expr>,
        field_name: String,
        span: Span,
    },
    FieldAssign {
        base: Box<Expr>,
        field_name: String,
        value: Box<Expr>,
        span: Span,
    },
    QualifiedCall {
        module: String,
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    QualifiedStructLiteral {
        module: String,
        struct_name: String,
        field_values: Vec<(String, Expr)>,
        span: Span,
    },
    EnumLiteral {
        enum_name: String,
        variant_name: String,
        data: Option<Box<Expr>>, // Optional associated data
        span: Span,
    },
    QualifiedEnumLiteral {
        module: String,
        enum_name: String,
        variant_name: String,
        data: Option<Box<Expr>>,
        span: Span,
    },
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        span: Span,
    },
    TupleLiteral {
        elements: Vec<Expr>,
        span: Span,
    },
    TupleIndex {
        tuple: Box<Expr>,
        index: usize,
        span: Span,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Variant {
        enum_name: Option<String>, // None for unqualified patterns
        variant_name: String,
        binding: Option<String>, // Variable to bind the data to
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::IntLiteral { span, .. } => span,
            Expr::BoolLiteral { span, .. } => span,
            Expr::StringLiteral { span, .. } => span,
            Expr::Var { span, .. } => span,
            Expr::Unary { span, .. } => span,
            Expr::Binary { span, .. } => span,
            Expr::Assign { span, .. } => span,
            Expr::CompoundAssign { span, .. } => span,
            Expr::Call { span, .. } => span,
            Expr::ArrayLiteral { span, .. } => span,
            Expr::Index { span, .. } => span,
            Expr::IndexAssign { span, .. } => span,
            Expr::StructLiteral { span, .. } => span,
            Expr::FieldAccess { span, .. } => span,
            Expr::FieldAssign { span, .. } => span,
            Expr::QualifiedCall { span, .. } => span,
            Expr::QualifiedStructLiteral { span, .. } => span,
            Expr::EnumLiteral { span, .. } => span,
            Expr::QualifiedEnumLiteral { span, .. } => span,
            Expr::Match { span, .. } => span,
            Expr::TupleLiteral { span, .. } => span,
            Expr::TupleIndex { span, .. } => span,
        }
    }
}

impl Pattern {
    pub fn span(&self) -> &Span {
        match self {
            Pattern::Variant { span, .. } => span,
        }
    }
}

#[derive(Debug, Error)]
pub enum FrontendError {
    #[error("{0}")]
    Tokenize(#[from] tokenizer::TokenizeError),
    #[error("{0}")]
    Parse(#[from] parser::ParseError),
    #[error("{0}")]
    Semantic(#[from] semantics::SemanticError),
    #[error("{0}")]
    Bytecode(#[from] bytecode::BytecodeError),
}

/// Convenience wrapper: tokenize and parse a source string into a `Program`.
pub fn parse_source(source: &str) -> Result<CheckedProgram, FrontendError> {
    let tokens = tokenizer::tokenize(source)?;
    let parsed = parser::parse_program(&tokens).map_err(FrontendError::from)?;
    let checked = semantics::analyze_program(parsed).map_err(FrontendError::from)?;
    Ok(checked)
}

/// Parse a source file with module support, merging all imported modules.
pub fn parse_source_with_modules(path: &std::path::Path) -> Result<CheckedProgram, FrontendError> {
    use std::fs;

    let source = fs::read_to_string(path).map_err(|e| {
        FrontendError::Semantic(semantics::SemanticError::ModuleNotFound {
            name: format!("Failed to read {}: {}", path.display(), e),
            span: Span {
                start: 0,
                end: 0,
                literal: String::new(),
            },
        })
    })?;

    let tokens = tokenizer::tokenize(&source)?;
    let parsed = parser::parse_program(&tokens).map_err(FrontendError::from)?;

    // If there are no imports, use the simple path
    if parsed.imports.is_empty() {
        let checked = semantics::analyze_program(parsed).map_err(FrontendError::from)?;
        return Ok(checked);
    }

    // Get the directory containing the source file
    let base_dir = path.parent().unwrap_or_else(|| std::path::Path::new("."));

    // Analyze with modules
    let modules = semantics::analyze_program_with_modules(parsed.clone(), base_dir)?;

    // Find the main program (it should be in the modules map with an empty key or special marker)
    // Actually, analyze_program_with_modules returns modules but not the main program's checked version
    // Let me look at what it returns...
    // For now, let's merge all modules into a single CheckedProgram

    let main_checked = semantics::analyze_program_with_imports(&parsed, &modules)?;
    let merged = merge_modules(main_checked, &modules);

    Ok(merged)
}

/// Merge imported modules into the main program for code generation.
fn merge_modules(
    main: CheckedProgram,
    modules: &std::collections::HashMap<String, semantics::Module>,
) -> CheckedProgram {
    let mut result = main;

    // Add all public functions from imported modules with name mangling
    for (module_name, module) in modules {
        for func in &module.checked_program.functions {
            // Only include public functions
            if module.public_functions.contains_key(&func.name) {
                // Clone and rename the function
                let mut module_func = func.clone();
                module_func.name = format!("{}_{}", module_name, func.name);
                result.functions.push(module_func);
            }
        }

        // Add all public structs from imported modules with qualified naming
        for struct_def in &module.checked_program.structs {
            if module.public_structs.contains_key(&struct_def.name) {
                let mut module_struct = struct_def.clone();
                module_struct.name = format!("{}::{}", module_name, struct_def.name);
                result.structs.push(module_struct);
            }
        }

        // Add all public enums from imported modules with qualified naming
        for enum_def in &module.checked_program.enums {
            if module.public_enums.contains_key(&enum_def.name) {
                let mut module_enum = enum_def.clone();
                module_enum.name = format!("{}::{}", module_name, enum_def.name);
                result.enums.push(module_enum);
            }
        }
    }

    result
}

/// Render a human-friendly error with source context and caret pointing to the span.
pub fn format_error(source: &str, err: &FrontendError) -> String {
    let (span, message) = match err {
        FrontendError::Tokenize(token_err) => match token_err {
            tokenizer::TokenizeError::UnexpectedChar {
                span_start,
                span_end,
                ..
            } => (
                Span {
                    start: *span_start,
                    end: *span_end,
                    literal: source
                        .get(*span_start..*span_end)
                        .unwrap_or_default()
                        .to_string(),
                },
                token_err.to_string(),
            ),
            tokenizer::TokenizeError::InvalidInt {
                span_start,
                span_end,
            } => (
                Span {
                    start: *span_start,
                    end: *span_end,
                    literal: source
                        .get(*span_start..*span_end)
                        .unwrap_or_default()
                        .to_string(),
                },
                token_err.to_string(),
            ),
            tokenizer::TokenizeError::UnterminatedString {
                span_start,
                span_end,
            } => (
                Span {
                    start: *span_start,
                    end: *span_end,
                    literal: String::new(),
                },
                token_err.to_string(),
            ),
        },
        FrontendError::Parse(parse_err) => {
            use parser::ParseError;
            match parse_err {
                ParseError::UnexpectedEof {
                    span_start,
                    span_end,
                } => (
                    Span {
                        start: *span_start,
                        end: *span_end,
                        literal: String::new(),
                    },
                    parse_err.to_string(),
                ),
                ParseError::ExpectedInt {
                    span_start,
                    span_end,
                    ..
                }
                | ParseError::ExpectedStatement {
                    span_start,
                    span_end,
                }
                | ParseError::ExpectedIdent {
                    span_start,
                    span_end,
                    ..
                }
                | ParseError::ExpectedEquals {
                    span_start,
                    span_end,
                    ..
                }
                | ParseError::TrailingTokens {
                    span_start,
                    span_end,
                    ..
                } => (
                    Span {
                        start: *span_start,
                        end: *span_end,
                        literal: source
                            .get(*span_start..*span_end)
                            .unwrap_or_default()
                            .to_string(),
                    },
                    parse_err.to_string(),
                ),
            }
        }
        FrontendError::Semantic(sem_err) => (sem_err.span().clone(), sem_err.to_string()),
        FrontendError::Bytecode(bc_err) => (bc_err.span().clone(), bc_err.to_string()),
    };

    render_snippet(source, &span, &message)
}

fn render_snippet(source: &str, span: &Span, message: &str) -> String {
    let (line_idx, col_idx, line_text) = line_and_col(source, span.start);
    let line_num = line_idx + 1;
    let col_num = col_idx + 1;

    let caret_len = usize::max(1, span.end.saturating_sub(span.start));
    let underline_start = col_idx;
    let mut underline = String::new();
    for _ in 0..underline_start {
        underline.push(' ');
    }
    for _ in 0..caret_len {
        underline.push('^');
    }

    let gutter_width = format!("{}", line_num).len();

    let mut out = String::new();
    out.push_str(&format!("{line_num}:{col_num}: {message}\n"));
    out.push_str(&format!(
        "{:>width$} | {}\n",
        line_num,
        line_text,
        width = gutter_width
    ));
    out.push_str(&format!(
        "{:>width$} | {}\n",
        "",
        underline,
        width = gutter_width
    ));
    out.push_str(&format!("{:>width$} | {message}", "", width = gutter_width));
    out
}

fn line_and_col(source: &str, byte_index: usize) -> (usize, usize, String) {
    let mut line_start = 0;
    let mut line_idx = 0;
    for (idx, ch) in source.char_indices() {
        if idx >= byte_index {
            break;
        }
        if ch == '\n' {
            line_idx += 1;
            line_start = idx + ch.len_utf8();
        }
    }
    let line_end = source[line_start..]
        .find('\n')
        .map(|rel| line_start + rel)
        .unwrap_or_else(|| source.len());
    let line_text = source[line_start..line_end].to_string();
    let col_idx = source[line_start..byte_index].chars().count();
    (line_idx, col_idx, line_text)
}
