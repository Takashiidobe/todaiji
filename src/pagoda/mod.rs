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
    pub structs: Vec<StructDef>,
    pub functions: Vec<Function>,
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructField>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: String, // For now, only "i64"
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
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
    pub functions: Vec<CheckedFunction>,
    pub stmts: Vec<CheckedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckedFunction {
    pub name: String,
    pub params: Vec<String>,
    pub body: CheckedStmt,
    pub span: Span,
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
}

impl Expr {
    pub fn span(&self) -> &Span {
        match self {
            Expr::IntLiteral { span, .. } => span,
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
