use thiserror::Error;

use crate::pagoda::{Expr, Program, Span};

use super::tokenizer::{Token, TokenKind};

#[derive(Debug, Error, PartialEq, Eq)]
pub enum ParseError {
    #[error("unexpected end of input at span {span_start}..{span_end}")]
    UnexpectedEof { span_start: usize, span_end: usize },
    #[error("expected integer literal, found {found:?} at bytes {span_start}..{span_end}")]
    ExpectedInt {
        span_start: usize,
        span_end: usize,
        found: TokenKind,
    },
    #[error("expected identifier, found {found:?} at bytes {span_start}..{span_end}")]
    ExpectedIdent {
        span_start: usize,
        span_end: usize,
        found: TokenKind,
    },
    #[error("expected '=', found {found:?} at bytes {span_start}..{span_end}")]
    ExpectedEquals {
        span_start: usize,
        span_end: usize,
        found: TokenKind,
    },
    #[error("expected statement starting at bytes {span_start}..{span_end}")]
    ExpectedStatement { span_start: usize, span_end: usize },
    #[error("trailing input {found:?} starting at bytes {span_start}..{span_end}")]
    TrailingTokens {
        span_start: usize,
        span_end: usize,
        found: TokenKind,
    },
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    cursor: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    BitAnd,
    BitOr,
    BitXor,
    LogicalAnd,
    LogicalOr,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    Plus,
    Minus,
    BitNot,
    LogicalNot,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self { tokens, cursor: 0 }
    }

    fn parse_for(&mut self) -> Result<crate::pagoda::Stmt, ParseError> {
        let tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;
        let open_paren = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: tok.span.start,
                span_end: tok.span.end,
            })?;
        if !matches!(open_paren.kind, TokenKind::LParen) {
            return Err(ParseError::TrailingTokens {
                span_start: open_paren.span.start,
                span_end: open_paren.span.end,
                found: open_paren.kind.clone(),
            });
        }
        self.cursor += 1;
        let init = if matches!(
            self.tokens.get(self.cursor).map(|t| &t.kind),
            Some(TokenKind::Semicolon)
        ) {
            self.cursor += 1;
            None
        } else if matches!(
            self.tokens.get(self.cursor).map(|t| &t.kind),
            Some(TokenKind::Let)
        ) {
            let let_tok = self.tokens.get(self.cursor).unwrap().clone();
            self.cursor += 1;
            let ident = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: let_tok.span.start,
                    span_end: let_tok.span.end,
                })?;
            let name = match &ident.kind {
                TokenKind::Ident(name) => name.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: ident.span.start,
                        span_end: ident.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;
            let ty = if let Some(colon_tok) = self.tokens.get(self.cursor) {
                if matches!(colon_tok.kind, TokenKind::Colon) {
                    self.cursor += 1;
                    let type_tok =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: colon_tok.span.start,
                                span_end: colon_tok.span.end,
                            })?;
                    let ty = match &type_tok.kind {
                        TokenKind::Ident(s) => s.clone(),
                        other => {
                            return Err(ParseError::ExpectedIdent {
                                span_start: type_tok.span.start,
                                span_end: type_tok.span.end,
                                found: other.clone(),
                            });
                        }
                    };
                    self.cursor += 1;
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };
            let eq = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: ident.span.start,
                    span_end: ident.span.end,
                })?;
            if !matches!(eq.kind, TokenKind::Assign) {
                return Err(ParseError::ExpectedEquals {
                    span_start: eq.span.start,
                    span_end: eq.span.end,
                    found: eq.kind.clone(),
                });
            }
            self.cursor += 1;
            let expr = self.parse_expr()?;
            let span = Span {
                start: let_tok.span.start,
                end: expr.span().end,
                literal: format!(
                    "let {}{} = {}",
                    name,
                    ty.as_ref().map(|t| format!(":{}", t)).unwrap_or_default(),
                    expr.span().literal
                ),
            };
            let semi = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: span.start,
                    span_end: span.end,
                })?;
            if !matches!(semi.kind, TokenKind::Semicolon) {
                return Err(ParseError::TrailingTokens {
                    span_start: semi.span.start,
                    span_end: semi.span.end,
                    found: semi.kind.clone(),
                });
            }
            self.cursor += 1;
            Some(Box::new(crate::pagoda::Stmt::Let {
                name,
                ty,
                expr,
                span,
            }))
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span().clone();
            let semi = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: span.start,
                    span_end: span.end,
                })?;
            if !matches!(semi.kind, TokenKind::Semicolon) {
                return Err(ParseError::TrailingTokens {
                    span_start: semi.span.start,
                    span_end: semi.span.end,
                    found: semi.kind.clone(),
                });
            }
            self.cursor += 1;
            Some(Box::new(crate::pagoda::Stmt::Expr { expr, span }))
        };
        let cond = if matches!(
            self.tokens.get(self.cursor).map(|t| &t.kind),
            Some(TokenKind::Semicolon)
        ) {
            self.cursor += 1;
            None
        } else {
            let expr = self.parse_expr()?;
            let semi = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: expr.span().start,
                    span_end: expr.span().end,
                })?;
            if !matches!(semi.kind, TokenKind::Semicolon) {
                return Err(ParseError::TrailingTokens {
                    span_start: semi.span.start,
                    span_end: semi.span.end,
                    found: semi.kind.clone(),
                });
            }
            self.cursor += 1;
            Some(expr)
        };
        let post = if matches!(
            self.tokens.get(self.cursor).map(|t| &t.kind),
            Some(TokenKind::RParen)
        ) {
            self.cursor += 1;
            None
        } else {
            let expr = self.parse_expr()?;
            let close = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: expr.span().start,
                    span_end: expr.span().end,
                })?;
            if !matches!(close.kind, TokenKind::RParen) {
                return Err(ParseError::TrailingTokens {
                    span_start: close.span.start,
                    span_end: close.span.end,
                    found: close.kind.clone(),
                });
            }
            self.cursor += 1;
            Some(expr)
        };
        let body = self.parse_block()?;
        let span = Span {
            start: tok.span.start,
            end: body.span().end,
            literal: tok.span.literal.clone() + &body.span().literal,
        };
        Ok(crate::pagoda::Stmt::For {
            init,
            cond,
            post,
            body: Box::new(body),
            span,
        })
    }

    fn parse_if(&mut self) -> Result<crate::pagoda::Stmt, ParseError> {
        let tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;
        let open_paren = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: tok.span.start,
                span_end: tok.span.end,
            })?;
        if !matches!(open_paren.kind, TokenKind::LParen) {
            return Err(ParseError::TrailingTokens {
                span_start: open_paren.span.start,
                span_end: open_paren.span.end,
                found: open_paren.kind.clone(),
            });
        }
        self.cursor += 1;
        let cond = self.parse_expr()?;
        let close_paren = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: tok.span.start,
                span_end: tok.span.end,
            })?;
        if !matches!(close_paren.kind, TokenKind::RParen) {
            return Err(ParseError::TrailingTokens {
                span_start: close_paren.span.start,
                span_end: close_paren.span.end,
                found: close_paren.kind.clone(),
            });
        }
        self.cursor += 1;
        let then_block = self.parse_block()?;
        let mut else_block = None;
        if let Some(next) = self.tokens.get(self.cursor)
            && matches!(next.kind, TokenKind::Else)
        {
            self.cursor += 1;
            if matches!(
                self.tokens.get(self.cursor).map(|t| &t.kind),
                Some(TokenKind::If)
            ) {
                let nested_if = self.parse_if()?;
                else_block = Some(Box::new(nested_if));
            } else {
                let block = self.parse_block()?;
                else_block = Some(Box::new(block));
            }
        }
        let span = Span {
            start: tok.span.start,
            end: else_block
                .as_ref()
                .map(|b| b.span().end)
                .unwrap_or(then_block.span().end),
            literal: format!(
                "{}({}){}{}",
                tok.span.literal,
                cond.span().literal,
                then_block.span().literal,
                else_block
                    .as_ref()
                    .map(|b| b.span().literal.clone())
                    .unwrap_or_default()
            ),
        };
        Ok(crate::pagoda::Stmt::If {
            cond,
            then_branch: Box::new(then_block),
            else_branch: else_block,
            span,
        })
    }

    fn parse_import(&mut self) -> Result<crate::pagoda::Import, ParseError> {
        use crate::pagoda::Import;

        let import_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;

        if !matches!(import_tok.kind, TokenKind::Import) {
            return Err(ParseError::TrailingTokens {
                span_start: import_tok.span.start,
                span_end: import_tok.span.end,
                found: import_tok.kind.clone(),
            });
        }
        self.cursor += 1;

        let name_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: import_tok.span.end,
                span_end: import_tok.span.end,
            })?;

        let module_name = match &name_tok.kind {
            TokenKind::Ident(name) => name.clone(),
            _ => {
                return Err(ParseError::ExpectedIdent {
                    span_start: name_tok.span.start,
                    span_end: name_tok.span.end,
                    found: name_tok.kind.clone(),
                });
            }
        };
        self.cursor += 1;

        let semi_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: name_tok.span.end,
                span_end: name_tok.span.end,
            })?;

        if !matches!(semi_tok.kind, TokenKind::Semicolon) {
            return Err(ParseError::TrailingTokens {
                span_start: semi_tok.span.start,
                span_end: semi_tok.span.end,
                found: semi_tok.kind.clone(),
            });
        }
        self.cursor += 1;

        let literal = format!("import {};", module_name);
        Ok(Import {
            module_name,
            span: crate::pagoda::Span {
                start: import_tok.span.start,
                end: semi_tok.span.end,
                literal,
            },
        })
    }

    fn parse_program_impl(&mut self) -> Result<Program, ParseError> {
        let mut imports = Vec::new();
        let mut structs = Vec::new();
        let mut enums = Vec::new();
        let mut functions = Vec::new();
        let mut stmts = Vec::new();
        loop {
            let Some(token) = self.tokens.get(self.cursor) else {
                return Err(ParseError::UnexpectedEof {
                    span_start: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                    span_end: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                });
            };
            if !matches!(token.kind, TokenKind::Import) {
                break;
            }
            imports.push(self.parse_import()?);
        }
        loop {
            let Some(token) = self.tokens.get(self.cursor) else {
                return Err(ParseError::UnexpectedEof {
                    span_start: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                    span_end: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                });
            };
            let is_public = matches!(token.kind, TokenKind::Pub);
            if is_public {
                self.cursor += 1;
            }
            let Some(token) = self.tokens.get(self.cursor) else {
                return Err(ParseError::UnexpectedEof {
                    span_start: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                    span_end: self.tokens.last().map(|t| t.span.end).unwrap_or(0),
                });
            };
            if matches!(token.kind, TokenKind::Struct) {
                let mut struct_def = self.parse_struct()?;
                struct_def.is_public = is_public;
                structs.push(struct_def);
                if let Some(next) = self.tokens.get(self.cursor) {
                    if matches!(next.kind, TokenKind::Semicolon) {
                        self.cursor += 1;
                    } else if matches!(next.kind, TokenKind::Eof) {
                        break;
                    }
                } else {
                    break;
                }
            } else if matches!(token.kind, TokenKind::Enum) {
                let mut enum_def = self.parse_enum()?;
                enum_def.is_public = is_public;
                enums.push(enum_def);
                if let Some(next) = self.tokens.get(self.cursor) {
                    if matches!(next.kind, TokenKind::Semicolon) {
                        self.cursor += 1;
                    } else if matches!(next.kind, TokenKind::Eof) {
                        break;
                    }
                } else {
                    break;
                }
            } else if matches!(token.kind, TokenKind::Fn) {
                let mut func = self.parse_function()?;
                func.is_public = is_public;
                functions.push(func);
                if let Some(next) = self.tokens.get(self.cursor) {
                    if matches!(next.kind, TokenKind::Semicolon) {
                        self.cursor += 1;
                    } else if matches!(next.kind, TokenKind::Eof) {
                        break;
                    }
                } else {
                    break;
                }
            } else {
                if is_public {
                    return Err(ParseError::TrailingTokens {
                        span_start: token.span.start,
                        span_end: token.span.end,
                        found: token.kind.clone(),
                    });
                }
                let stmt = self.parse_block()?;
                stmts.push(stmt);
                if let Some(token) = self.tokens.get(self.cursor) {
                    if matches!(token.kind, TokenKind::Semicolon) {
                        self.cursor += 1;
                        if matches!(
                            self.tokens.get(self.cursor).map(|t| &t.kind),
                            Some(TokenKind::Eof)
                        ) {
                            break;
                        }
                    } else if matches!(token.kind, TokenKind::Eof) {
                        break;
                    } else {
                        return Err(ParseError::TrailingTokens {
                            span_start: token.span.start,
                            span_end: token.span.end,
                            found: token.kind.clone(),
                        });
                    }
                } else {
                    break;
                }
            }
            if matches!(
                self.tokens.get(self.cursor).map(|t| &t.kind),
                Some(TokenKind::Eof)
            ) {
                break;
            }
        }
        let program_start = stmts
            .first()
            .map(|stmt| stmt.span().start)
            .or_else(|| functions.first().map(|f| f.span.start))
            .or_else(|| structs.first().map(|s| s.span.start))
            .unwrap_or_else(|| self.tokens.first().map(|t| t.span.start).unwrap_or(0));
        let program_end = self
            .tokens
            .last()
            .map(|t| t.span.end)
            .unwrap_or(program_start);
        Ok(Program {
            imports,
            structs,
            enums,
            functions,
            stmts,
            span: Span {
                start: program_start,
                end: program_end,
                literal: String::new(),
            },
        })
    }

    fn parse_struct(&mut self) -> Result<crate::pagoda::StructDef, ParseError> {
        use crate::pagoda::{StructDef, StructField};

        let struct_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;

        let name_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: struct_tok.span.start,
                span_end: struct_tok.span.end,
            })?;
        let name = match &name_tok.kind {
            TokenKind::Ident(s) => s.clone(),
            other => {
                return Err(ParseError::ExpectedIdent {
                    span_start: name_tok.span.start,
                    span_end: name_tok.span.end,
                    found: other.clone(),
                });
            }
        };
        self.cursor += 1;

        let lbrace = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: name_tok.span.start,
                span_end: name_tok.span.end,
            })?;
        if !matches!(lbrace.kind, TokenKind::LBrace) {
            return Err(ParseError::TrailingTokens {
                span_start: lbrace.span.start,
                span_end: lbrace.span.end,
                found: lbrace.kind.clone(),
            });
        }
        self.cursor += 1;

        let mut fields = Vec::new();
        loop {
            let tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lbrace.span.start,
                    span_end: lbrace.span.end,
                })?;
            if matches!(tok.kind, TokenKind::RBrace) {
                self.cursor += 1;
                break;
            }

            // Parse field: name: type
            let field_name_tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lbrace.span.start,
                    span_end: lbrace.span.end,
                })?;
            let field_name = match &field_name_tok.kind {
                TokenKind::Ident(s) => s.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: field_name_tok.span.start,
                        span_end: field_name_tok.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;

            // Expect colon
            let colon_tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: field_name_tok.span.start,
                    span_end: field_name_tok.span.end,
                })?;
            if !matches!(colon_tok.kind, TokenKind::Colon) {
                return Err(ParseError::TrailingTokens {
                    span_start: colon_tok.span.start,
                    span_end: colon_tok.span.end,
                    found: colon_tok.kind.clone(),
                });
            }
            self.cursor += 1;

            // Parse type (for now, only i64)
            let type_tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: colon_tok.span.start,
                    span_end: colon_tok.span.end,
                })?;
            let field_type = match &type_tok.kind {
                TokenKind::Ident(s) => s.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: type_tok.span.start,
                        span_end: type_tok.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;

            fields.push(StructField {
                name: field_name,
                ty: field_type,
                span: Span {
                    start: field_name_tok.span.start,
                    end: type_tok.span.end,
                    literal: String::new(),
                },
            });

            // Optional comma
            if let Some(next) = self.tokens.get(self.cursor)
                && matches!(next.kind, TokenKind::Comma)
            {
                self.cursor += 1;
            }
        }

        Ok(StructDef {
            is_public: false,
            name,
            fields,
            span: Span {
                start: struct_tok.span.start,
                end: self
                    .tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(struct_tok.span.end),
                literal: String::new(),
            },
        })
    }

    fn parse_enum(&mut self) -> Result<crate::pagoda::EnumDef, ParseError> {
        use crate::pagoda::{EnumDef, EnumVariant};

        let enum_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;

        let name_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: enum_tok.span.start,
                span_end: enum_tok.span.end,
            })?;
        let name = match &name_tok.kind {
            TokenKind::Ident(s) => s.clone(),
            other => {
                return Err(ParseError::ExpectedIdent {
                    span_start: name_tok.span.start,
                    span_end: name_tok.span.end,
                    found: other.clone(),
                });
            }
        };
        self.cursor += 1;

        let lbrace = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: name_tok.span.start,
                span_end: name_tok.span.end,
            })?;
        if !matches!(lbrace.kind, TokenKind::LBrace) {
            return Err(ParseError::TrailingTokens {
                span_start: lbrace.span.start,
                span_end: lbrace.span.end,
                found: lbrace.kind.clone(),
            });
        }
        self.cursor += 1;

        let mut variants = Vec::new();
        loop {
            let tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lbrace.span.start,
                    span_end: lbrace.span.end,
                })?;
            if matches!(tok.kind, TokenKind::RBrace) {
                self.cursor += 1;
                break;
            }

            // Parse variant: VariantName or VariantName(Type)
            let variant_name_tok =
                self.tokens
                    .get(self.cursor)
                    .ok_or(ParseError::UnexpectedEof {
                        span_start: lbrace.span.start,
                        span_end: lbrace.span.end,
                    })?;
            let variant_name = match &variant_name_tok.kind {
                TokenKind::Ident(s) => s.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: variant_name_tok.span.start,
                        span_end: variant_name_tok.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;

            // Check for optional associated data type
            let data = if let Some(lparen) = self.tokens.get(self.cursor) {
                if matches!(lparen.kind, TokenKind::LParen) {
                    self.cursor += 1;

                    let type_tok =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: lparen.span.start,
                                span_end: lparen.span.end,
                            })?;
                    let ty = match &type_tok.kind {
                        TokenKind::Ident(s) => s.clone(),
                        other => {
                            return Err(ParseError::ExpectedIdent {
                                span_start: type_tok.span.start,
                                span_end: type_tok.span.end,
                                found: other.clone(),
                            });
                        }
                    };
                    self.cursor += 1;

                    let rparen = self
                        .tokens
                        .get(self.cursor)
                        .ok_or(ParseError::UnexpectedEof {
                            span_start: type_tok.span.start,
                            span_end: type_tok.span.end,
                        })?;
                    if !matches!(rparen.kind, TokenKind::RParen) {
                        return Err(ParseError::TrailingTokens {
                            span_start: rparen.span.start,
                            span_end: rparen.span.end,
                            found: rparen.kind.clone(),
                        });
                    }
                    self.cursor += 1;

                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };

            let variant_span = Span {
                start: variant_name_tok.span.start,
                end: self
                    .tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(variant_name_tok.span.end),
                literal: String::new(),
            };

            variants.push(EnumVariant {
                name: variant_name,
                data,
                span: variant_span,
            });

            // Expect comma or closing brace
            if let Some(tok) = self.tokens.get(self.cursor)
                && matches!(tok.kind, TokenKind::Comma)
            {
                self.cursor += 1;
            }
        }

        Ok(EnumDef {
            is_public: false,
            name,
            variants,
            span: Span {
                start: enum_tok.span.start,
                end: self
                    .tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(enum_tok.span.end),
                literal: String::new(),
            },
        })
    }

    fn parse_match(&mut self) -> Result<Expr, ParseError> {
        use crate::pagoda::MatchArm;

        let match_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;

        // Parse the expression being matched
        let expr = Box::new(self.parse_expr()?);

        // Expect opening brace
        let lbrace = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: expr.span().end,
                span_end: expr.span().end,
            })?;
        if !matches!(lbrace.kind, TokenKind::LBrace) {
            return Err(ParseError::TrailingTokens {
                span_start: lbrace.span.start,
                span_end: lbrace.span.end,
                found: lbrace.kind.clone(),
            });
        }
        self.cursor += 1;

        // Parse match arms
        let mut arms = Vec::new();
        loop {
            let tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lbrace.span.start,
                    span_end: lbrace.span.end,
                })?;
            if matches!(tok.kind, TokenKind::RBrace) {
                self.cursor += 1;
                break;
            }

            // Parse pattern
            let pattern = self.parse_pattern()?;

            // Expect =>
            let arrow = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: pattern.span().end,
                    span_end: pattern.span().end,
                })?;
            if !matches!(arrow.kind, TokenKind::FatArrow) {
                return Err(ParseError::TrailingTokens {
                    span_start: arrow.span.start,
                    span_end: arrow.span.end,
                    found: arrow.kind.clone(),
                });
            }
            self.cursor += 1;

            // Parse body expression
            let body = Box::new(self.parse_expr()?);

            let arm_span = Span {
                start: pattern.span().start,
                end: body.span().end,
                literal: String::new(),
            };

            arms.push(MatchArm {
                pattern,
                body,
                span: arm_span,
            });

            // Expect comma or closing brace
            if let Some(tok) = self.tokens.get(self.cursor)
                && matches!(tok.kind, TokenKind::Comma)
            {
                self.cursor += 1;
            }
        }

        Ok(Expr::Match {
            expr,
            arms,
            span: Span {
                start: match_tok.span.start,
                end: self
                    .tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(match_tok.span.end),
                literal: String::new(),
            },
        })
    }

    fn parse_pattern(&mut self) -> Result<crate::pagoda::Pattern, ParseError> {
        use crate::pagoda::Pattern;

        let tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;

        let ident1 = match &tok.kind {
            TokenKind::Ident(s) => s.clone(),
            other => {
                return Err(ParseError::ExpectedIdent {
                    span_start: tok.span.start,
                    span_end: tok.span.end,
                    found: other.clone(),
                });
            }
        };
        let start_span = tok.span.clone();
        self.cursor += 1;

        // Check if this is qualified (Enum::Variant) or just (Variant)
        if let Some(next) = self.tokens.get(self.cursor)
            && matches!(next.kind, TokenKind::ColonColon)
        {
            // Qualified: Enum::Variant
            self.cursor += 1;

            let variant_tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: next.span.end,
                    span_end: next.span.end,
                })?;
            let variant_name = match &variant_tok.kind {
                TokenKind::Ident(s) => s.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: variant_tok.span.start,
                        span_end: variant_tok.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;

            // Check for optional binding: Variant(x)
            let binding = if let Some(lparen) = self.tokens.get(self.cursor) {
                if matches!(lparen.kind, TokenKind::LParen) {
                    self.cursor += 1;

                    let bind_tok =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: lparen.span.end,
                                span_end: lparen.span.end,
                            })?;
                    let binding_name = match &bind_tok.kind {
                        TokenKind::Ident(s) => s.clone(),
                        other => {
                            return Err(ParseError::ExpectedIdent {
                                span_start: bind_tok.span.start,
                                span_end: bind_tok.span.end,
                                found: other.clone(),
                            });
                        }
                    };
                    self.cursor += 1;

                    let rparen = self
                        .tokens
                        .get(self.cursor)
                        .ok_or(ParseError::UnexpectedEof {
                            span_start: bind_tok.span.end,
                            span_end: bind_tok.span.end,
                        })?;
                    if !matches!(rparen.kind, TokenKind::RParen) {
                        return Err(ParseError::TrailingTokens {
                            span_start: rparen.span.start,
                            span_end: rparen.span.end,
                            found: rparen.kind.clone(),
                        });
                    }
                    self.cursor += 1;

                    Some(binding_name)
                } else {
                    None
                }
            } else {
                None
            };

            return Ok(Pattern::Variant {
                enum_name: Some(ident1),
                variant_name,
                binding,
                span: Span {
                    start: start_span.start,
                    end: self
                        .tokens
                        .get(self.cursor - 1)
                        .map(|t| t.span.end)
                        .unwrap_or(start_span.end),
                    literal: String::new(),
                },
            });
        }

        // Unqualified: just Variant or Variant(x)
        let binding = if let Some(lparen) = self.tokens.get(self.cursor) {
            if matches!(lparen.kind, TokenKind::LParen) {
                self.cursor += 1;

                let bind_tok = self
                    .tokens
                    .get(self.cursor)
                    .ok_or(ParseError::UnexpectedEof {
                        span_start: lparen.span.end,
                        span_end: lparen.span.end,
                    })?;
                let binding_name = match &bind_tok.kind {
                    TokenKind::Ident(s) => s.clone(),
                    other => {
                        return Err(ParseError::ExpectedIdent {
                            span_start: bind_tok.span.start,
                            span_end: bind_tok.span.end,
                            found: other.clone(),
                        });
                    }
                };
                self.cursor += 1;

                let rparen = self
                    .tokens
                    .get(self.cursor)
                    .ok_or(ParseError::UnexpectedEof {
                        span_start: bind_tok.span.end,
                        span_end: bind_tok.span.end,
                    })?;
                if !matches!(rparen.kind, TokenKind::RParen) {
                    return Err(ParseError::TrailingTokens {
                        span_start: rparen.span.start,
                        span_end: rparen.span.end,
                        found: rparen.kind.clone(),
                    });
                }
                self.cursor += 1;

                Some(binding_name)
            } else {
                None
            }
        } else {
            None
        };

        Ok(Pattern::Variant {
            enum_name: None,
            variant_name: ident1,
            binding,
            span: Span {
                start: start_span.start,
                end: self
                    .tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(start_span.end),
                literal: String::new(),
            },
        })
    }

    fn parse_function(&mut self) -> Result<crate::pagoda::Function, ParseError> {
        let fn_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        self.cursor += 1;
        let name_tok = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: fn_tok.span.start,
                span_end: fn_tok.span.end,
            })?;
        let name = match &name_tok.kind {
            TokenKind::Ident(s) => s.clone(),
            other => {
                return Err(ParseError::ExpectedIdent {
                    span_start: name_tok.span.start,
                    span_end: name_tok.span.end,
                    found: other.clone(),
                });
            }
        };
        self.cursor += 1;
        let lparen = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: name_tok.span.start,
                span_end: name_tok.span.end,
            })?;
        if !matches!(lparen.kind, TokenKind::LParen) {
            return Err(ParseError::TrailingTokens {
                span_start: lparen.span.start,
                span_end: lparen.span.end,
                found: lparen.kind.clone(),
            });
        }
        self.cursor += 1;
        let mut params = Vec::new();
        loop {
            let tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lparen.span.start,
                    span_end: lparen.span.end,
                })?;
            if matches!(tok.kind, TokenKind::RParen) {
                self.cursor += 1;
                break;
            }

            // Parse parameter name
            let param_name_tok = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lparen.span.start,
                    span_end: lparen.span.end,
                })?;
            let param_name = match &param_name_tok.kind {
                TokenKind::Ident(s) => s.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: param_name_tok.span.start,
                        span_end: param_name_tok.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;

            // Check for optional type annotation (: type)
            let param_type = if let Some(colon_tok) = self.tokens.get(self.cursor) {
                if matches!(colon_tok.kind, TokenKind::Colon) {
                    self.cursor += 1;

                    // Parse type
                    let type_tok =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: colon_tok.span.start,
                                span_end: colon_tok.span.end,
                            })?;
                    let ty = match &type_tok.kind {
                        TokenKind::Ident(s) => s.clone(),
                        other => {
                            return Err(ParseError::ExpectedIdent {
                                span_start: type_tok.span.start,
                                span_end: type_tok.span.end,
                                found: other.clone(),
                            });
                        }
                    };
                    self.cursor += 1;
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };

            // Calculate the end position for the span
            let param_end = if self.cursor > 0 {
                self.tokens
                    .get(self.cursor - 1)
                    .map(|t| t.span.end)
                    .unwrap_or(param_name_tok.span.end)
            } else {
                param_name_tok.span.end
            };

            params.push(crate::pagoda::FunctionParam {
                name: param_name,
                ty: param_type,
                span: crate::pagoda::Span {
                    start: param_name_tok.span.start,
                    end: param_end,
                    literal: String::new(),
                },
            });

            // Check for comma or closing paren
            let sep = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: lparen.span.start,
                    span_end: lparen.span.end,
                })?;
            if matches!(sep.kind, TokenKind::RParen) {
                self.cursor += 1;
                break;
            } else if matches!(sep.kind, TokenKind::Comma) {
                self.cursor += 1;
                continue;
            } else {
                return Err(ParseError::TrailingTokens {
                    span_start: sep.span.start,
                    span_end: sep.span.end,
                    found: sep.kind.clone(),
                });
            }
        }

        // Parse optional return type (-> type)
        let return_type = if let Some(arrow_tok) = self.tokens.get(self.cursor) {
            if matches!(arrow_tok.kind, TokenKind::Arrow) {
                self.cursor += 1;
                let ret_type_tok =
                    self.tokens
                        .get(self.cursor)
                        .ok_or(ParseError::UnexpectedEof {
                            span_start: arrow_tok.span.start,
                            span_end: arrow_tok.span.end,
                        })?;
                let ret_type = match &ret_type_tok.kind {
                    TokenKind::Ident(s) => s.clone(),
                    other => {
                        return Err(ParseError::ExpectedIdent {
                            span_start: ret_type_tok.span.start,
                            span_end: ret_type_tok.span.end,
                            found: other.clone(),
                        });
                    }
                };
                self.cursor += 1;
                Some(ret_type)
            } else {
                None
            }
        } else {
            None
        };

        let body = self.parse_block()?;
        let span = Span {
            start: fn_tok.span.start,
            end: body.span().end,
            literal: format!(
                "{}{}{}{}",
                fn_tok.span.literal,
                name_tok.span.literal,
                lparen.span.literal,
                body.span().literal
            ),
        };
        Ok(crate::pagoda::Function {
            is_public: false,
            name,
            params,
            return_type,
            body,
            span,
        })
    }

    fn parse_stmt(&mut self) -> Result<crate::pagoda::Stmt, ParseError> {
        let Some(tok) = self.tokens.get(self.cursor) else {
            return Err(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            });
        };

        if matches!(tok.kind, TokenKind::LBrace) {
            self.parse_block()
        } else if matches!(tok.kind, TokenKind::For) {
            self.parse_for()
        } else if matches!(tok.kind, TokenKind::If) {
            self.parse_if()
        } else if matches!(tok.kind, TokenKind::Let) {
            self.cursor += 1;
            let ident = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: tok.span.start,
                    span_end: tok.span.end,
                })?;
            let name = match &ident.kind {
                TokenKind::Ident(name) => name.clone(),
                other => {
                    return Err(ParseError::ExpectedIdent {
                        span_start: ident.span.start,
                        span_end: ident.span.end,
                        found: other.clone(),
                    });
                }
            };
            self.cursor += 1;
            // Optional type annotation
            let ty = if let Some(colon_tok) = self.tokens.get(self.cursor) {
                if matches!(colon_tok.kind, TokenKind::Colon) {
                    self.cursor += 1;
                    let type_tok =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: colon_tok.span.start,
                                span_end: colon_tok.span.end,
                            })?;
                    let ty = match &type_tok.kind {
                        TokenKind::Ident(s) => s.clone(),
                        other => {
                            return Err(ParseError::ExpectedIdent {
                                span_start: type_tok.span.start,
                                span_end: type_tok.span.end,
                                found: other.clone(),
                            });
                        }
                    };
                    self.cursor += 1;
                    Some(ty)
                } else {
                    None
                }
            } else {
                None
            };
            let eq = self
                .tokens
                .get(self.cursor)
                .ok_or(ParseError::UnexpectedEof {
                    span_start: ident.span.start,
                    span_end: ident.span.end,
                })?;
            if !matches!(eq.kind, TokenKind::Assign) {
                return Err(ParseError::ExpectedEquals {
                    span_start: eq.span.start,
                    span_end: eq.span.end,
                    found: eq.kind.clone(),
                });
            }
            self.cursor += 1;
            let expr = self.parse_expr()?;
            let span = Span {
                start: tok.span.start,
                end: expr.span().end,
                literal: format!(
                    "let {}{} = {}",
                    name,
                    ty.as_ref().map(|t| format!(":{}", t)).unwrap_or_default(),
                    expr.span().literal
                ),
            };
            Ok(crate::pagoda::Stmt::Let {
                name,
                ty,
                expr,
                span,
            })
        } else if matches!(tok.kind, TokenKind::Return) {
            self.cursor += 1;
            let expr = self.parse_expr()?;
            let span = Span {
                start: tok.span.start,
                end: expr.span().end,
                literal: format!("{}{}", tok.span.literal, expr.span().literal),
            };
            Ok(crate::pagoda::Stmt::Return { expr, span })
        } else {
            let expr = self.parse_expr()?;
            let span = expr.span().clone();
            Ok(crate::pagoda::Stmt::Expr { expr, span })
        }
    }

    fn parse_block(&mut self) -> Result<crate::pagoda::Stmt, ParseError> {
        let open = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;
        if !matches!(open.kind, TokenKind::LBrace) {
            return Err(ParseError::ExpectedStatement {
                span_start: open.span.start,
                span_end: open.span.end,
            });
        }
        self.cursor += 1;

        let mut stmts = Vec::new();
        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                return Err(ParseError::UnexpectedEof {
                    span_start: open.span.start,
                    span_end: open.span.end,
                });
            };
            if matches!(tok.kind, TokenKind::RBrace) {
                break;
            }
            if matches!(tok.kind, TokenKind::Semicolon) {
                let span = tok.span.clone();
                self.cursor += 1;
                stmts.push(crate::pagoda::Stmt::Empty { span });
                continue;
            }
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            if let Some(sep) = self.tokens.get(self.cursor)
                && matches!(sep.kind, TokenKind::Semicolon)
            {
                self.cursor += 1;
            }
        }

        if stmts.is_empty() {
            stmts.push(crate::pagoda::Stmt::Empty {
                span: Span {
                    start: open.span.start,
                    end: open.span.end,
                    literal: "{}".to_string(),
                },
            });
        }

        let close = self.tokens.get(self.cursor).unwrap();
        let span = Span {
            start: open.span.start,
            end: close.span.end,
            literal: open.span.literal.clone()
                + stmts.last().unwrap().span().literal.as_str()
                + &close.span.literal,
        };
        self.cursor += 1;
        Ok(crate::pagoda::Stmt::Block { stmts, span })
    }

    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        self.parse_assign()
    }

    fn parse_assign(&mut self) -> Result<Expr, ParseError> {
        let node = self.parse_logical_or()?;
        let Some(tok) = self.tokens.get(self.cursor) else {
            return Ok(node);
        };
        if matches!(
            tok.kind,
            TokenKind::Assign
                | TokenKind::PlusAssign
                | TokenKind::MinusAssign
                | TokenKind::StarAssign
                | TokenKind::SlashAssign
                | TokenKind::PercentAssign
                | TokenKind::ShlAssign
                | TokenKind::ShrAssign
                | TokenKind::AmpAssign
                | TokenKind::PipeAssign
                | TokenKind::CaretAssign
        ) {
            // Only identifiers, index expressions, or field accesses can be assigned to.
            let lhs_span = node.span().clone();
            let (target, _index_expr) = match node {
                Expr::Var { name, .. } => (name, None::<Expr>),
                Expr::Index { base, index, .. } => {
                    let idx_span = index.span().clone();
                    let base_span = base.span().clone();
                    let span = Span {
                        start: base_span.start,
                        end: idx_span.end,
                        literal: format!("{}[{}]", base_span.literal, idx_span.literal),
                    };
                    return {
                        let op_span = tok.span.clone();
                        self.cursor += 1;
                        let rhs = self.parse_assign()?;
                        let rhs_span = rhs.span().clone();
                        let full_span = Span {
                            start: span.start,
                            end: rhs_span.end,
                            literal: format!(
                                "{}{}{}",
                                span.literal, op_span.literal, rhs_span.literal
                            ),
                        };
                        Ok(Expr::IndexAssign {
                            base,
                            index,
                            value: Box::new(rhs),
                            span: full_span,
                        })
                    };
                }
                Expr::FieldAccess {
                    base, field_name, ..
                } => {
                    let base_span = base.span().clone();
                    let span = Span {
                        start: base_span.start,
                        end: lhs_span.end,
                        literal: format!("{}.{}", base_span.literal, field_name),
                    };
                    return {
                        let op_span = tok.span.clone();
                        self.cursor += 1;
                        let rhs = self.parse_assign()?;
                        let rhs_span = rhs.span().clone();
                        let full_span = Span {
                            start: span.start,
                            end: rhs_span.end,
                            literal: format!(
                                "{}{}{}",
                                span.literal, op_span.literal, rhs_span.literal
                            ),
                        };
                        Ok(Expr::FieldAssign {
                            base,
                            field_name,
                            value: Box::new(rhs),
                            span: full_span,
                        })
                    };
                }
                _ => {
                    return Err(ParseError::TrailingTokens {
                        span_start: tok.span.start,
                        span_end: tok.span.end,
                        found: tok.kind.clone(),
                    });
                }
            };
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_assign()?;
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            return Ok(match tok.kind {
                TokenKind::Assign => Expr::Assign {
                    name: target,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::PlusAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Add,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::MinusAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Sub,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::StarAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Mul,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::SlashAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Div,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::PercentAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Mod,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::ShlAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Shl,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::ShrAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::Shr,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::AmpAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::BitAnd,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::PipeAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::BitOr,
                    value: Box::new(rhs),
                    span,
                },
                TokenKind::CaretAssign => Expr::CompoundAssign {
                    name: target,
                    op: BinOp::BitXor,
                    value: Box::new(rhs),
                    span,
                },
                _ => unreachable!(),
            });
        }
        Ok(node)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_logical_and()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if !matches!(tok.kind, TokenKind::PipePipe) {
                break;
            }
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_logical_and()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op: BinOp::LogicalOr,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_compare()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if !matches!(tok.kind, TokenKind::AmpAmp) {
                break;
            }
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_compare()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op: BinOp::LogicalAnd,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_or()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            let op = match tok.kind {
                TokenKind::EqEq => BinOp::Eq,
                TokenKind::NotEq => BinOp::Ne,
                TokenKind::Less => BinOp::Lt,
                TokenKind::Greater => BinOp::Gt,
                TokenKind::LessEq => BinOp::Le,
                TokenKind::GreaterEq => BinOp::Ge,
                _ => break,
            };
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_sum()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_bit_or(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_xor()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if !matches!(tok.kind, TokenKind::Pipe) {
                break;
            }
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_bit_xor()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op: BinOp::BitOr,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_bit_xor(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_bit_and()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if !matches!(tok.kind, TokenKind::Caret) {
                break;
            }
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_bit_and()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op: BinOp::BitXor,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_bit_and(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_shift()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if !matches!(tok.kind, TokenKind::Amp) {
                break;
            }
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_shift()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op: BinOp::BitAnd,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_sum()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            let op = match tok.kind {
                TokenKind::Shl => BinOp::Shl,
                TokenKind::Shr => BinOp::Shr,
                _ => break,
            };
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_sum()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_sum(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_term()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            let op = match tok.kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => break,
            };
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_term()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_term(&mut self) -> Result<Expr, ParseError> {
        let mut node = self.parse_factor()?;

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            let op = match tok.kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                TokenKind::Percent => BinOp::Mod,
                _ => break,
            };
            let op_span = tok.span.clone();
            self.cursor += 1;
            let rhs = self.parse_factor()?;
            let lhs_span = node.span().clone();
            let rhs_span = rhs.span().clone();
            let span = Span {
                start: lhs_span.start,
                end: rhs_span.end,
                literal: format!(
                    "{}{}{}",
                    lhs_span.literal, op_span.literal, rhs_span.literal
                ),
            };
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(rhs),
                span,
            };
        }

        Ok(node)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let mut prefixes: Vec<(UnaryOp, Span)> = Vec::new();
        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            match tok.kind {
                TokenKind::Plus => {
                    prefixes.push((UnaryOp::Plus, tok.span.clone()));
                    self.cursor += 1;
                }
                TokenKind::Minus => {
                    prefixes.push((UnaryOp::Minus, tok.span.clone()));
                    self.cursor += 1;
                }
                TokenKind::Tilde => {
                    prefixes.push((UnaryOp::BitNot, tok.span.clone()));
                    self.cursor += 1;
                }
                TokenKind::Bang => {
                    prefixes.push((UnaryOp::LogicalNot, tok.span.clone()));
                    self.cursor += 1;
                }
                _ => break,
            }
        }

        let token = self
            .tokens
            .get(self.cursor)
            .ok_or(ParseError::UnexpectedEof {
                span_start: 0,
                span_end: 0,
            })?;

        let mut node =
            match &token.kind {
                TokenKind::Int(value) => {
                    self.cursor += 1;
                    Expr::IntLiteral {
                        value: *value,
                        span: token.span.clone(),
                    }
                }
                TokenKind::True => {
                    self.cursor += 1;
                    Expr::BoolLiteral {
                        value: true,
                        span: token.span.clone(),
                    }
                }
                TokenKind::False => {
                    self.cursor += 1;
                    Expr::BoolLiteral {
                        value: false,
                        span: token.span.clone(),
                    }
                }
                TokenKind::String(value) => {
                    self.cursor += 1;
                    Expr::StringLiteral {
                        value: value.clone(),
                        span: token.span.clone(),
                    }
                }
                TokenKind::LBracket => {
                    self.cursor += 1;
                    let mut elems = Vec::new();
                    let mut end_pos = token.span.end;
                    let _ = &end_pos;
                    if matches!(
                        self.tokens.get(self.cursor).map(|t| &t.kind),
                        Some(TokenKind::RBracket)
                    ) {
                        end_pos = self.tokens[self.cursor].span.end;
                        self.cursor += 1;
                    } else {
                        loop {
                            let elem = self.parse_expr()?;
                            end_pos = elem.span().end;
                            elems.push(elem);
                            let sep =
                                self.tokens
                                    .get(self.cursor)
                                    .ok_or(ParseError::UnexpectedEof {
                                        span_start: token.span.start,
                                        span_end: end_pos,
                                    })?;
                            match sep.kind {
                                TokenKind::Comma => {
                                    self.cursor += 1;
                                    continue;
                                }
                                TokenKind::RBracket => {
                                    end_pos = sep.span.end;
                                    self.cursor += 1;
                                    break;
                                }
                                _ => {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: sep.span.start,
                                        span_end: sep.span.end,
                                        found: sep.kind.clone(),
                                    });
                                }
                            }
                        }
                    }
                    let mut literal = String::from("[");
                    for (idx, e) in elems.iter().enumerate() {
                        literal.push_str(&e.span().literal);
                        if idx + 1 != elems.len() {
                            literal.push(',');
                        }
                    }
                    literal.push(']');
                    Expr::ArrayLiteral {
                        elements: elems,
                        span: Span {
                            start: token.span.start,
                            end: end_pos,
                            literal,
                        },
                    }
                }
                TokenKind::Ident(name) => {
                    // Check for qualified names (module::name)
                    if let Some(next) = self.tokens.get(self.cursor + 1)
                        && matches!(next.kind, TokenKind::ColonColon)
                    {
                        let module_name = name.clone();
                        let start_span = token.span.clone();
                        self.cursor += 2; // Skip ident and ::

                        let name_tok =
                            self.tokens
                                .get(self.cursor)
                                .ok_or(ParseError::UnexpectedEof {
                                    span_start: start_span.end,
                                    span_end: start_span.end,
                                })?;

                        let item_name = match &name_tok.kind {
                            TokenKind::Ident(n) => n.clone(),
                            _ => {
                                return Err(ParseError::ExpectedIdent {
                                    span_start: name_tok.span.start,
                                    span_end: name_tok.span.end,
                                    found: name_tok.kind.clone(),
                                });
                            }
                        };
                        self.cursor += 1;

                        let next_tok = self.tokens.get(self.cursor);

                        // Check if it's a function call: module::func(...)
                        if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::LParen)) {
                            self.cursor += 1;

                            let mut args = Vec::new();
                            let closing_end = loop {
                                let tok = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: start_span.start,
                                        span_end: start_span.end,
                                    },
                                )?;

                                if matches!(tok.kind, TokenKind::RParen) {
                                    self.cursor += 1;
                                    break tok.span.end;
                                }

                                let arg = self.parse_expr()?;
                                args.push(arg);

                                let sep = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: tok.span.start,
                                        span_end: tok.span.end,
                                    },
                                )?;

                                if matches!(sep.kind, TokenKind::RParen) {
                                    self.cursor += 1;
                                    break sep.span.end;
                                } else if matches!(sep.kind, TokenKind::Comma) {
                                    self.cursor += 1;
                                    continue;
                                } else {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: sep.span.start,
                                        span_end: sep.span.end,
                                        found: sep.kind.clone(),
                                    });
                                }
                            };

                            let literal = format!("{}::{}(...)", module_name, item_name);
                            return Ok(Expr::QualifiedCall {
                                module: module_name,
                                name: item_name,
                                args,
                                span: Span {
                                    start: start_span.start,
                                    end: closing_end,
                                    literal,
                                },
                            });
                        }
                        // Check if it's a struct literal: module::Struct { ... }
                        else if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::LBrace)) {
                            self.cursor += 1;

                            let mut field_values = Vec::new();
                            let closing_end = loop {
                                let tok = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: start_span.start,
                                        span_end: start_span.end,
                                    },
                                )?;

                                if matches!(tok.kind, TokenKind::RBrace) {
                                    self.cursor += 1;
                                    break tok.span.end;
                                }

                                // Parse field_name: expr
                                let field_name_tok = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: start_span.start,
                                        span_end: start_span.end,
                                    },
                                )?;

                                let field_name = match &field_name_tok.kind {
                                    TokenKind::Ident(s) => s.clone(),
                                    other => {
                                        return Err(ParseError::ExpectedIdent {
                                            span_start: field_name_tok.span.start,
                                            span_end: field_name_tok.span.end,
                                            found: other.clone(),
                                        });
                                    }
                                };
                                self.cursor += 1;

                                // Expect colon
                                let colon = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: field_name_tok.span.start,
                                        span_end: field_name_tok.span.end,
                                    },
                                )?;

                                if !matches!(colon.kind, TokenKind::Colon) {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: colon.span.start,
                                        span_end: colon.span.end,
                                        found: colon.kind.clone(),
                                    });
                                }
                                self.cursor += 1;

                                let value = self.parse_expr()?;
                                field_values.push((field_name, value));

                                // Optional comma
                                let sep = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: start_span.start,
                                        span_end: start_span.end,
                                    },
                                )?;

                                if matches!(sep.kind, TokenKind::Comma) {
                                    self.cursor += 1;
                                } else if matches!(sep.kind, TokenKind::RBrace) {
                                    self.cursor += 1;
                                    break sep.span.end;
                                } else {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: sep.span.start,
                                        span_end: sep.span.end,
                                        found: sep.kind.clone(),
                                    });
                                }
                            };

                            let literal = format!("{}::{} {{ ... }}", module_name, item_name);
                            return Ok(Expr::QualifiedStructLiteral {
                                module: module_name,
                                struct_name: item_name,
                                field_values,
                                span: Span {
                                    start: start_span.start,
                                    end: closing_end,
                                    literal,
                                },
                            });
                        }
                        // Check if it's another :: (for Enum::Variant or module::Enum::Variant)
                        else if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::ColonColon)) {
                            self.cursor += 1; // skip ::

                            let variant_tok =
                                self.tokens
                                    .get(self.cursor)
                                    .ok_or(ParseError::UnexpectedEof {
                                        span_start: start_span.end,
                                        span_end: start_span.end,
                                    })?;

                            let variant_name = match &variant_tok.kind {
                                TokenKind::Ident(n) => n.clone(),
                                _ => {
                                    return Err(ParseError::ExpectedIdent {
                                        span_start: variant_tok.span.start,
                                        span_end: variant_tok.span.end,
                                        found: variant_tok.kind.clone(),
                                    });
                                }
                            };
                            self.cursor += 1;

                            // Check for optional data: Variant or Variant(expr)
                            let data = if matches!(
                                self.tokens.get(self.cursor).map(|t| &t.kind),
                                Some(TokenKind::LParen)
                            ) {
                                self.cursor += 1;
                                let expr = self.parse_expr()?;

                                let rparen = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: expr.span().end,
                                        span_end: expr.span().end,
                                    },
                                )?;

                                if !matches!(rparen.kind, TokenKind::RParen) {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: rparen.span.start,
                                        span_end: rparen.span.end,
                                        found: rparen.kind.clone(),
                                    });
                                }
                                self.cursor += 1;
                                Some(Box::new(expr))
                            } else {
                                None
                            };

                            let literal = if let Some(ref d) = data {
                                format!(
                                    "{}::{}::{}({})",
                                    module_name,
                                    item_name,
                                    variant_name,
                                    d.span().literal
                                )
                            } else {
                                format!("{}::{}::{}", module_name, item_name, variant_name)
                            };

                            return Ok(Expr::QualifiedEnumLiteral {
                                module: module_name,
                                enum_name: item_name,
                                variant_name,
                                data,
                                span: Span {
                                    start: start_span.start,
                                    end: self
                                        .tokens
                                        .get(self.cursor - 1)
                                        .map(|t| t.span.end)
                                        .unwrap_or(start_span.end),
                                    literal,
                                },
                            });
                        }
                        // No third ::, LParen, or LBrace - treat as EnumLiteral (Enum::Variant)
                        else {
                            // item_name is the variant, module_name is the enum
                            // Check for optional data: Variant or Variant(expr)
                            let data =
                                if matches!(next_tok.map(|t| &t.kind), Some(TokenKind::LParen)) {
                                    self.cursor += 1;
                                    let expr = self.parse_expr()?;

                                    let rparen = self.tokens.get(self.cursor).ok_or(
                                        ParseError::UnexpectedEof {
                                            span_start: expr.span().end,
                                            span_end: expr.span().end,
                                        },
                                    )?;

                                    if !matches!(rparen.kind, TokenKind::RParen) {
                                        return Err(ParseError::TrailingTokens {
                                            span_start: rparen.span.start,
                                            span_end: rparen.span.end,
                                            found: rparen.kind.clone(),
                                        });
                                    }
                                    self.cursor += 1;
                                    Some(Box::new(expr))
                                } else {
                                    None
                                };

                            let literal = if let Some(ref d) = data {
                                format!("{}::{}({})", module_name, item_name, d.span().literal)
                            } else {
                                format!("{}::{}", module_name, item_name)
                            };

                            return Ok(Expr::EnumLiteral {
                                enum_name: module_name,
                                variant_name: item_name,
                                data,
                                span: Span {
                                    start: start_span.start,
                                    end: self
                                        .tokens
                                        .get(self.cursor - 1)
                                        .map(|t| t.span.end)
                                        .unwrap_or(start_span.end),
                                    literal,
                                },
                            });
                        }
                    }

                    // Not a qualified name, proceed with normal parsing
                    self.cursor += 1;
                    if let Some(next) = self.tokens.get(self.cursor) {
                        if matches!(next.kind, TokenKind::LBrace) {
                            // Check if this is actually a struct literal by peeking ahead
                            // A struct literal has the pattern: Ident { field: value, ... }
                            // If the first token after { is not an identifier followed by :, it's not a struct literal
                            let is_struct_literal =
                                if let Some(peek1) = self.tokens.get(self.cursor + 1) {
                                    if matches!(peek1.kind, TokenKind::Ident(_)) {
                                        // Check if there's a colon after the identifier
                                        if let Some(peek2) = self.tokens.get(self.cursor + 2) {
                                            matches!(peek2.kind, TokenKind::Colon)
                                        } else {
                                            false
                                        }
                                    } else if matches!(peek1.kind, TokenKind::RBrace) {
                                        // Empty struct literal
                                        true
                                    } else {
                                        false
                                    }
                                } else {
                                    false
                                };

                            if is_struct_literal {
                                // Struct literal: StructName { field: value, ... }
                                let literal_start_idx = self.cursor - 1;
                                self.cursor += 1;
                                let mut field_values = Vec::new();
                                let (closing_end, closing_idx) = loop {
                                    let tok = self.tokens.get(self.cursor).ok_or(
                                        ParseError::UnexpectedEof {
                                            span_start: next.span.start,
                                            span_end: next.span.end,
                                        },
                                    )?;
                                    if matches!(tok.kind, TokenKind::RBrace) {
                                        let idx = self.cursor;
                                        self.cursor += 1;
                                        break (tok.span.end, idx);
                                    }

                                    // Parse field_name: expr
                                    let field_name_tok = self.tokens.get(self.cursor).ok_or(
                                        ParseError::UnexpectedEof {
                                            span_start: next.span.start,
                                            span_end: next.span.end,
                                        },
                                    )?;
                                    let field_name = match &field_name_tok.kind {
                                        TokenKind::Ident(s) => s.clone(),
                                        other => {
                                            return Err(ParseError::ExpectedIdent {
                                                span_start: field_name_tok.span.start,
                                                span_end: field_name_tok.span.end,
                                                found: other.clone(),
                                            });
                                        }
                                    };
                                    self.cursor += 1;

                                    // Expect colon
                                    let colon = self.tokens.get(self.cursor).ok_or(
                                        ParseError::UnexpectedEof {
                                            span_start: field_name_tok.span.start,
                                            span_end: field_name_tok.span.end,
                                        },
                                    )?;
                                    if !matches!(colon.kind, TokenKind::Colon) {
                                        return Err(ParseError::TrailingTokens {
                                            span_start: colon.span.start,
                                            span_end: colon.span.end,
                                            found: colon.kind.clone(),
                                        });
                                    }
                                    self.cursor += 1;

                                    // Parse value expression
                                    let value_expr = self.parse_expr()?;
                                    field_values.push((field_name, value_expr));

                                    // Check for comma or closing brace
                                    let sep = self.tokens.get(self.cursor).ok_or(
                                        ParseError::UnexpectedEof {
                                            span_start: next.span.start,
                                            span_end: next.span.end,
                                        },
                                    )?;
                                    if matches!(sep.kind, TokenKind::Comma) {
                                        self.cursor += 1;
                                    } else if matches!(sep.kind, TokenKind::RBrace) {
                                        let idx = self.cursor;
                                        self.cursor += 1;
                                        break (sep.span.end, idx);
                                    } else {
                                        return Err(ParseError::TrailingTokens {
                                            span_start: sep.span.start,
                                            span_end: sep.span.end,
                                            found: sep.kind.clone(),
                                        });
                                    }
                                };

                                let literal = self.tokens[literal_start_idx..=closing_idx]
                                    .iter()
                                    .map(|t| t.span.literal.as_str())
                                    .collect::<String>();
                                Expr::StructLiteral {
                                    struct_name: name.clone(),
                                    field_values,
                                    span: Span {
                                        start: token.span.start,
                                        end: closing_end,
                                        literal,
                                    },
                                }
                            } else {
                                // Not a struct literal, just return the variable
                                // Cursor is already past the identifier, which is correct
                                Expr::Var {
                                    name: name.clone(),
                                    span: token.span.clone(),
                                }
                            }
                        } else if matches!(next.kind, TokenKind::LParen) {
                            self.cursor += 1;
                            let mut args = Vec::new();
                            let closing_end = loop {
                                let tok = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: next.span.start,
                                        span_end: next.span.end,
                                    },
                                )?;
                                if matches!(tok.kind, TokenKind::RParen) {
                                    self.cursor += 1;
                                    break tok.span.end;
                                }
                                let arg = self.parse_expr()?;
                                args.push(arg);
                                let sep = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: tok.span.start,
                                        span_end: tok.span.end,
                                    },
                                )?;
                                if matches!(sep.kind, TokenKind::RParen) {
                                    self.cursor += 1;
                                    break sep.span.end;
                                } else if matches!(sep.kind, TokenKind::Comma) {
                                    self.cursor += 1;
                                    continue;
                                } else {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: sep.span.start,
                                        span_end: sep.span.end,
                                        found: sep.kind.clone(),
                                    });
                                }
                            };
                            let end_span = closing_end;
                            let mut literal = String::new();
                            literal.push_str(&token.span.literal);
                            literal.push('(');
                            for (idx, arg) in args.iter().enumerate() {
                                literal.push_str(&arg.span().literal);
                                if idx + 1 != args.len() {
                                    literal.push(',');
                                }
                            }
                            literal.push(')');
                            Expr::Call {
                                name: name.clone(),
                                args,
                                span: Span {
                                    start: token.span.start,
                                    end: end_span,
                                    literal,
                                },
                            }
                        } else if matches!(next.kind, TokenKind::ColonColon) {
                            // Enum variant: EnumName::Variant or EnumName::Variant(expr)
                            self.cursor += 1;

                            let variant_tok =
                                self.tokens
                                    .get(self.cursor)
                                    .ok_or(ParseError::UnexpectedEof {
                                        span_start: next.span.end,
                                        span_end: next.span.end,
                                    })?;

                            let variant_name = match &variant_tok.kind {
                                TokenKind::Ident(n) => n.clone(),
                                _ => {
                                    return Err(ParseError::ExpectedIdent {
                                        span_start: variant_tok.span.start,
                                        span_end: variant_tok.span.end,
                                        found: variant_tok.kind.clone(),
                                    });
                                }
                            };
                            self.cursor += 1;

                            // Check for optional data: Variant or Variant(expr)
                            let data = if matches!(
                                self.tokens.get(self.cursor).map(|t| &t.kind),
                                Some(TokenKind::LParen)
                            ) {
                                self.cursor += 1;
                                let expr = self.parse_expr()?;

                                let rparen = self.tokens.get(self.cursor).ok_or(
                                    ParseError::UnexpectedEof {
                                        span_start: expr.span().end,
                                        span_end: expr.span().end,
                                    },
                                )?;

                                if !matches!(rparen.kind, TokenKind::RParen) {
                                    return Err(ParseError::TrailingTokens {
                                        span_start: rparen.span.start,
                                        span_end: rparen.span.end,
                                        found: rparen.kind.clone(),
                                    });
                                }
                                self.cursor += 1;
                                Some(Box::new(expr))
                            } else {
                                None
                            };

                            let literal = if let Some(ref d) = data {
                                format!("{}::{}({})", name, variant_name, d.span().literal)
                            } else {
                                format!("{}::{}", name, variant_name)
                            };

                            Expr::EnumLiteral {
                                enum_name: name.clone(),
                                variant_name,
                                data,
                                span: Span {
                                    start: token.span.start,
                                    end: self
                                        .tokens
                                        .get(self.cursor - 1)
                                        .map(|t| t.span.end)
                                        .unwrap_or(token.span.end),
                                    literal,
                                },
                            }
                        } else {
                            Expr::Var {
                                name: name.clone(),
                                span: token.span.clone(),
                            }
                        }
                    } else {
                        Expr::Var {
                            name: name.clone(),
                            span: token.span.clone(),
                        }
                    }
                }
                TokenKind::Match => {
                    return self.parse_match();
                }
                TokenKind::LParen => {
                    self.cursor += 1;
                    let expr = self.parse_expr()?;
                    let closing =
                        self.tokens
                            .get(self.cursor)
                            .ok_or(ParseError::UnexpectedEof {
                                span_start: token.span.start,
                                span_end: token.span.end,
                            })?;
                    match closing.kind {
                        TokenKind::RParen => {
                            let span = Span {
                                start: token.span.start,
                                end: closing.span.end,
                                literal: format!("({})", expr.span().literal),
                            };
                            self.cursor += 1;
                            expr_with_span(expr, span)
                        }
                        _ => {
                            return Err(ParseError::ExpectedInt {
                                span_start: closing.span.start,
                                span_end: closing.span.end,
                                found: closing.kind.clone(),
                            });
                        }
                    }
                }
                TokenKind::Eof => {
                    return Err(ParseError::UnexpectedEof {
                        span_start: token.span.start,
                        span_end: token.span.end,
                    });
                }
                other => {
                    return Err(ParseError::ExpectedInt {
                        span_start: token.span.start,
                        span_end: token.span.end,
                        found: other.clone(),
                    });
                }
            };

        loop {
            let Some(tok) = self.tokens.get(self.cursor) else {
                break;
            };
            if matches!(tok.kind, TokenKind::LBracket) {
                // Array indexing: base[index]
                let open_span = tok.span.clone();
                self.cursor += 1;
                let idx_expr = self.parse_expr()?;
                let closing = self
                    .tokens
                    .get(self.cursor)
                    .ok_or(ParseError::UnexpectedEof {
                        span_start: open_span.start,
                        span_end: open_span.end,
                    })?;
                if !matches!(closing.kind, TokenKind::RBracket) {
                    return Err(ParseError::TrailingTokens {
                        span_start: closing.span.start,
                        span_end: closing.span.end,
                        found: closing.kind.clone(),
                    });
                }
                self.cursor += 1;
                let base_span = node.span().clone();
                let idx_span = idx_expr.span().clone();
                let span = Span {
                    start: base_span.start,
                    end: closing.span.end,
                    literal: format!("{}[{}]", base_span.literal, idx_span.literal),
                };
                node = Expr::Index {
                    base: Box::new(node),
                    index: Box::new(idx_expr),
                    span,
                };
            } else if matches!(tok.kind, TokenKind::Dot) {
                // Field access: base.field
                let dot_span = tok.span.clone();
                self.cursor += 1;
                let field_tok = self
                    .tokens
                    .get(self.cursor)
                    .ok_or(ParseError::UnexpectedEof {
                        span_start: dot_span.start,
                        span_end: dot_span.end,
                    })?;
                let field_name = match &field_tok.kind {
                    TokenKind::Ident(s) => s.clone(),
                    other => {
                        return Err(ParseError::ExpectedIdent {
                            span_start: field_tok.span.start,
                            span_end: field_tok.span.end,
                            found: other.clone(),
                        });
                    }
                };
                self.cursor += 1;
                let base_span = node.span().clone();
                let span = Span {
                    start: base_span.start,
                    end: field_tok.span.end,
                    literal: format!("{}.{}", base_span.literal, field_name),
                };
                node = Expr::FieldAccess {
                    base: Box::new(node),
                    field_name,
                    span,
                };
            } else {
                break;
            }
        }

        for (op, op_span) in prefixes.into_iter().rev() {
            let span = Span {
                start: op_span.start.min(node.span().start),
                end: node.span().end,
                literal: format!("{}{}", op_span.literal, node.span().literal),
            };
            node = Expr::Unary {
                op,
                expr: Box::new(node),
                span,
            };
        }

        Ok(node)
    }
}

fn expr_with_span(expr: Expr, span: Span) -> Expr {
    match expr {
        Expr::IntLiteral { value, .. } => Expr::IntLiteral { value, span },
        Expr::BoolLiteral { value, .. } => Expr::BoolLiteral { value, span },
        Expr::Var { name, .. } => Expr::Var { name, span },
        Expr::Binary {
            op, left, right, ..
        } => Expr::Binary {
            op,
            left,
            right,
            span,
        },
        Expr::Assign { name, value, .. } => Expr::Assign { name, value, span },
        Expr::CompoundAssign {
            name, op, value, ..
        } => Expr::CompoundAssign {
            name,
            op,
            value,
            span,
        },
        Expr::Unary { op, expr, .. } => Expr::Unary { op, expr, span },
        Expr::Call { name, args, .. } => Expr::Call { name, args, span },
        Expr::ArrayLiteral { elements, .. } => Expr::ArrayLiteral { elements, span },
        Expr::Index { base, index, .. } => Expr::Index { base, index, span },
        Expr::IndexAssign {
            base, index, value, ..
        } => Expr::IndexAssign {
            base,
            index,
            value,
            span,
        },
        Expr::StructLiteral {
            struct_name,
            field_values,
            ..
        } => Expr::StructLiteral {
            struct_name,
            field_values,
            span,
        },
        Expr::StringLiteral { value, .. } => Expr::StringLiteral { value, span },
        Expr::FieldAccess {
            base, field_name, ..
        } => Expr::FieldAccess {
            base,
            field_name,
            span,
        },
        Expr::FieldAssign {
            base,
            field_name,
            value,
            ..
        } => Expr::FieldAssign {
            base,
            field_name,
            value,
            span,
        },
        Expr::QualifiedCall {
            module, name, args, ..
        } => Expr::QualifiedCall {
            module,
            name,
            args,
            span,
        },
        Expr::QualifiedStructLiteral {
            module,
            struct_name,
            field_values,
            ..
        } => Expr::QualifiedStructLiteral {
            module,
            struct_name,
            field_values,
            span,
        },
        Expr::EnumLiteral {
            enum_name,
            variant_name,
            data,
            ..
        } => Expr::EnumLiteral {
            enum_name,
            variant_name,
            data,
            span,
        },
        Expr::QualifiedEnumLiteral {
            module,
            enum_name,
            variant_name,
            data,
            ..
        } => Expr::QualifiedEnumLiteral {
            module,
            enum_name,
            variant_name,
            data,
            span,
        },
        Expr::Match { expr, arms, .. } => Expr::Match { expr, arms, span },
    }
}

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut parser = Parser::new(tokens);
    parser.parse_program_impl()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pagoda::tokenizer::tokenize;
    use insta::assert_debug_snapshot;

    #[test]
    fn parses_single_int_literal() {
        let tokens = tokenize("{123}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_single_int_literal", program);
    }

    #[test]
    fn parses_string_literal() {
        let tokens = tokenize("{\"hi\"}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_string_literal", program);
    }

    #[test]
    fn parses_string_assignment() {
        let tokens = tokenize("{ let x = \"hi\"; x }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_string_assignment", program.stmts);
    }

    #[test]
    fn rejects_trailing_tokens() {
        let tokens = tokenize("{1 2}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_without_semicolon", program);
    }

    #[test]
    fn parses_precedence() {
        let tokens = tokenize("{1+2*3}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_precedence", program.stmts);
    }

    #[test]
    fn parses_unary_precedence() {
        let tokens = tokenize("{-1+2}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_unary_precedence", program.stmts);
    }

    #[test]
    fn parses_bitwise_ops() {
        let tokens = tokenize("{~1 & 2 | 3 ^ 4}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_bitwise_ops", program.stmts);
    }

    #[test]
    fn parses_shifts() {
        let tokens = tokenize("{1<<2 + 3>>1}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_shifts", program.stmts);
    }

    #[test]
    fn parses_parentheses() {
        let tokens = tokenize("{-(1+2)*3}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_parentheses", program.stmts);
    }

    #[test]
    fn parses_comparisons() {
        let tokens = tokenize("{1+2==3*4}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_comparisons", program.stmts);
    }

    #[test]
    fn parses_statements() {
        let tokens = tokenize("{1;2+3;4}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_statements", program.stmts);
    }

    #[test]
    fn parses_let_statement() {
        let tokens = tokenize("{let x = 1+2; x}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_let_statement", program.stmts);
    }

    #[test]
    fn parses_typed_let_statement() {
        let tokens = tokenize("{let x: i32 = 1+2; x}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_typed_let_statement", program.stmts);
    }

    #[test]
    fn parses_return_statement() {
        let tokens = tokenize("{1; return 2; 3}").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_return_statement", program.stmts);
    }

    #[test]
    fn parses_if_else_statement() {
        let tokens = tokenize("{ if (1) { 2 } else { 3 } }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_if_else_statement", program.stmts);
    }

    #[test]
    fn parses_if_elseif_else_statement() {
        let tokens = tokenize("{ if (1) { 2 } else if (0) { 3 } else { 4 } }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_if_elseif_else_statement", program.stmts);
    }

    #[test]
    fn parses_for_loop() {
        let tokens = tokenize("{ for (let i = 0; i < 1; i = i + 1) { 2; }; 3 }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_for_loop", program.stmts);
    }

    #[test]
    fn parses_compound_assignments() {
        let tokens = tokenize("{ let a = 1; a += 2; a -= 3; a *= 4; a /= 5; a %= 6 }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_compound_assignments", program.stmts);
    }

    #[test]
    fn parses_modulo_expression() {
        let tokens = tokenize("{ 7 % 3 }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_modulo_expression", program.stmts);
    }

    #[test]
    fn parses_functions() {
        let tokens = tokenize("fn foo() { {1;} ; } ; { foo() }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_functions", program);
    }

    #[test]
    fn parses_function_with_args() {
        let tokens = tokenize("fn add(a: i64, b: i64) { a + b } { add(2,3) }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_function_with_args", program);
    }

    #[test]
    fn parses_struct_definition() {
        let tokens = tokenize("struct Point { x: i64, y: i64 }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_struct_definition", program);
    }

    #[test]
    fn parses_struct_with_string_field() {
        let tokens = tokenize("struct Message { text: string, code: i64 }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_struct_with_string_field", program);
    }

    #[test]
    fn parses_enum_definition() {
        let tokens = tokenize("enum Option { Some(i64), None }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_enum_definition", program);
    }

    #[test]
    fn parses_enum_with_multiple_variants() {
        let tokens = tokenize("enum Result { Ok(i64), Err(string) }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_enum_with_multiple_variants", program);
    }

    #[test]
    fn parses_enum_literal_without_data() {
        let tokens = tokenize("{ let x = Option::None; x }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_enum_literal_without_data", program);
    }

    #[test]
    fn parses_enum_literal_with_data() {
        let tokens = tokenize("{ Option::Some(42) }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_enum_literal_with_data", program);
    }

    #[test]
    fn parses_match_with_qualified_patterns() {
        let tokens =
            tokenize("{ match x { Option::Some(val) => val, Option::None => 0 } }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_match_with_qualified_patterns", program);
    }

    #[test]
    fn parses_match_with_unqualified_patterns() {
        let tokens = tokenize("{ match x { Some(val) => val, None => 0 } }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_match_with_unqualified_patterns", program);
    }

    #[test]
    fn parses_match_without_bindings() {
        let tokens = tokenize("{ match x { Some => 1, None => 0 } }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_match_without_bindings", program);
    }

    #[test]
    fn parses_struct_literal_and_field_access() {
        let tokens = tokenize("{ let p = Point { x: 10, y: 20 }; p.x }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_struct_literal_and_field_access", program);
    }

    #[test]
    fn parses_field_assignment() {
        let tokens = tokenize("{ let p = Point { x: 10, y: 20 }; p.x = 30; p.x }").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_field_assignment", program);
    }
}
