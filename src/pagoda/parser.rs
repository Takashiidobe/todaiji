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

fn parse_for(tokens: &[Token], cursor: &mut usize) -> Result<crate::pagoda::Stmt, ParseError> {
    let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;
    *cursor += 1;

    let open_paren = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;

    // init;
    let init = if matches!(
        tokens.get(*cursor).map(|t| &t.kind),
        Some(TokenKind::Semicolon)
    ) {
        *cursor += 1;
        None
    } else if matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::Let)) {
        // Reuse let parsing logic.
        let let_tok = tokens.get(*cursor).unwrap().clone();
        *cursor += 1;
        let ident = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        let eq = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        let expr = parse_expr(tokens, cursor)?;
        let span = Span {
            start: let_tok.span.start,
            end: expr.span().end,
            literal: format!("{}={}{}", let_tok.span.literal, name, expr.span().literal),
        };
        let semi = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        Some(Box::new(crate::pagoda::Stmt::Let { name, expr, span }))
    } else {
        let expr = parse_expr(tokens, cursor)?;
        let span = expr.span().clone();
        let semi = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        Some(Box::new(crate::pagoda::Stmt::Expr { expr, span }))
    };

    // cond;
    let cond = if matches!(
        tokens.get(*cursor).map(|t| &t.kind),
        Some(TokenKind::Semicolon)
    ) {
        *cursor += 1;
        None
    } else {
        let expr = parse_expr(tokens, cursor)?;
        let semi = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        Some(expr)
    };

    // post )
    let post = if matches!(
        tokens.get(*cursor).map(|t| &t.kind),
        Some(TokenKind::RParen)
    ) {
        *cursor += 1;
        None
    } else {
        let expr = parse_expr(tokens, cursor)?;
        let close = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        Some(expr)
    };

    let body = parse_block(tokens, cursor)?;
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

fn parse_if(tokens: &[Token], cursor: &mut usize) -> Result<crate::pagoda::Stmt, ParseError> {
    let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;
    *cursor += 1;
    let open_paren = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;
    let cond = parse_expr(tokens, cursor)?;
    let close_paren = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;
    let then_block = parse_block(tokens, cursor)?;
    let mut else_block = None;
    if let Some(next) = tokens.get(*cursor) {
        if matches!(next.kind, TokenKind::Else) {
            *cursor += 1;
            if matches!(tokens.get(*cursor).map(|t| &t.kind), Some(TokenKind::If)) {
                let nested_if = parse_if(tokens, cursor)?;
                else_block = Some(Box::new(nested_if));
            } else {
                let block = parse_block(tokens, cursor)?;
                else_block = Some(Box::new(block));
            }
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

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut cursor = 0;
    let mut structs = Vec::new();
    let mut functions = Vec::new();
    let mut stmts = Vec::new();

    loop {
        let Some(token) = tokens.get(cursor) else {
            return Err(ParseError::UnexpectedEof {
                span_start: tokens.last().map(|t| t.span.end).unwrap_or(0),
                span_end: tokens.last().map(|t| t.span.end).unwrap_or(0),
            });
        };

        if matches!(token.kind, TokenKind::Struct) {
            let struct_def = parse_struct(tokens, &mut cursor)?;
            structs.push(struct_def);
            if let Some(next) = tokens.get(cursor) {
                if matches!(next.kind, TokenKind::Semicolon) {
                    cursor += 1;
                } else if matches!(next.kind, TokenKind::Eof) {
                    break;
                }
            } else {
                break;
            }
        } else if matches!(token.kind, TokenKind::Fn) {
            let func = parse_function(tokens, &mut cursor)?;
            functions.push(func);
            if let Some(next) = tokens.get(cursor) {
                if matches!(next.kind, TokenKind::Semicolon) {
                    cursor += 1;
                } else if matches!(next.kind, TokenKind::Eof) {
                    break;
                }
            } else {
                break;
            }
        } else {
            let stmt = parse_block(tokens, &mut cursor)?;
            stmts.push(stmt);
            if let Some(token) = tokens.get(cursor) {
                if matches!(token.kind, TokenKind::Semicolon) {
                    cursor += 1;
                    if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Eof)) {
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

        if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Eof)) {
            break;
        }
    }

    let program_start = stmts
        .first()
        .map(|stmt| stmt.span().start)
        .or_else(|| functions.first().map(|f| f.span.start))
        .or_else(|| structs.first().map(|s| s.span.start))
        .unwrap_or_else(|| tokens.first().map(|t| t.span.start).unwrap_or(0));
    let program_end = tokens.last().map(|t| t.span.end).unwrap_or(program_start);

    Ok(Program {
        structs,
        functions,
        stmts,
        span: Span {
            start: program_start,
            end: program_end,
            literal: String::new(),
        },
    })
}

fn parse_struct(
    tokens: &[Token],
    cursor: &mut usize,
) -> Result<crate::pagoda::StructDef, ParseError> {
    use crate::pagoda::{StructDef, StructField};

    let struct_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;
    *cursor += 1;

    let name_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;

    let lbrace = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;

    let mut fields = Vec::new();
    loop {
        let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
            span_start: lbrace.span.start,
            span_end: lbrace.span.end,
        })?;
        if matches!(tok.kind, TokenKind::RBrace) {
            *cursor += 1;
            break;
        }

        // Parse field: name: type
        let field_name_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;

        // Expect colon
        let colon_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;

        // Parse type (for now, only i64)
        let type_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;

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
        if let Some(next) = tokens.get(*cursor) {
            if matches!(next.kind, TokenKind::Comma) {
                *cursor += 1;
            }
        }
    }

    Ok(StructDef {
        name,
        fields,
        span: Span {
            start: struct_tok.span.start,
            end: tokens
                .get(*cursor - 1)
                .map(|t| t.span.end)
                .unwrap_or(struct_tok.span.end),
            literal: String::new(),
        },
    })
}

fn parse_function(
    tokens: &[Token],
    cursor: &mut usize,
) -> Result<crate::pagoda::Function, ParseError> {
    let fn_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;
    *cursor += 1;
    let name_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;
    let lparen = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
    *cursor += 1;
    let mut params = Vec::new();
    loop {
        let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
            span_start: lparen.span.start,
            span_end: lparen.span.end,
        })?;
        if matches!(tok.kind, TokenKind::RParen) {
            *cursor += 1;
            break;
        }
        let name_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
            span_start: lparen.span.start,
            span_end: lparen.span.end,
        })?;
        let pname = match &name_tok.kind {
            TokenKind::Ident(s) => s.clone(),
            other => {
                return Err(ParseError::ExpectedIdent {
                    span_start: name_tok.span.start,
                    span_end: name_tok.span.end,
                    found: other.clone(),
                });
            }
        };
        params.push(pname);
        *cursor += 1;
        let sep = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
            span_start: lparen.span.start,
            span_end: lparen.span.end,
        })?;
        if matches!(sep.kind, TokenKind::RParen) {
            *cursor += 1;
            break;
        } else if matches!(sep.kind, TokenKind::Comma) {
            *cursor += 1;
            continue;
        } else {
            return Err(ParseError::TrailingTokens {
                span_start: sep.span.start,
                span_end: sep.span.end,
                found: sep.kind.clone(),
            });
        }
    }
    let body = parse_block(tokens, cursor)?;
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
        name,
        params,
        body,
        span,
    })
}

fn parse_stmt(tokens: &[Token], cursor: &mut usize) -> Result<crate::pagoda::Stmt, ParseError> {
    let Some(tok) = tokens.get(*cursor) else {
        return Err(ParseError::UnexpectedEof {
            span_start: 0,
            span_end: 0,
        });
    };

    if matches!(tok.kind, TokenKind::LBrace) {
        parse_block(tokens, cursor)
    } else if matches!(tok.kind, TokenKind::For) {
        parse_for(tokens, cursor)
    } else if matches!(tok.kind, TokenKind::If) {
        parse_if(tokens, cursor)
    } else if matches!(tok.kind, TokenKind::Let) {
        *cursor += 1;
        let ident = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        let eq = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
        *cursor += 1;
        let expr = parse_expr(tokens, cursor)?;
        let span = Span {
            start: tok.span.start,
            end: expr.span().end,
            literal: format!("{}={}{}", tok.span.literal, name, expr.span().literal),
        };
        Ok(crate::pagoda::Stmt::Let { name, expr, span })
    } else if matches!(tok.kind, TokenKind::Return) {
        *cursor += 1;
        let expr = parse_expr(tokens, cursor)?;
        let span = Span {
            start: tok.span.start,
            end: expr.span().end,
            literal: format!("{}{}", tok.span.literal, expr.span().literal),
        };
        Ok(crate::pagoda::Stmt::Return { expr, span })
    } else {
        let expr = parse_expr(tokens, cursor)?;
        let span = expr.span().clone();
        Ok(crate::pagoda::Stmt::Expr { expr, span })
    }
}

fn parse_block(tokens: &[Token], cursor: &mut usize) -> Result<crate::pagoda::Stmt, ParseError> {
    let open = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;
    if !matches!(open.kind, TokenKind::LBrace) {
        return Err(ParseError::ExpectedStatement {
            span_start: open.span.start,
            span_end: open.span.end,
        });
    }
    *cursor += 1;

    let mut stmts = Vec::new();
    loop {
        let Some(tok) = tokens.get(*cursor) else {
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
            *cursor += 1;
            stmts.push(crate::pagoda::Stmt::Empty { span });
            continue;
        }
        let stmt = parse_stmt(tokens, cursor)?;
        stmts.push(stmt);
        if let Some(sep) = tokens.get(*cursor)
            && matches!(sep.kind, TokenKind::Semicolon)
        {
            *cursor += 1;
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

    let close = tokens.get(*cursor).unwrap();
    let span = Span {
        start: open.span.start,
        end: close.span.end,
        literal: open.span.literal.clone()
            + stmts.last().unwrap().span().literal.as_str()
            + &close.span.literal,
    };
    *cursor += 1;
    Ok(crate::pagoda::Stmt::Block { stmts, span })
}

fn parse_expr(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    parse_assign(tokens, cursor)
}

fn parse_assign(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let node = parse_compare(tokens, cursor)?;
    let Some(tok) = tokens.get(*cursor) else {
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
                    *cursor += 1;
                    let rhs = parse_assign(tokens, cursor)?;
                    let rhs_span = rhs.span().clone();
                    let full_span = Span {
                        start: span.start,
                        end: rhs_span.end,
                        literal: format!("{}{}{}", span.literal, op_span.literal, rhs_span.literal),
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
                    *cursor += 1;
                    let rhs = parse_assign(tokens, cursor)?;
                    let rhs_span = rhs.span().clone();
                    let full_span = Span {
                        start: span.start,
                        end: rhs_span.end,
                        literal: format!("{}{}{}", span.literal, op_span.literal, rhs_span.literal),
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
        *cursor += 1;
        let rhs = parse_assign(tokens, cursor)?;
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

fn parse_compare(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_bit_or(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
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
        *cursor += 1;
        let rhs = parse_sum(tokens, cursor)?;
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

fn parse_bit_or(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_bit_xor(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        if !matches!(tok.kind, TokenKind::Pipe) {
            break;
        }
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_bit_xor(tokens, cursor)?;
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

fn parse_bit_xor(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_bit_and(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        if !matches!(tok.kind, TokenKind::Caret) {
            break;
        }
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_bit_and(tokens, cursor)?;
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

fn parse_bit_and(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_shift(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        if !matches!(tok.kind, TokenKind::Amp) {
            break;
        }
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_shift(tokens, cursor)?;
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

fn parse_shift(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_sum(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        let op = match tok.kind {
            TokenKind::Shl => BinOp::Shl,
            TokenKind::Shr => BinOp::Shr,
            _ => break,
        };
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_sum(tokens, cursor)?;
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
}

fn parse_sum(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_term(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        let op = match tok.kind {
            TokenKind::Plus => BinOp::Add,
            TokenKind::Minus => BinOp::Sub,
            _ => break,
        };
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_term(tokens, cursor)?;
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

fn parse_term(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_factor(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        let op = match tok.kind {
            TokenKind::Star => BinOp::Mul,
            TokenKind::Slash => BinOp::Div,
            TokenKind::Percent => BinOp::Mod,
            _ => break,
        };
        let op_span = tok.span.clone();
        *cursor += 1;
        let rhs = parse_factor(tokens, cursor)?;
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

fn parse_factor(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut prefixes: Vec<(UnaryOp, Span)> = Vec::new();
    loop {
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        match tok.kind {
            TokenKind::Plus => {
                prefixes.push((UnaryOp::Plus, tok.span.clone()));
                *cursor += 1;
            }
            TokenKind::Minus => {
                prefixes.push((UnaryOp::Minus, tok.span.clone()));
                *cursor += 1;
            }
            TokenKind::Tilde => {
                prefixes.push((UnaryOp::BitNot, tok.span.clone()));
                *cursor += 1;
            }
            _ => break,
        }
    }

    let token = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;

    let mut node = match &token.kind {
        TokenKind::Int(value) => {
            *cursor += 1;
            Expr::IntLiteral {
                value: *value,
                span: token.span.clone(),
            }
        }
        TokenKind::String(value) => {
            *cursor += 1;
            Expr::StringLiteral {
                value: value.clone(),
                span: token.span.clone(),
            }
        }
        TokenKind::LBracket => {
            *cursor += 1;
            let mut elems = Vec::new();
            let mut end_pos = token.span.end;
            let _ = &end_pos;
            if matches!(
                tokens.get(*cursor).map(|t| &t.kind),
                Some(TokenKind::RBracket)
            ) {
                end_pos = tokens[*cursor].span.end;
                *cursor += 1;
            } else {
                loop {
                    let elem = parse_expr(tokens, cursor)?;
                    end_pos = elem.span().end;
                    elems.push(elem);
                    let sep = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                        span_start: token.span.start,
                        span_end: end_pos,
                    })?;
                    match sep.kind {
                        TokenKind::Comma => {
                            *cursor += 1;
                            continue;
                        }
                        TokenKind::RBracket => {
                            end_pos = sep.span.end;
                            *cursor += 1;
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
            *cursor += 1;
            if let Some(next) = tokens.get(*cursor) {
                if matches!(next.kind, TokenKind::LBrace) {
                    // Struct literal: StructName { field: value, ... }
                    let literal_start_idx = *cursor - 1;
                    *cursor += 1;
                    let mut field_values = Vec::new();
                    let (closing_end, closing_idx) = loop {
                        let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                            span_start: next.span.start,
                            span_end: next.span.end,
                        })?;
                        if matches!(tok.kind, TokenKind::RBrace) {
                            let idx = *cursor;
                            *cursor += 1;
                            break (tok.span.end, idx);
                        }

                        // Parse field_name: expr
                        let field_name_tok =
                            tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                                span_start: next.span.start,
                                span_end: next.span.end,
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
                        *cursor += 1;

                        // Expect colon
                        let colon = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                            span_start: field_name_tok.span.start,
                            span_end: field_name_tok.span.end,
                        })?;
                        if !matches!(colon.kind, TokenKind::Colon) {
                            return Err(ParseError::TrailingTokens {
                                span_start: colon.span.start,
                                span_end: colon.span.end,
                                found: colon.kind.clone(),
                            });
                        }
                        *cursor += 1;

                        // Parse value expression
                        let value_expr = parse_expr(tokens, cursor)?;
                        field_values.push((field_name, value_expr));

                        // Check for comma or closing brace
                        let sep = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                            span_start: next.span.start,
                            span_end: next.span.end,
                        })?;
                        if matches!(sep.kind, TokenKind::Comma) {
                            *cursor += 1;
                        } else if matches!(sep.kind, TokenKind::RBrace) {
                            let idx = *cursor;
                            *cursor += 1;
                            break (sep.span.end, idx);
                        } else {
                            return Err(ParseError::TrailingTokens {
                                span_start: sep.span.start,
                                span_end: sep.span.end,
                                found: sep.kind.clone(),
                            });
                        }
                    };

                    let literal = tokens[literal_start_idx..=closing_idx]
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
                } else if matches!(next.kind, TokenKind::LParen) {
                    *cursor += 1;
                    let mut args = Vec::new();
                    let closing_end = loop {
                        let tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                            span_start: next.span.start,
                            span_end: next.span.end,
                        })?;
                        if matches!(tok.kind, TokenKind::RParen) {
                            *cursor += 1;
                            break tok.span.end;
                        }
                        let arg = parse_expr(tokens, cursor)?;
                        args.push(arg);
                        let sep = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
                            span_start: tok.span.start,
                            span_end: tok.span.end,
                        })?;
                        if matches!(sep.kind, TokenKind::RParen) {
                            *cursor += 1;
                            break sep.span.end;
                        } else if matches!(sep.kind, TokenKind::Comma) {
                            *cursor += 1;
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
        TokenKind::LParen => {
            *cursor += 1;
            let expr = parse_expr(tokens, cursor)?;
            let closing = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
                    *cursor += 1;
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
        let Some(tok) = tokens.get(*cursor) else {
            break;
        };
        if matches!(tok.kind, TokenKind::LBracket) {
            // Array indexing: base[index]
            let open_span = tok.span.clone();
            *cursor += 1;
            let idx_expr = parse_expr(tokens, cursor)?;
            let closing = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
            *cursor += 1;
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
            *cursor += 1;
            let field_tok = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
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
            *cursor += 1;
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

fn expr_with_span(expr: Expr, span: Span) -> Expr {
    match expr {
        Expr::IntLiteral { value, .. } => Expr::IntLiteral { value, span },
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
    }
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
        let tokens = tokenize("fn add(a,b) { a + b } { add(2,3) }").unwrap();
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
