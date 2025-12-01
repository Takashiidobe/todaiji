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
    let mut stmts = Vec::new();

    loop {
        let stmt = parse_block(tokens, &mut cursor)?;
        stmts.push(stmt);

        let Some(token) = tokens.get(cursor) else {
            return Err(ParseError::UnexpectedEof {
                span_start: tokens.last().map(|t| t.span.end).unwrap_or(0),
                span_end: tokens.last().map(|t| t.span.end).unwrap_or(0),
            });
        };

        match token.kind {
            TokenKind::Semicolon => {
                cursor += 1;
                // Allow a trailing semicolon before EOF.
                if matches!(tokens.get(cursor).map(|t| &t.kind), Some(TokenKind::Eof)) {
                    break;
                }
            }
            TokenKind::Eof => break,
            _ => {
                return Err(ParseError::TrailingTokens {
                    span_start: token.span.start,
                    span_end: token.span.end,
                    found: token.kind.clone(),
                });
            }
        }
    }

    let program_start = stmts
        .first()
        .map(|stmt| stmt.span().start)
        .unwrap_or_else(|| tokens.first().map(|t| t.span.start).unwrap_or(0));
    let program_end = tokens.last().map(|t| t.span.end).unwrap_or(program_start);

    Ok(Program {
        stmts,
        span: Span {
            start: program_start,
            end: program_end,
            literal: String::new(),
        },
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
    if matches!(tok.kind, TokenKind::Assign) {
        // Only identifiers can be assigned to.
        let lhs_span = node.span().clone();
        let name = match node {
            Expr::Var { name, .. } => name,
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
        return Ok(Expr::Assign {
            name,
            value: Box::new(rhs),
            span,
        });
    }
    Ok(node)
}

fn parse_compare(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_sum(tokens, cursor)?;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
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
        TokenKind::Ident(name) => {
            *cursor += 1;
            Expr::Var {
                name: name.clone(),
                span: token.span.clone(),
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
        Expr::Unary { op, expr, .. } => Expr::Unary { op, expr, span },
        Expr::Binary {
            op, left, right, ..
        } => Expr::Binary {
            op,
            left,
            right,
            span,
        },
        Expr::Assign { name, value, .. } => Expr::Assign { name, value, span },
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
}
