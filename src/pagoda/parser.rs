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
    #[error("trailing input {found:?} starting at bytes {span_start}..{span_end}")]
    TrailingTokens {
        span_start: usize,
        span_end: usize,
        found: TokenKind,
    },
}

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut cursor = 0;
    let expr = parse_expr(tokens, &mut cursor)?;

    let Some(token) = tokens.get(cursor) else {
        return Err(ParseError::UnexpectedEof {
            span_start: tokens.last().map(|t| t.span.end).unwrap_or(0),
            span_end: tokens.last().map(|t| t.span.end).unwrap_or(0),
        });
    };

    match token.kind {
        TokenKind::Eof => {
            let expr_span = expr.span().clone();
            Ok(Program {
                expr,
                span: Span {
                    start: expr_span.start,
                    end: token.span.end,
                    literal: expr_span.literal,
                },
            })
        }
        _ => Err(ParseError::TrailingTokens {
            span_start: token.span.start,
            span_end: token.span.end,
            found: token.kind.clone(),
        }),
    }
}

fn parse_expr(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    parse_compare(tokens, cursor)
}

fn parse_compare(tokens: &[Token], cursor: &mut usize) -> Result<Expr, ParseError> {
    let mut node = parse_sum(tokens, cursor)?;

    loop {
        let Some(tok) = tokens.get(*cursor) else { break };
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
            literal: format!("{}{}{}", lhs_span.literal, op_span.literal, rhs_span.literal),
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
        let Some(tok) = tokens.get(*cursor) else { break };
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
            literal: format!("{}{}{}", lhs_span.literal, op_span.literal, rhs_span.literal),
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
        let Some(tok) = tokens.get(*cursor) else { break };
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
            literal: format!("{}{}{}", lhs_span.literal, op_span.literal, rhs_span.literal),
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
        let Some(tok) = tokens.get(*cursor) else { break };
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
                    })
                }
            }
        }
        TokenKind::Eof => {
            return Err(ParseError::UnexpectedEof {
                span_start: token.span.start,
                span_end: token.span.end,
            })
        }
        other => {
            return Err(ParseError::ExpectedInt {
                span_start: token.span.start,
                span_end: token.span.end,
                found: other.clone(),
            })
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
        Expr::Unary { op, expr, .. } => Expr::Unary { op, expr, span },
        Expr::Binary { op, left, right, .. } => Expr::Binary { op, left, right, span },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pagoda::tokenizer::{tokenize, TokenKind};
    use insta::assert_debug_snapshot;

    #[test]
    fn parses_single_int_literal() {
        let tokens = tokenize("123").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_single_int_literal", program);
    }

    #[test]
    fn rejects_trailing_tokens() {
        let tokens = vec![
            Token {
                kind: TokenKind::Int(1),
                span: Span {
                    start: 0,
                    end: 1,
                    literal: "1".to_string(),
                },
            },
            Token {
                kind: TokenKind::Int(2),
                span: Span {
                    start: 2,
                    end: 3,
                    literal: "2".to_string(),
                },
            },
            Token {
                kind: TokenKind::Eof,
                span: Span {
                    start: 3,
                    end: 3,
                    literal: String::new(),
                },
            },
        ];

        let err = parse_program(&tokens).unwrap_err();
        assert!(matches!(
            err,
            ParseError::TrailingTokens { span_start: 2, .. }
        ));
    }

    #[test]
    fn parses_precedence() {
        let tokens = tokenize("1+2*3").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_precedence", program.expr);
    }

    #[test]
    fn parses_unary_precedence() {
        let tokens = tokenize("-1+2").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_unary_precedence", program.expr);
    }

    #[test]
    fn parses_parentheses() {
        let tokens = tokenize("-(1+2)*3").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_parentheses", program.expr);
    }

    #[test]
    fn parses_comparisons() {
        let tokens = tokenize("1+2==3*4").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_debug_snapshot!("parses_comparisons", program.expr);
    }
}
