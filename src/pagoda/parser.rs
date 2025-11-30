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
    let token = tokens.get(*cursor).ok_or(ParseError::UnexpectedEof {
        span_start: 0,
        span_end: 0,
    })?;

    match &token.kind {
        TokenKind::Int(value) => {
            *cursor += 1;
            Ok(Expr::IntLiteral {
                value: *value,
                span: token.span.clone(),
            })
        }
        TokenKind::Eof => Err(ParseError::UnexpectedEof {
            span_start: token.span.start,
            span_end: token.span.end,
        }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pagoda::tokenizer::{TokenKind, tokenize};

    #[test]
    fn parses_single_int_literal() {
        let tokens = tokenize("123").unwrap();
        let program = parse_program(&tokens).unwrap();
        assert_eq!(
            program,
            Program {
                span: Span {
                    start: 0,
                    end: 3,
                    literal: "123".to_string()
                },
                expr: Expr::IntLiteral {
                    value: 123,
                    span: Span {
                        start: 0,
                        end: 3,
                        literal: "123".to_string()
                    }
                }
            }
        );
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
}
