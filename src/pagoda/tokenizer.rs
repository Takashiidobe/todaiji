use thiserror::Error;

use crate::pagoda::Span;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Int(i64),
    Plus,
    Minus,
    Star,
    Slash,
    Eof,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TokenizeError {
    #[error("unexpected character '{ch}' at span {span_start}..{span_end}")]
    UnexpectedChar { ch: char, span_start: usize, span_end: usize },
    #[error("invalid integer literal at bytes {span_start}..{span_end}")]
    InvalidInt { span_start: usize, span_end: usize },
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenizeError> {
    let mut tokens = Vec::new();
    let bytes = input.as_bytes();
    let mut idx = 0;

    while idx < bytes.len() {
        let b = bytes[idx];
        if b.is_ascii_whitespace() {
            idx += 1;
            continue;
        }

        if let Some(op) = match b {
            b'+' => Some(TokenKind::Plus),
            b'-' => Some(TokenKind::Minus),
            b'*' => Some(TokenKind::Star),
            b'/' => Some(TokenKind::Slash),
            _ => None,
        } {
            let ch = input[idx..].chars().next().unwrap_or('?');
            let span = Span {
                start: idx,
                end: idx + ch.len_utf8(),
                literal: ch.to_string(),
            };
            tokens.push(Token { kind: op, span });
            idx += 1;
            continue;
        }

        let start = idx;
        if !b.is_ascii_digit() {
            let ch = input[idx..].chars().next().unwrap_or('?');
            let span_end = idx + ch.len_utf8();
            return Err(TokenizeError::UnexpectedChar {
                ch,
                span_start: idx,
                span_end,
            });
        }

        while idx < bytes.len() && bytes[idx].is_ascii_digit() {
            idx += 1;
        }

        let lexeme = &input[start..idx];
        match lexeme.parse::<i64>() {
            Ok(value) => tokens.push(Token {
                kind: TokenKind::Int(value),
                span: Span {
                    start,
                    end: idx,
                    literal: lexeme.to_string(),
                },
            }),
            Err(_) => {
                return Err(TokenizeError::InvalidInt {
                    span_start: start,
                    span_end: idx,
                })
            }
        }
    }

    tokens.push(Token {
        kind: TokenKind::Eof,
        span: Span {
            start: input.len(),
            end: input.len(),
            literal: String::new(),
        },
    });

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use insta::assert_debug_snapshot;

    #[test]
    fn tokenizes_signed_ints() {
        let tokens = tokenize(" -42 ").unwrap();
        assert_debug_snapshot!("tokenizes_signed_ints", tokens);
    }

    #[test]
    fn errors_on_invalid_characters() {
        let err = tokenize("abc").unwrap_err();
        assert!(matches!(
            err,
            TokenizeError::UnexpectedChar {
                span_start: 0, ..
            }
        ));
    }

    #[test]
    fn tokenizes_operators() {
        let tokens = tokenize("1+-*/2").unwrap();
        assert_debug_snapshot!("tokenizes_operators", tokens);
    }
}
