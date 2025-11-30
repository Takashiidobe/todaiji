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

        let start = idx;
        if matches!(b, b'+' | b'-') {
            idx += 1;
            if idx >= bytes.len() || !bytes[idx].is_ascii_digit() {
                let ch = input[start..].chars().next().unwrap_or('?');
                let span_end = start + ch.len_utf8();
                return Err(TokenizeError::UnexpectedChar {
                    ch,
                    span_start: start,
                    span_end,
                });
            }
        } else if !b.is_ascii_digit() {
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

    #[test]
    fn tokenizes_signed_ints() {
        let tokens = tokenize(" -42 ").unwrap();
        assert_eq!(
            tokens,
            vec![
                Token {
                    kind: TokenKind::Int(-42),
                    span: Span {
                        start: 1,
                        end: 4,
                        literal: "-42".to_string()
                    }
                },
                Token {
                    kind: TokenKind::Eof,
                    span: Span {
                        start: 5,
                        end: 5,
                        literal: String::new()
                    }
                }
            ]
        );
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
}
