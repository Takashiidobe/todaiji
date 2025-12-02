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
    Ident(String),
    String(String),
    Let,
    Return,
    If,
    Else,
    For,
    Fn,
    Struct,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    EqEq,
    NotEq,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    Shl,
    Shr,
    ShlAssign,
    ShrAssign,
    Comma,
    AmpAssign,
    PipeAssign,
    CaretAssign,
    Amp,
    AmpAmp,
    Pipe,
    PipePipe,
    Caret,
    Tilde,
    Bang,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Colon,
    Dot,
    Eof,
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum TokenizeError {
    #[error("unexpected character '{ch}' at span {span_start}..{span_end}")]
    UnexpectedChar {
        ch: char,
        span_start: usize,
        span_end: usize,
    },
    #[error("invalid integer literal at bytes {span_start}..{span_end}")]
    InvalidInt { span_start: usize, span_end: usize },
    #[error("unterminated string literal starting at bytes {span_start}")]
    UnterminatedString { span_start: usize, span_end: usize },
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

        if b == b'"' {
            let start = idx;
            idx += 1;
            let mut value = String::new();
            let mut closed = false;
            while idx < bytes.len() {
                let ch = input[idx..].chars().next().unwrap();
                if ch == '"' {
                    idx += ch.len_utf8();
                    closed = true;
                    break;
                }
                if ch == '\\' {
                    idx += ch.len_utf8();
                    if idx >= bytes.len() {
                        break;
                    }
                    let escape = input[idx..].chars().next().unwrap();
                    idx += escape.len_utf8();
                    let resolved = match escape {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        other => other,
                    };
                    value.push(resolved);
                    continue;
                }
                value.push(ch);
                idx += ch.len_utf8();
            }
            if !closed {
                return Err(TokenizeError::UnterminatedString {
                    span_start: start,
                    span_end: idx,
                });
            }
            let end = idx;
            tokens.push(Token {
                kind: TokenKind::String(value),
                span: Span {
                    start,
                    end,
                    literal: input[start..end].to_string(),
                },
            });
            continue;
        }

        // Three-character operators
        if idx + 2 < bytes.len() {
            let three = &bytes[idx..idx + 3];
            if three == b"<<=" {
                let span = Span {
                    start: idx,
                    end: idx + 3,
                    literal: "<<=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::ShlAssign,
                    span,
                });
                idx += 3;
                continue;
            } else if three == b">>=" {
                let span = Span {
                    start: idx,
                    end: idx + 3,
                    literal: ">>=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::ShrAssign,
                    span,
                });
                idx += 3;
                continue;
            }
        }

        // Two-character operators
        if idx + 1 < bytes.len() {
            let two = &bytes[idx..idx + 2];
            if two == b"==" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "==".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::EqEq,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"!=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "!=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::NotEq,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"<=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "<=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::LessEq,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b">=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: ">=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::GreaterEq,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"+=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "+=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::PlusAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"-=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "-=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::MinusAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"*=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "*=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::StarAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"/=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "/=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::SlashAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"%=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "%=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::PercentAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"<<" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "<<".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::Shl,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b">>" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: ">>".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::Shr,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"<<=" {
                let span = Span {
                    start: idx,
                    end: idx + 3,
                    literal: "<<=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::ShlAssign,
                    span,
                });
                idx += 3;
                continue;
            } else if two == b">>=" {
                let span = Span {
                    start: idx,
                    end: idx + 3,
                    literal: ">>=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::ShrAssign,
                    span,
                });
                idx += 3;
                continue;
            } else if two == b"&=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "&=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::AmpAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"|=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "|=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::PipeAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"^=" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "^=".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::CaretAssign,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"&&" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "&&".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::AmpAmp,
                    span,
                });
                idx += 2;
                continue;
            } else if two == b"||" {
                let span = Span {
                    start: idx,
                    end: idx + 2,
                    literal: "||".to_string(),
                };
                tokens.push(Token {
                    kind: TokenKind::PipePipe,
                    span,
                });
                idx += 2;
                continue;
            }
        }

        if let Some(op) = match b {
            b'+' => Some(TokenKind::Plus),
            b'-' => Some(TokenKind::Minus),
            b'*' => Some(TokenKind::Star),
            b'/' => Some(TokenKind::Slash),
            b'%' => Some(TokenKind::Percent),
            b'<' => Some(TokenKind::Less),
            b'>' => Some(TokenKind::Greater),
            b'=' => Some(TokenKind::Assign),
            b'&' => Some(TokenKind::Amp),
            b'|' => Some(TokenKind::Pipe),
            b'^' => Some(TokenKind::Caret),
            b'~' => Some(TokenKind::Tilde),
            b'!' => Some(TokenKind::Bang),
            b'(' => Some(TokenKind::LParen),
            b')' => Some(TokenKind::RParen),
            b'{' => Some(TokenKind::LBrace),
            b'}' => Some(TokenKind::RBrace),
            b'[' => Some(TokenKind::LBracket),
            b']' => Some(TokenKind::RBracket),
            b';' => Some(TokenKind::Semicolon),
            b',' => Some(TokenKind::Comma),
            b':' => Some(TokenKind::Colon),
            b'.' => Some(TokenKind::Dot),
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

        if b.is_ascii_digit() {
            let start = idx;
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
                    });
                }
            }
            continue;
        }

        if b.is_ascii_alphabetic() || b == b'_' {
            let start = idx;
            idx += 1;
            while idx < bytes.len() && (bytes[idx].is_ascii_alphanumeric() || bytes[idx] == b'_') {
                idx += 1;
            }
            let lexeme = &input[start..idx];
            let kind = match lexeme {
                "let" => TokenKind::Let,
                "return" => TokenKind::Return,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "for" => TokenKind::For,
                "fn" => TokenKind::Fn,
                "struct" => TokenKind::Struct,
                _ => TokenKind::Ident(lexeme.to_string()),
            };
            tokens.push(Token {
                kind,
                span: Span {
                    start,
                    end: idx,
                    literal: lexeme.to_string(),
                },
            });
            continue;
        }

        let ch = input[idx..].chars().next().unwrap_or('?');
        let span_end = idx + ch.len_utf8();
        return Err(TokenizeError::UnexpectedChar {
            ch,
            span_start: idx,
            span_end,
        });
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
        let err = tokenize("@").unwrap_err();
        assert!(matches!(
            err,
            TokenizeError::UnexpectedChar { span_start: 0, .. }
        ));
    }

    #[test]
    fn tokenizes_operators() {
        let tokens = tokenize("1+-*/%2").unwrap();
        assert_debug_snapshot!("tokenizes_operators", tokens);
    }

    #[test]
    fn tokenizes_parens() {
        let tokens = tokenize("(1)").unwrap();
        assert_debug_snapshot!("tokenizes_parens", tokens);
    }

    #[test]
    fn tokenizes_braces() {
        let tokens = tokenize("{1}").unwrap();
        assert_debug_snapshot!("tokenizes_braces", tokens);
    }

    #[test]
    fn tokenizes_identifiers_and_keywords() {
        let tokens = tokenize("let foo = return").unwrap();
        assert_debug_snapshot!("tokenizes_identifiers_and_keywords", tokens);
    }

    #[test]
    fn tokenizes_return_keyword() {
        let tokens = tokenize("return 1").unwrap();
        assert_debug_snapshot!("tokenizes_return_keyword", tokens);
    }

    #[test]
    fn tokenizes_if_keyword() {
        let tokens = tokenize("if (1) {}").unwrap();
        assert_debug_snapshot!("tokenizes_if_keyword", tokens);
    }

    #[test]
    fn tokenizes_else_keyword() {
        let tokens = tokenize("else { }").unwrap();
        assert_debug_snapshot!("tokenizes_else_keyword", tokens);
    }

    #[test]
    fn tokenizes_fn_keyword() {
        let tokens = tokenize("fn foo() {}").unwrap();
        assert_debug_snapshot!("tokenizes_fn_keyword", tokens);
    }

    #[test]
    fn tokenizes_for_keyword() {
        let tokens = tokenize("for ( ; )").unwrap();
        assert_debug_snapshot!("tokenizes_for_keyword", tokens);
    }

    #[test]
    fn tokenizes_semicolons() {
        let tokens = tokenize("1;2").unwrap();
        assert_debug_snapshot!("tokenizes_semicolons", tokens);
    }

    #[test]
    fn tokenizes_assignments() {
        let tokens = tokenize("i = i + 1").unwrap();
        assert_debug_snapshot!("tokenizes_assignments", tokens);
    }

    #[test]
    fn tokenizes_compound_assignments() {
        let tokens = tokenize("a+=1 b-=2 c*=3 d/=4 e%=5").unwrap();
        assert_debug_snapshot!("tokenizes_compound_assignments", tokens);
    }

    #[test]
    fn tokenizes_bitwise_ops() {
        let tokens = tokenize("a&b|c^~d").unwrap();
        assert_debug_snapshot!("tokenizes_bitwise_ops", tokens);
    }

    #[test]
    fn tokenizes_shifts() {
        let tokens = tokenize("a<<b c>>=d e<<=f").unwrap();
        assert_debug_snapshot!("tokenizes_shifts", tokens);
    }

    #[test]
    fn tokenizes_commas_in_fn() {
        let tokens = tokenize("fn f(a,b,c) {}").unwrap();
        assert_debug_snapshot!("tokenizes_commas_in_fn", tokens);
    }

    #[test]
    fn tokenizes_comparisons() {
        let tokens = tokenize("1==2!=3<=4>=5<6>7").unwrap();
        assert_debug_snapshot!("tokenizes_comparisons", tokens);
    }
}
