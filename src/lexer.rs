use thiserror::Error;

use crate::{Position, Span, Token, TokenKind};

#[derive(Debug, Error, Clone)]
pub enum LexError {
    #[error("Expected character '{expected}', got '{got}' at {pos}.")]
    ExpectedCharacter {
        pos: Position,
        expected: char,
        got: char,
    },
    #[error("Unexpected character '{ch}' at {pos}.")]
    UnexpectedCharacter { pos: Position, ch: char },
    #[error("Unexpected end-of-file at {pos}.")]
    UnexpectedEndOfFile { pos: Position },
}

pub fn lex<'a>(input: &'a str) -> impl Iterator<Item = Result<Token<'a>, LexError>> + 'a {
    Lexer::new(input)
}

pub struct Lexer<'a> {
    input: &'a str,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            position: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
        }
    }

    fn slice_from_span(&self, span: &Span) -> &'a str {
        &self.input[span.start.offset..span.end.offset]
    }

    fn peek(&self) -> Option<char> {
        self.input[self.position.offset..].chars().next()
    }

    fn expect(&mut self, ch: char) -> Result<(), LexError> {
        let pos = self.position;
        match self.advance() {
            Some(actual) => {
                if actual == ch {
                    Ok(())
                } else {
                    Err(LexError::ExpectedCharacter {
                        pos,
                        expected: ch,
                        got: actual,
                    })
                }
            }
            None => Err(LexError::UnexpectedEndOfFile { pos }),
        }
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        let len = ch.len_utf8();
        self.position.offset += len;
        if ch == '\n' {
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.position.column += 1;
        }
        Some(ch)
    }

    fn advance_while(&mut self, pred: impl Fn(char) -> bool) {
        while self.peek().is_some_and(&pred) {
            self.advance();
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            self.advance_while(|ch| ch.is_whitespace());
            // NOTE: treat comments as whitespace and ignore them
            if self.peek() == Some('#') {
                self.skip_line();
            } else {
                break;
            }
        }
    }

    fn skip_line(&mut self) {
        self.advance_while(|ch| ch != '\n');
    }

    fn collect_string_while(&mut self, pred: impl Fn(char) -> bool) -> Span {
        let start_pos = self.position;
        self.advance_while(pred);
        let end_pos = self.position;

        Span::new(start_pos, end_pos)
    }

    fn lex_token(&mut self) -> Option<Result<Token<'a>, LexError>> {
        self.skip_whitespace_and_comments();

        let Some(ch) = self.peek() else {
            return None;
        };

        let result = if ch.is_alphabetic() || ch == ' ' {
            Ok(self.lex_keyword_or_identifier())
        } else if ch.is_ascii_digit() {
            self.lex_number()
        } else if ch == '"' {
            self.lex_string()
        } else {
            self.lex_special_characters()
        };

        Some(result)
    }

    fn lex_keyword_or_identifier(&mut self) -> Token<'a> {
        let span = self.collect_string_while(|ch| ch.is_alphanumeric() || ch == '_');
        let string = self.slice_from_span(&span);
        let kind = match string {
            "let" => TokenKind::Let,
            "func" => TokenKind::Func,
            "return" => TokenKind::Return,
            _ => return Token::new(TokenKind::Ident, span, Some(string)),
        };
        Token::new(kind, span, None)
    }

    fn lex_number(&mut self) -> Result<Token<'a>, LexError> {
        let span = self.collect_string_while(|ch| ch.is_ascii_digit());
        let string = self.slice_from_span(&span);
        Ok(Token::new(TokenKind::Number, span, Some(string)))
    }

    fn lex_string(&mut self) -> Result<Token<'a>, LexError> {
        self.expect('"')?;
        let span = self.collect_string_while(|ch| ch != '"');
        self.expect('"')?;
        let string = self.slice_from_span(&span);
        Ok(Token::new(TokenKind::String, span, Some(string)))
    }

    fn lex_special_characters(&mut self) -> Result<Token<'a>, LexError> {
        let start = self.position;

        let Some(ch) = self.advance() else {
            return Err(LexError::UnexpectedEndOfFile { pos: start });
        };

        let kind = match ch {
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '*' => TokenKind::Star,
            '/' => TokenKind::Slash,
            '=' => TokenKind::Eq,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            ',' => TokenKind::Comma,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ';' => TokenKind::Semicolon,

            _ => return Err(LexError::UnexpectedCharacter { pos: start, ch }),
        };

        let end = self.position;
        let span = Span::new(start, end);

        Ok(Token::new(kind, span, None))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, LexError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex_token()
    }
}
