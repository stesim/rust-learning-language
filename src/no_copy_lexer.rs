use std::fmt;

use thiserror::Error;

#[derive(Debug, Clone, Copy)]
pub enum Token<'a> {
    Let,
    Func,
    Return,
    Ident(&'a str),
    Number(i64),
    String(&'a str),
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    LParen,
    RParen,
    Comma,
    LBrace,
    RBrace,
    Semicolon,
    EOF,
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Token::Let => "let",
            Token::Func => "func",
            Token::Return => "return",
            Token::Ident(name) => return write!(f, "'{name}'"),
            Token::Number(value) => return write!(f, "'{value}'"),
            Token::String(content) => return write!(f, "\"{content}\""),
            Token::Plus => "+",
            Token::Minus => "-",
            Token::Star => "*",
            Token::Slash => "/",
            Token::Eq => "=",
            Token::LParen => "(",
            Token::RParen => ")",
            Token::Comma => ",",
            Token::LBrace => "{",
            Token::RBrace => "}",
            Token::Semicolon => ";",
            Token::EOF => return write!(f, "EOF"),
        };
        write!(f, "'{string}'")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} to {}", self.start, self.end)
    }
}

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    fn from_positions(node: T, start: Position, end: Position) -> Self {
        Self::new(node, Span::new(start, end))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(Debug, Error)]
pub enum Error {
    #[error("Expected character '{expected}', got '{got}' at {pos}.")]
    ExpectedCharacter {
        pos: Position,
        expected: char,
        got: char,
    },
    #[error("Unexpected character '{ch}' at {pos}.")]
    UnexpectedCharacter { pos: Position, ch: char },
    #[error("Invalid number literal {literal} at {span}")]
    InvalidNumber { span: Span, literal: String },
    #[error("Unexpected end-of-file at {pos}.")]
    UnexpectedEndOfFile { pos: Position },
}

pub fn lex<'a>(input: &'a str) -> impl Iterator<Item = Result<Spanned<Token<'a>>, Error>> + 'a {
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

    fn expect(&mut self, ch: char) -> Result<(), Error> {
        let pos = self.position;
        match self.advance() {
            Some(actual) => {
                if actual == ch {
                    Ok(())
                } else {
                    Err(Error::ExpectedCharacter {
                        pos,
                        expected: ch,
                        got: actual,
                    })
                }
            }
            None => Err(Error::UnexpectedEndOfFile { pos }),
        }
    }

    fn advance(&mut self) -> Option<char> {
        self.peek().inspect(|ch| {
            let len = ch.len_utf8();
            self.position.offset += len;
            if *ch == '\n' {
                self.position.line += 1;
                self.position.column = 1;
            } else {
                self.position.column += 1;
            }
        })
    }

    fn advance_while(&mut self, pred: impl Fn(char) -> bool) {
        while self.peek().is_some_and(&pred) {
            self.advance();
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(|ch| ch.is_whitespace());
    }

    fn collect_string_while(&mut self, pred: impl Fn(char) -> bool) -> Span {
        let start_pos = self.position;
        self.advance_while(pred);
        let end_pos = self.position;

        Span::new(start_pos, end_pos)
    }

    fn lex_token(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        self.skip_whitespace();

        let Some(ch) = self.peek() else {
            let span = Span::new(self.position, self.position);
            return Ok(Spanned::new(Token::EOF, span));
        };

        if ch.is_alphabetic() || ch == ' ' {
            Ok(self.lex_keyword_or_identifier())
        } else if ch.is_ascii_digit() {
            self.lex_number()
        } else if ch == '"' {
            self.lex_string()
        } else {
            self.lex_special_characters()
        }
    }

    fn lex_keyword_or_identifier(&mut self) -> Spanned<Token<'a>> {
        let span = self.collect_string_while(|ch| ch.is_alphanumeric() || ch == '_');
        let string = self.slice_from_span(&span);
        let token = match string {
            "let" => Token::Let,
            "func" => Token::Func,
            "return" => Token::Return,
            _ => Token::Ident(string),
        };
        Spanned::new(token, span)
    }

    fn lex_number(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        let span = self.collect_string_while(|ch| ch.is_ascii_digit());
        let string = self.slice_from_span(&span);
        match string.parse() {
            Ok(value) => Ok(Spanned::new(Token::Number(value), span)),
            Err(_) => Err(Error::InvalidNumber {
                span,
                literal: string.to_owned(),
            }),
        }
    }

    fn lex_string(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        self.expect('"')?;
        let span = self.collect_string_while(|ch| ch != '"');
        self.expect('"')?;
        let string = self.slice_from_span(&span);
        Ok(Spanned::new(Token::String(string), span))
    }

    fn lex_special_characters(&mut self) -> Result<Spanned<Token<'a>>, Error> {
        let start = self.position;

        let Some(ch) = self.advance() else {
            return Err(Error::UnexpectedEndOfFile { pos: start });
        };

        let token = match ch {
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Star,
            '/' => Token::Slash,
            '=' => Token::Eq,
            '(' => Token::LParen,
            ')' => Token::RParen,
            ',' => Token::Comma,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            ';' => Token::Semicolon,

            _ => return Err(Error::UnexpectedCharacter { pos: start, ch }),
        };

        self.advance();

        let end = self.position;

        Ok(Spanned::from_positions(token, start, end))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Spanned<Token<'a>>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_token() {
            Ok(Spanned {
                node: Token::EOF, ..
            }) => None,
            result => Some(result),
        }
    }
}
