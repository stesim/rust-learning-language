use std::fmt;

use thiserror::Error;

const MAX_KEYWORD_LENGTH: usize = 6;

#[derive(Debug, Clone, Copy)]
pub enum Token {
    Let,
    Func,
    Return,
    Ident,
    Number,
    String,
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

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            Token::Let => "let",
            Token::Func => "func",
            Token::Return => "return",
            Token::Ident => return write!(f, "<identifier>"),
            Token::Number => return write!(f, "<number>"),
            Token::String => return write!(f, "<string>"),
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

pub fn lex<I>(input: I) -> impl Iterator<Item = Result<Spanned<Token>, Error>>
where
    I: Iterator<Item = char>,
{
    Lexer::new(input)
}

pub struct Lexer<I>
where
    I: Iterator<Item = char>,
{
    input: std::iter::Peekable<I>,
    position: Position,
}

impl<I> Lexer<I>
where
    I: Iterator<Item = char>,
{
    pub fn new(input: I) -> Self {
        Self {
            input: input.peekable(),
            position: Position {
                line: 1,
                column: 1,
                offset: 0,
            },
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.input.peek().copied()
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
        let ch = self.input.next()?;
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

    fn skip_whitespace(&mut self) {
        self.advance_while(|ch| ch.is_whitespace());
    }

    fn span_while(&mut self, pred: impl Fn(char) -> bool) -> Span {
        let start_pos = self.position;
        self.advance_while(pred);
        let end_pos = self.position;

        Span::new(start_pos, end_pos)
    }

    fn lex_token(&mut self) -> Result<Spanned<Token>, Error> {
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

    fn lex_keyword_or_identifier(&mut self) -> Spanned<Token> {
        let pred = |ch: char| ch.is_alphanumeric() || ch == '_';

        let start = self.position;

        let mut len: usize = 0;
        let mut buf: [char; MAX_KEYWORD_LENGTH] = ['\0'; MAX_KEYWORD_LENGTH];
        for i in 0..len {
            if self.peek().is_some_and(pred) {
                buf[i] = self.advance().unwrap();
                len += 1;
            } else {
                break;
            }
        }

        while self.peek().is_some_and(&pred) {
            self.advance();
            len += 1;
        }

        let end = self.position;

        let token = if len <= MAX_KEYWORD_LENGTH {
            match &buf[..len] {
                ['l', 'e', 't', ..] => Token::Let,
                ['f', 'u', 'n', 'c', ..] => Token::Func,
                ['r', 'e', 't', 'u', 'r', 'n', ..] => Token::Return,
                _ => Token::Ident,
            }
        } else {
            Token::Ident
        };

        Spanned::from_positions(token, start, end)
    }

    fn lex_number(&mut self) -> Result<Spanned<Token>, Error> {
        let span = self.span_while(|ch| ch.is_ascii_digit());
        Ok(Spanned::new(Token::Number, span))
    }

    fn lex_string(&mut self) -> Result<Spanned<Token>, Error> {
        self.expect('"')?;
        let span = self.span_while(|ch| ch != '"');
        self.expect('"')?;
        Ok(Spanned::new(Token::String, span))
    }

    fn lex_special_characters(&mut self) -> Result<Spanned<Token>, Error> {
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

        let end = self.position;

        Ok(Spanned::from_positions(token, start, end))
    }
}

impl<I> Iterator for Lexer<I>
where
    I: Iterator<Item = char>,
{
    type Item = Result<Spanned<Token>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.lex_token() {
            Ok(Spanned {
                node: Token::EOF, ..
            }) => None,
            result => Some(result),
        }
    }
}
