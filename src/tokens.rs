use std::fmt;

use crate::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind {
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
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let string = match self {
            TokenKind::Let => "let",
            TokenKind::Func => "func",
            TokenKind::Return => "return",
            TokenKind::Ident => return write!(f, "<identifier>"),
            TokenKind::Number => return write!(f, "<number>"),
            TokenKind::String => return write!(f, "<string>"),
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Slash => "/",
            TokenKind::Eq => "=",
            TokenKind::LParen => "(",
            TokenKind::RParen => ")",
            TokenKind::Comma => ",",
            TokenKind::LBrace => "{",
            TokenKind::RBrace => "}",
            TokenKind::Semicolon => ";",
        };
        write!(f, "'{string}'")
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub span: Span,
    pub text: Option<&'a str>,
}

impl<'a> Token<'a> {
    pub fn new(kind: TokenKind, span: Span, text: Option<&'a str>) -> Self {
        Self { kind, span, text }
    }
}
