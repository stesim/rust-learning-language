use std::{char, fmt};

#[derive(Debug, Clone, PartialEq)]
pub struct Node {
    pub token: Token,
    pub location: SourceLocation,
}

impl Node {
    fn new(token: Token, location: SourceLocation) -> Self {
        Self { token, location }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Func,
    Return,
    Ident(String),
    Number(i64),
    String(String),
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
}

pub fn lex(input: &str) -> Vec<Node> {
    Lexer::new(input).tokenize()
}

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    location: SourceLocation,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            location: SourceLocation { line: 1, column: 1 },
        }
    }

    pub fn tokenize(&mut self) -> Vec<Node> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token();
            let is_eof = token.token == Token::EOF;
            tokens.push(token);
            if is_eof {
                break;
            }
        }
        tokens
    }

    pub fn next_token(&mut self) -> Node {
        self.skip_whitespace();

        let Some(ch) = self.peek() else {
            return Node::new(Token::EOF, self.location);
        };

        if ch.is_alphabetic() || ch == ' ' {
            self.lex_keyword_or_identifier()
        } else if ch.is_ascii_digit() {
            self.lex_number()
        } else if ch == '"' {
            self.lex_string()
        } else {
            self.lex_special_characters(ch)
        }
    }

    pub fn peek(&self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    pub fn advance(&mut self) -> Option<char> {
        self.peek().inspect(|ch| {
            self.pos += 1;

            if *ch == '\n' {
                self.location.line += 1;
                self.location.column = 1;
            } else {
                self.location.column += 1;
            }
        })
    }

    pub fn advance_while(&mut self, pred: impl Fn(char) -> bool) {
        while self.peek().is_some_and(&pred) {
            self.advance();
        }
    }

    pub fn collect_string_while(&mut self, pred: impl Fn(char) -> bool) -> String {
        let start_pos = self.pos;
        self.advance_while(pred);
        let end_pos = self.pos;

        let string = self.input[start_pos..end_pos].iter().collect();

        string
    }

    pub fn skip_whitespace(&mut self) {
        self.advance_while(|ch| ch.is_whitespace());
    }

    pub fn lex_keyword_or_identifier(&mut self) -> Node {
        let location = self.location;
        let ident = self.collect_string_while(|ch| ch.is_alphanumeric() || ch == '_');
        let token = match ident.as_str() {
            "let" => Token::Let,
            "func" => Token::Func,
            "return" => Token::Return,
            _ => Token::Ident(ident),
        };
        Node::new(token, location)
    }

    pub fn lex_number(&mut self) -> Node {
        let location = self.location;
        let literal = self.collect_string_while(|ch| ch.is_ascii_digit());
        let value = literal.parse().unwrap();
        Node::new(Token::Number(value), location)
    }

    pub fn lex_string(&mut self) -> Node {
        let location = self.location;
        if self.advance() != Some('"') {
            panic!("Expected '\"'.");
        }
        let literal = self.collect_string_while(|ch| ch != '"');
        if self.advance() != Some('"') {
            panic!("Expected '\"'.");
        }
        Node::new(Token::String(literal), location)
    }

    pub fn lex_special_characters(&mut self, ch: char) -> Node {
        let location = self.location;

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

            _ => panic!("Unexpected character: {ch}"),
        };

        self.advance();

        Node::new(token, location)
    }

    pub fn new_token(&self, kind: Token) -> Node {
        Node::new(kind, self.location)
    }
}
