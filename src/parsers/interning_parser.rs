use std::{collections::HashMap, num::ParseIntError};

use thiserror::Error;

use crate::lexers::no_copy_lexer::{Error as LexError, Position, Span, Token, TokenKind};

#[derive(Debug, Clone)]
pub enum Stmt {
    NoOp,
    Block(Vec<Stmt>),
    VarDecl {
        name: InternId,
        value: Expr,
    },
    Assign {
        name: InternId,
        value: Expr,
    },
    FuncDef {
        name: InternId,
        params: Vec<InternId>,
        body: Box<Stmt>,
    },
    Return(Option<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    String(InternId),
    Variable(InternId),
    FunCall(InternId, Vec<Expr>),
    BinaryOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternId(usize);

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    LexError(#[from] LexError),
    #[error("Unexpected end of file.")]
    UnexpectedEndOfFile(),
    #[error("Expected {expected}, got {actual} at {position}.")]
    ExpectedToken {
        expected: TokenKind,
        actual: TokenKind,
        position: Position,
    },
    #[error("Unexpected {token} at {position}.")]
    UnexpectedToken {
        token: TokenKind,
        position: Position,
    },
    #[error("Invalid number at {span}.")]
    InvalidNumber { span: Span, source: ParseIntError },
}

pub struct Ast(Vec<Stmt>);

impl Ast {
    pub fn iter(&self) -> impl Iterator<Item = &Stmt> {
        self.0.iter()
    }
}

pub struct Interns(Vec<String>);

impl Interns {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, id: &InternId) -> Option<&str> {
        self.0.get(id.0).map(|s| s.as_str())
    }
}

pub fn parse<'a, I>(input: I) -> Result<(Ast, Interns), Error>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    Parser::new(input).parse(None)
}

pub fn parse_with_interns<'a, I>(input: I, interns: &mut Interns) -> Result<Ast, Error>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    Parser::new(input).parse_with_interns(interns)
}

struct Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    tokens: std::iter::Peekable<I>,
    buffered: Option<Token<'a>>,
    interns: HashMap<String, InternId>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<Token<'a>, LexError>>,
{
    fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            buffered: None,
            interns: HashMap::new(),
        }
    }

    fn parse(&mut self, interns: Option<Interns>) -> Result<(Ast, Interns), Error> {
        let mut ast = Ast(Vec::new());
        let mut interns = interns.unwrap_or_else(|| Interns(Vec::new()));
        self.parse_into(&mut ast, &mut interns)?;
        Ok((ast, interns))
    }

    fn parse_with_interns(&mut self, interns: &mut Interns) -> Result<Ast, Error> {
        let mut ast = Ast(Vec::new());
        self.parse_into(&mut ast, interns)?;
        Ok(ast)
    }

    fn parse_into(&mut self, ast: &mut Ast, interns: &mut Interns) -> Result<(), Error> {
        // move existing interns into inverse map (string -> id)
        for (i, string) in interns.0.drain(..).enumerate() {
            self.interns.insert(string, InternId(i));
        }

        // parse
        while self.tokens.peek().is_some() || self.buffered.is_some() {
            let stmt = self.parse_stmt()?;
            ast.0.push(stmt);
        }

        // move all interns into original vec
        interns.0.resize(self.interns.len(), String::new());
        for (string, i) in self.interns.drain() {
            interns.0[i.0] = string;
        }
        Ok(())
    }

    fn parse_stmt(&mut self) -> Result<Stmt, Error> {
        let next = self.peek()?.kind;
        match next {
            TokenKind::Semicolon => {
                self.advance().unwrap();
                Ok(Stmt::NoOp)
            }
            TokenKind::LBrace => self.parse_block(),
            TokenKind::Let => self.parse_var_decl(),
            TokenKind::Func => self.parse_func_def(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Ident if self.peek_next().is_ok_and(|tok| tok.kind == TokenKind::Eq) => {
                self.parse_assign()
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(TokenKind::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_block(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenKind::LBrace)?;
        let mut statements = Vec::new();
        loop {
            let next = self.peek()?.kind;
            if next != TokenKind::RBrace {
                let stmt = self.parse_stmt()?;
                statements.push(stmt);
            } else {
                break;
            }
        }
        self.expect(TokenKind::RBrace)?;
        Ok(Stmt::Block(statements))
    }

    fn parse_var_decl(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenKind::Let)?;
        let name = self.parse_identifier()?;
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::VarDecl { name, value })
    }

    fn parse_func_def(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenKind::Func)?;
        let name = self.parse_identifier()?;
        self.expect(TokenKind::LParen)?;
        let params = self.parse_parameter_list()?;
        self.expect(TokenKind::RParen)?;
        let body = self.parse_func_body()?;
        Ok(Stmt::FuncDef {
            name,
            params,
            body: Box::new(body),
        })
    }

    fn parse_parameter_list(&mut self) -> Result<Vec<InternId>, Error> {
        let mut params = Vec::new();
        let first = self.peek()?.kind;
        if first != TokenKind::RParen {
            loop {
                let param = self.parse_identifier()?;
                params.push(param);
                let next = self.peek()?.kind;
                if next == TokenKind::Comma {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }
        Ok(params)
    }

    fn parse_func_body(&mut self) -> Result<Stmt, Error> {
        let next = self.peek()?.kind;
        if next == TokenKind::LBrace {
            self.parse_block()
        } else {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semicolon)?;
            Ok(Stmt::Expr(expr))
        }
    }

    fn parse_return(&mut self) -> Result<Stmt, Error> {
        self.expect(TokenKind::Return)?;
        if self.peek()?.kind == TokenKind::Semicolon {
            Ok(Stmt::Return(None))
        } else {
            self.parse_expr().map(|expr| Stmt::Return(Some(expr)))
        }
    }

    fn parse_assign(&mut self) -> Result<Stmt, Error> {
        let name = self.parse_identifier()?;
        self.expect(TokenKind::Eq)?;
        let value = self.parse_expr()?;
        self.expect(TokenKind::Semicolon)?;
        Ok(Stmt::Assign { name, value })
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
        return self.parse_additive_expr();
    }

    fn parse_additive_expr(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_multiplicative_expr()?;
        loop {
            let next = self.peek()?;
            let op = match next.kind {
                TokenKind::Plus => BinOp::Add,
                TokenKind::Minus => BinOp::Sub,
                _ => {
                    break;
                }
            };
            self.advance().unwrap();

            let right = self.parse_multiplicative_expr()?;

            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_multiplicative_expr(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_factor()?;
        loop {
            let next = self.peek()?;
            let op = match next.kind {
                TokenKind::Star => BinOp::Mul,
                TokenKind::Slash => BinOp::Div,
                _ => {
                    break;
                }
            };
            self.advance().unwrap();

            let right = self.parse_factor()?;

            left = Expr::BinaryOp {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
        }
        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<Expr, Error> {
        let token = self.advance()?;
        match token.kind {
            TokenKind::Number => self.parse_number(&token),
            TokenKind::String => {
                let string = token.text.unwrap();
                let intern = self.get_intern(string);
                Ok(Expr::String(intern))
            }
            TokenKind::Ident => {
                let name = token.text.unwrap();
                self.parse_identifier_initiated_expr(name)
            }
            TokenKind::LParen => self.parse_paren_expr(),
            _ => Err(Error::UnexpectedToken {
                token: token.kind,
                position: token.span.start,
            }),
        }
    }

    fn parse_identifier_initiated_expr(&mut self, string: &'a str) -> Result<Expr, Error> {
        let is_function_call = self.peek().is_ok_and(|tok| tok.kind == TokenKind::LParen);
        if is_function_call {
            let args = self.parse_argument_list()?;
            let intern = self.get_intern(string);
            Ok(Expr::FunCall(intern, args))
        } else {
            let intern = self.get_intern(string);
            Ok(Expr::Variable(intern))
        }
    }

    fn parse_paren_expr(&mut self) -> Result<Expr, Error> {
        let expr = self.parse_expr()?;
        self.expect(TokenKind::RParen)?;
        Ok(expr)
    }

    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, Error> {
        self.expect(TokenKind::LParen)?;

        let mut args = Vec::new();

        let first = self.peek()?.kind;
        if first != TokenKind::RParen {
            loop {
                let expr = self.parse_expr()?;
                args.push(expr);
                let next = self.peek()?.kind;
                if next == TokenKind::Comma {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }

        self.expect(TokenKind::RParen)?;

        Ok(args)
    }

    fn parse_number(&mut self, token: &Token<'a>) -> Result<Expr, Error> {
        let string = token.text.unwrap();
        match string.parse::<i64>() {
            Ok(val) => Ok(Expr::Number(val)),
            Err(source) => Err(Error::InvalidNumber {
                span: token.span,
                source,
            }),
        }
    }

    fn parse_identifier(&mut self) -> Result<InternId, Error> {
        let token = self.expect(TokenKind::Ident)?;
        let string = token.text.unwrap();
        let intern = self.get_intern(string);
        Ok(intern)
    }

    fn get_intern(&mut self, text: &str) -> InternId {
        match self.interns.get(text) {
            Some(id) => *id,
            None => {
                let next_id = InternId(self.interns.len());
                self.interns.insert(text.to_owned(), next_id);
                next_id
            }
        }
    }

    fn peek(&mut self) -> Result<&Token<'a>, Error> {
        if let Some(token) = &self.buffered {
            return Ok(token);
        }

        match self.tokens.peek() {
            Some(Ok(spanned)) => Ok(spanned),
            Some(Err(err)) => Err(Error::LexError(err.clone())),
            None => Err(Error::UnexpectedEndOfFile()),
        }
    }

    fn peek_next(&mut self) -> Result<&Token<'a>, Error> {
        if self.buffered.is_none() {
            self.buffered = Some(self.advance()?);
        }

        match self.tokens.peek() {
            Some(Ok(spanned)) => Ok(spanned),
            Some(Err(err)) => Err(Error::LexError(err.clone())),
            None => Err(Error::UnexpectedEndOfFile()),
        }
    }

    fn advance(&mut self) -> Result<Token<'a>, Error> {
        if let Some(token) = self.buffered {
            self.buffered = None;
            return Ok(token);
        }

        match self.tokens.next() {
            Some(Ok(spanned)) => Ok(spanned),
            Some(Err(err)) => Err(Error::LexError(err.clone())),
            None => Err(Error::UnexpectedEndOfFile()),
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Result<Token<'a>, Error> {
        let token = self.advance()?;
        if token.kind == expected {
            Ok(token)
        } else {
            Err(Error::ExpectedToken {
                expected,
                actual: token.kind,
                position: token.span.start,
            })
        }
    }
}
