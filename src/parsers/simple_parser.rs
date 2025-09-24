use std::fmt;

use crate::lexers::simple_lexer::{Node, SourceLocation, Token};

#[derive(Debug, Clone, PartialEq)]
pub struct AST(pub Vec<Stmt>);

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    NoOp,
    Block(Vec<Stmt>),
    VarDecl {
        name: String,
        value: Expr,
    },
    Assign {
        name: String,
        value: Expr,
    },
    FuncDef {
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
    },
    Return(Option<Expr>),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Number(i64),
    String(String),
    Variable(String),
    FunCall(String, Vec<Expr>),
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

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ParseError {
    ExpectedToken(Token, Token, SourceLocation),
    UnexpectedToken(&'static str, Token, SourceLocation),
    MissingToken(Token),
    UnexpectedEndOfFile(&'static str),
}

impl std::error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ParseError::ExpectedToken(expected, got, loc) => {
                write!(
                    f,
                    "Parse error at line {}, column {}:\n\tExpected {}, got {}.",
                    loc.line, loc.column, expected, got
                )
            }
            ParseError::UnexpectedToken(expected, got, loc) => {
                write!(
                    f,
                    "Parse error at line {}, column {}:\n\tExpected {}, got {}.",
                    loc.line, loc.column, expected, got
                )
            }
            ParseError::MissingToken(expected) => {
                write!(f, "Parse error:\n\tExpected {}, got nothing.", expected)
            }
            ParseError::UnexpectedEndOfFile(expected) => {
                write!(f, "Parse error:\n\tExpected {}, got end of file.", expected)
            }
        }
    }
}

pub fn parse(tokens: Vec<Node>) -> Result<AST, ParseError> {
    Parser::new(tokens).parse()
}

pub struct Parser {
    tokens: Vec<Node>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Node>) -> Self {
        Self { tokens, pos: 0 }
    }

    pub fn peek_token(&self) -> Option<&Token> {
        self.peek().map(|n| &n.token)
    }

    pub fn peek(&self) -> Option<&Node> {
        self.tokens.get(self.pos)
    }

    pub fn peek_or_error(&self, err: &'static str) -> Result<&Node, ParseError> {
        self.peek().ok_or(ParseError::UnexpectedEndOfFile(err))
    }

    pub fn peek_next(&self) -> Option<&Node> {
        self.tokens.get(self.pos + 1)
    }

    pub fn peek_next_or_error(&self, err: &'static str) -> Result<&Node, ParseError> {
        self.peek_next().ok_or(ParseError::UnexpectedEndOfFile(err))
    }

    pub fn advance(&mut self) -> Result<Node, ParseError> {
        self.advance_or_error("token")
    }

    pub fn advance_or_error(&mut self, err: &'static str) -> Result<Node, ParseError> {
        let node = self
            .peek()
            .cloned()
            .ok_or(ParseError::UnexpectedEndOfFile(err));
        self.pos += 1;
        node
    }

    pub fn expect(&mut self, expected: &Token) -> Result<(), ParseError> {
        match self.advance() {
            Ok(node) if &node.token == expected => Ok(()),
            Ok(node) => Err(ParseError::ExpectedToken(
                expected.clone(),
                node.token,
                node.location,
            )),
            Err(_) => Err(ParseError::MissingToken(expected.clone())),
        }
    }

    pub fn parse(&mut self) -> Result<AST, ParseError> {
        let mut statements = Vec::new();
        loop {
            let next = self.peek();
            if next.is_none_or(|next| next.token == Token::EOF) {
                break;
            }
            let stmt = self.parse_stmt()?;
            statements.push(stmt);
        }
        Ok(AST(statements))
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let node =
            self.peek_or_error("identifier, expression, 'let', 'func', 'return', ';' or '{'")?;

        match node.token {
            Token::Semicolon => {
                self.advance().unwrap();
                Ok(Stmt::NoOp)
            }
            Token::LBrace => self.parse_block(),
            Token::Let => self.parse_var_decl(),
            Token::Func => self.parse_func_def(),
            Token::Return => self.parse_return(),
            Token::Ident(_) if self.peek_next().is_some_and(|n| n.token == Token::Eq) => {
                self.parse_assign()
            }
            _ => {
                let expr = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    pub fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.expect(&Token::LBrace)?;
        let mut statements = Vec::new();
        loop {
            let next = self.peek_or_error("statement or '}'")?;
            if next.token != Token::RBrace {
                let stmt = self.parse_stmt();
                if let Err(ParseError::UnexpectedToken(_, tok, loc)) = stmt {
                    return Err(ParseError::UnexpectedToken("statement or '}'", tok, loc));
                }
                statements.push(stmt?);
            } else {
                break;
            }
        }
        self.expect(&Token::RBrace)?;
        Ok(Stmt::Block(statements))
    }

    pub fn parse_assign(&mut self) -> Result<Stmt, ParseError> {
        let name = self.parse_identifier()?;
        self.expect(&Token::Eq)?;
        let value = self.parse_expr()?;
        self.expect(&Token::Semicolon)?;
        Ok(Stmt::Assign { name, value })
    }

    pub fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        return self.parse_additive_expr();
    }

    pub fn parse_additive_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_multiplicative_expr()?;
        loop {
            let next = self.peek_or_error("token")?;
            let op = match next.token {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
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

    pub fn parse_multiplicative_expr(&mut self) -> Result<Expr, ParseError> {
        let mut left = self.parse_factor()?;
        loop {
            let next = self.peek_or_error("token")?;
            let op = match next.token {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
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

    pub fn parse_factor(&mut self) -> Result<Expr, ParseError> {
        let node = self.advance_or_error("number, identifier or '('")?;
        match node.token {
            Token::Number(value) => Ok(Expr::Number(value)),
            Token::String(content) => Ok(Expr::String(content)),
            Token::Ident(ident) => {
                let is_function_call = self.peek().is_some_and(|n| n.token == Token::LParen);
                if is_function_call {
                    let args = self.parse_argument_list()?;
                    Ok(Expr::FunCall(ident, args))
                } else {
                    Ok(Expr::Variable(ident))
                }
            }
            Token::LParen => {
                let expr = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                Ok(expr)
            }
            token => Err(ParseError::UnexpectedToken(
                "number, identifier or '('",
                token,
                node.location,
            )),
        }
    }

    pub fn parse_var_decl(&mut self) -> Result<Stmt, ParseError> {
        self.expect(&Token::Let)?;

        let name = self.parse_identifier()?;

        self.expect(&Token::Eq)?;

        let value = self.parse_expr()?;

        self.expect(&Token::Semicolon)?;

        Ok(Stmt::VarDecl { name, value })
    }

    pub fn parse_func_def(&mut self) -> Result<Stmt, ParseError> {
        self.expect(&Token::Func)?;

        let name = self.parse_identifier()?;

        self.expect(&Token::LParen)?;

        let params = self.parse_parameter_list()?;

        self.expect(&Token::RParen)?;

        let body = self.parse_func_body()?;

        Ok(Stmt::FuncDef {
            name,
            params,
            body: Box::new(body),
        })
    }

    pub fn parse_func_body(&mut self) -> Result<Stmt, ParseError> {
        let next = &self.peek_or_error("expression or '{'")?.token;
        if next == &Token::LBrace {
            self.parse_block()
        } else {
            let expr = self.parse_expr()?;
            self.expect(&Token::Semicolon)?;
            Ok(Stmt::Expr(expr))
        }
    }

    pub fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        self.expect(&Token::Return)?;
        if self.peek_or_error("expression or ';'")?.token == Token::Semicolon {
            Ok(Stmt::Return(None))
        } else {
            self.parse_expr().map(|expr| Stmt::Return(Some(expr)))
        }
    }

    pub fn parse_parameter_list(&mut self) -> Result<Vec<String>, ParseError> {
        let mut params = Vec::new();

        let node = self.peek_or_error("identifier or ')'")?;
        if node.token != Token::RParen {
            loop {
                let param = self.parse_identifier()?;
                params.push(param);
                let node = self.peek_or_error("',' or ')'")?;
                if node.token == Token::Comma {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }

        Ok(params)
    }

    pub fn parse_identifier(&mut self) -> Result<String, ParseError> {
        let ident_node = self.advance_or_error("identifier")?;
        let Token::Ident(ident) = ident_node.token else {
            return Err(ParseError::UnexpectedToken(
                "identifier",
                ident_node.token,
                ident_node.location,
            ));
        };
        Ok(ident)
    }

    pub fn parse_argument_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        self.expect(&Token::LParen)?;

        let mut args = Vec::new();

        let node = self.peek_or_error("expression or ')'")?;
        if node.token != Token::RParen {
            loop {
                let expr = self.parse_expr()?;
                args.push(expr);
                let node = self.peek_or_error("',' or ')'")?;
                if node.token == Token::Comma {
                    self.advance().unwrap();
                } else {
                    break;
                }
            }
        }

        self.expect(&Token::RParen)?;

        Ok(args)
    }
}
