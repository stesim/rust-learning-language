use std::{collections::HashMap, fmt};

use thiserror::Error;

use crate::parsers::no_copy_parser::{BinOp, Expr, Stmt};

#[derive(Debug, Clone)]
pub enum Value<'a> {
    Unit,
    Number(i64),
    String(String),
    StaticString(&'a str),
    Function {
        params: &'a Vec<&'a str>,
        body: &'a Stmt<'a>,
    },
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(value) => write!(f, "{value}"),
            Value::String(content) => write!(f, "\"{content}\""),
            Value::StaticString(content) => write!(f, "\"{content}\""),
            Value::Function { params, .. } => {
                write!(f, "func(")?;
                for (i, name) in params.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{name}")?;
                    } else {
                        write!(f, ", {name}")?;
                    }
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Symbol already exists: {0}")]
    SymbolExists(String),
    #[error("Unknown symbol: {0}")]
    UnknownSymbol(String),
    #[error("Unsupported binary operation.")]
    UnsupportedBinaryOp,
    #[error("Symbol is not a function: {0}")]
    NotAFunction(String),
    #[error("Invalid argument count; expected {0}, got {1}")]
    InvalidArgumentCount(usize, usize),
}

#[derive(Debug)]
pub struct EvaluationScope<'a> {
    symbols: HashMap<&'a str, Value<'a>>,
}

impl<'a> EvaluationScope<'a> {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlow<'a> {
    Default(Value<'a>),
    Return(Value<'a>),
}

pub struct Interpreter<'a> {
    scopes: Vec<EvaluationScope<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self {
            scopes: vec![EvaluationScope::new()],
        }
    }

    /// Evaluates the given syntax tree and returns the value of the last statement.
    pub fn eval<I>(&mut self, statements: I) -> Result<Value<'a>, Error>
    where
        I: Iterator<Item = &'a Stmt<'a>>,
    {
        let mut value = ControlFlow::Default(Value::Unit);
        for stmt in statements {
            value = self.eval_stmt(stmt)?;
        }
        match value {
            ControlFlow::Default(value) => Ok(value),
            ControlFlow::Return(value) => Ok(value),
        }
    }

    /// Evaluates the given statement and returns its value if it is an expression statement or a
    /// unit value otherwise.
    fn eval_stmt(&mut self, stmt: &'a Stmt<'a>) -> Result<ControlFlow<'a>, Error> {
        match stmt {
            Stmt::Block(sub_statments) => {
                for sub_stmt in sub_statments {
                    let result = self.eval_stmt(sub_stmt)?;
                    match result {
                        ControlFlow::Default(_) => {}
                        ControlFlow::Return(_) => return Ok(result),
                    }
                }
                Ok(ControlFlow::Default(Value::Unit))
            }
            Stmt::Expr(expr) => self.eval_expr(expr).map(ControlFlow::Default),
            Stmt::NoOp => Ok(ControlFlow::Default(Value::Unit)),
            Stmt::VarDecl { name, value } => {
                if !self.has_local_symbol(name) {
                    let value = self.eval_expr(value)?;
                    self.add_symbol(name, value);
                    Ok(ControlFlow::Default(Value::Unit))
                } else {
                    Err(Error::SymbolExists(name.to_string()))
                }
            }
            Stmt::Assign { name, value } => {
                let value = self.eval_expr(value)?;
                self.assign_symbol(name, value)?;
                Ok(ControlFlow::Default(Value::Unit))
            }
            Stmt::FuncDef { name, params, body } => {
                if !self.has_local_symbol(name) {
                    let value = Value::Function { params, body };
                    self.add_symbol(name, value);
                    Ok(ControlFlow::Default(Value::Unit))
                } else {
                    Err(Error::SymbolExists(name.to_string()))
                }
            }
            Stmt::Return(expr) => {
                let value = match expr {
                    Some(expr) => self.eval_expr(expr)?,
                    None => Value::Unit,
                };
                Ok(ControlFlow::Return(value))
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr<'a>) -> Result<Value<'a>, Error> {
        match expr {
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(left, op, right),
            Expr::FunCall(name, args) => self.eval_func(name, args),
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::String(content) => Ok(Value::StaticString(content)),
            Expr::Variable(name) => self
                .get_symbol(name)
                .cloned()
                .ok_or_else(|| Error::UnknownSymbol(name.to_string())),
        }
    }

    fn eval_func(&mut self, name: &str, args: &Vec<Expr<'a>>) -> Result<Value<'a>, Error> {
        match self.get_symbol(name) {
            Some(Value::Function { params, body }) => self.eval_user_func(params, args, body),
            Some(_) => Err(Error::NotAFunction(name.to_string())),
            None => self.eval_builtin_func(name, args),
        }
    }

    fn eval_user_func(
        &mut self,
        params: &Vec<&'a str>,
        args: &Vec<Expr<'a>>,
        body: &'a Stmt<'a>,
    ) -> Result<Value<'a>, Error> {
        if args.len() != params.len() {
            return Err(Error::InvalidArgumentCount(params.len(), args.len()));
        }

        let args = self.eval_func_args(args)?;
        self.push_scope();
        for (name, value) in params.iter().zip(args.iter()) {
            self.add_symbol(name, value.clone());
        }

        let result = match self.eval_stmt(body)? {
            ControlFlow::Default(value) => Ok(value),
            ControlFlow::Return(value) => Ok(value),
        };

        self.pop_scope();

        result
    }

    fn eval_builtin_func(&mut self, name: &str, args: &Vec<Expr<'a>>) -> Result<Value<'a>, Error> {
        match name {
            "print" => self.eval_print(args).map(|_| Value::Unit),
            _ => Err(Error::UnknownSymbol(name.to_owned())),
        }
    }

    fn eval_func_args(&mut self, args: &Vec<Expr<'a>>) -> Result<Vec<Value<'a>>, Error> {
        args.iter().map(|expr| self.eval_expr(expr)).collect()
    }

    fn eval_binary_op(
        &mut self,
        left: &Expr<'a>,
        op: &BinOp,
        right: &Expr<'a>,
    ) -> Result<Value<'a>, Error> {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

        match (&left, &right) {
            (Value::Number(lhs), Value::Number(rhs)) => {
                let result = match op {
                    BinOp::Add => lhs + rhs,
                    BinOp::Sub => lhs - rhs,
                    BinOp::Mul => lhs * rhs,
                    BinOp::Div => lhs / rhs,
                };
                Ok(Value::Number(result))
            }
            (Value::String(lhs), Value::String(rhs)) if op == &BinOp::Add => {
                Ok(Value::String(format!("{lhs}{rhs}")))
            }
            (Value::String(lhs), Value::StaticString(rhs)) if op == &BinOp::Add => {
                Ok(Value::String(format!("{lhs}{rhs}")))
            }
            (Value::StaticString(lhs), Value::String(rhs)) if op == &BinOp::Add => {
                Ok(Value::String(format!("{lhs}{rhs}")))
            }
            (Value::StaticString(lhs), Value::StaticString(rhs)) if op == &BinOp::Add => {
                Ok(Value::String(format!("{lhs}{rhs}")))
            }
            _ => Err(Error::UnsupportedBinaryOp),
        }
    }

    fn eval_print(&mut self, args: &Vec<Expr<'a>>) -> Result<(), Error> {
        for (i, arg) in args.iter().enumerate() {
            let value = self.eval_expr(arg)?;
            if i > 0 {
                print!(" ");
            }
            match value {
                Value::String(content) => print!("{content}"),
                Value::StaticString(content) => print!("{content}"),
                _ => print!("{value}"),
            }
        }
        print!("\n");
        Ok(())
    }

    fn has_local_symbol(&self, name: &str) -> bool {
        self.current_scope().symbols.contains_key(name)
    }

    fn get_symbol(&self, name: &str) -> Option<&Value<'a>> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.symbols.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn add_symbol(&mut self, name: &'a str, value: Value<'a>) {
        self.current_scope_mut().symbols.insert(name, value);
    }

    fn assign_symbol(&mut self, name: &str, value: Value<'a>) -> Result<(), Error> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(stored) = scope.symbols.get_mut(name) {
                *stored = value;
                return Ok(());
            }
        }
        Err(Error::UnknownSymbol(name.to_owned()))
    }

    fn push_scope(&mut self) {
        self.scopes.push(EvaluationScope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&self) -> &EvaluationScope<'a> {
        self.scopes.last().unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut EvaluationScope<'a> {
        self.scopes.last_mut().unwrap()
    }
}
