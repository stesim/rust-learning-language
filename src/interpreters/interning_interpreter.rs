use std::{collections::HashMap, fmt, rc::Rc};

use thiserror::Error;

use crate::parsers::interning_parser::{Ast, BinOp, Expr, InternId, Interns, Stmt};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(i64),
    String(String),
    Function(Rc<FuncDef>),
}

#[derive(Debug)]
pub struct FuncDef {
    pub params: Vec<InternId>,
    pub body: Stmt,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Number(value) => write!(f, "{value}"),
            Value::String(content) => write!(f, "\"{content}\""),
            Value::Function(def) => {
                write!(f, "func(")?;
                for i in 0..def.params.len() {
                    if i == 0 {
                        write!(f, "_{i}")?;
                    } else {
                        write!(f, ", _{i}")?;
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
    #[error("Symbol is not a function: {name}")]
    NotAFunction { name: String },
    #[error("Invalid argument count; expected {0}, got {1}")]
    InvalidArgumentCount(usize, usize),
}

#[derive(Debug)]
pub struct EvaluationScope {
    symbols: HashMap<InternId, Value>,
}

impl EvaluationScope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Default(Value),
    Return(Value),
}

pub struct Interpreter {
    scopes: Vec<EvaluationScope>,
    pub interns: Interns,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: vec![EvaluationScope::new()],
            interns: Interns::new(),
        }
    }

    /// Evaluates the given syntax tree and returns the value of the last statement.
    pub fn eval(&mut self, ast: &Ast) -> Result<Value, Error> {
        let mut value = ControlFlow::Default(Value::Unit);
        for stmt in ast.iter() {
            value = self.eval_stmt(stmt)?;
        }
        let value = match value {
            ControlFlow::Default(value) => value,
            ControlFlow::Return(value) => value,
        };
        Ok(value)
    }

    /// Evaluates the given statement and returns its value if it is an expression statement or a
    /// unit value otherwise.
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<ControlFlow, Error> {
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
                    self.add_symbol(*name, value);
                    Ok(ControlFlow::Default(Value::Unit))
                } else {
                    let name = self.get_intern_string(name);
                    Err(Error::SymbolExists(name.to_owned()))
                }
            }
            Stmt::Assign { name, value } => {
                let value = self.eval_expr(value)?;
                self.assign_symbol(name, value)?;
                Ok(ControlFlow::Default(Value::Unit))
            }
            Stmt::FuncDef { name, params, body } => {
                if !self.has_local_symbol(name) {
                    let value = Value::Function(Rc::new(FuncDef {
                        params: params.clone(),
                        body: *body.clone(),
                    }));
                    self.add_symbol(*name, value);
                    Ok(ControlFlow::Default(Value::Unit))
                } else {
                    let name = self.get_intern_string(name);
                    Err(Error::SymbolExists(name.to_owned()))
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

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(left, op, right),
            Expr::FunCall(name, args) => self.eval_func(&name, args),
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::String(content) => {
                let content = self.get_intern_string(content);
                Ok(Value::String(content.to_owned()))
            }
            Expr::Variable(name) => self
                .get_symbol(name)
                .cloned()
                .ok_or_else(|| Error::UnknownSymbol(self.get_intern_string(name).to_owned())),
        }
    }

    fn eval_func(&mut self, name: &InternId, args: &Vec<Expr>) -> Result<Value, Error> {
        match self.get_symbol(name) {
            Some(Value::Function(def)) => {
                let def = def.clone();
                self.eval_user_func(&def.params, args, &def.body)
            }
            Some(_) => Err(Error::NotAFunction {
                name: self.get_intern_string(name).to_owned(),
            }),
            None => self.eval_builtin_func(name, args),
        }
    }

    fn eval_user_func(
        &mut self,
        params: &Vec<InternId>,
        args: &Vec<Expr>,
        body: &Stmt,
    ) -> Result<Value, Error> {
        if args.len() != params.len() {
            return Err(Error::InvalidArgumentCount(params.len(), args.len()));
        }

        let args = self.eval_func_args(args)?;
        self.push_scope();
        for (name, value) in params.iter().zip(args.iter()) {
            self.add_symbol(*name, value.clone());
        }

        let result = match self.eval_stmt(body)? {
            ControlFlow::Default(value) => Ok(value),
            ControlFlow::Return(value) => Ok(value),
        };

        self.pop_scope();

        result
    }

    fn eval_builtin_func(&mut self, name: &InternId, args: &Vec<Expr>) -> Result<Value, Error> {
        let name = self.get_intern_string(name);
        match name {
            "print" => self.eval_print(args).map(|_| Value::Unit),
            _ => Err(Error::UnknownSymbol(name.to_owned())),
        }
    }

    fn eval_func_args(&mut self, args: &Vec<Expr>) -> Result<Vec<Value>, Error> {
        args.iter().map(|expr| self.eval_expr(expr)).collect()
    }

    fn eval_binary_op(&mut self, left: &Expr, op: &BinOp, right: &Expr) -> Result<Value, Error> {
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
            _ => Err(Error::UnsupportedBinaryOp),
        }
    }

    fn eval_print(&mut self, args: &Vec<Expr>) -> Result<(), Error> {
        for (i, arg) in args.iter().enumerate() {
            let value = self.eval_expr(arg)?;
            if i > 0 {
                print!(" ");
            }
            match value {
                Value::String(content) => print!("{content}"),
                _ => print!("{value}"),
            }
        }
        print!("\n");
        Ok(())
    }

    fn has_local_symbol(&self, name: &InternId) -> bool {
        self.current_scope().symbols.contains_key(name)
    }

    fn get_symbol(&self, name: &InternId) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.symbols.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn add_symbol(&mut self, name: InternId, value: Value) {
        self.current_scope_mut().symbols.insert(name, value);
    }

    fn assign_symbol(&mut self, name: &InternId, value: Value) -> Result<(), Error> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(stored) = scope.symbols.get_mut(name) {
                *stored = value;
                return Ok(());
            }
        }
        let name = self.get_intern_string(name);
        Err(Error::UnknownSymbol(name.to_owned()))
    }

    fn get_intern_string(&self, id: &InternId) -> &str {
        self.interns.get(id).expect("InternId should be valid")
    }

    fn push_scope(&mut self) {
        self.scopes.push(EvaluationScope::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&self) -> &EvaluationScope {
        self.scopes.last().unwrap()
    }

    fn current_scope_mut(&mut self) -> &mut EvaluationScope {
        self.scopes.last_mut().unwrap()
    }
}
