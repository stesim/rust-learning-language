use std::collections::HashMap;

use thiserror::Error;

use crate::parser::{AST, BinOp, Expr, Stmt};

#[derive(Debug, Clone)]
pub enum Value {
    Unit,
    Number(i64),
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("Symbol already exists: {0}")]
    SymbolExists(String),
    #[error("Unknown symbol: {0}")]
    UnknownSymbol(String),
    #[error("Unsupported binary operation: {left:?} {op} {right:?}")]
    UnsupportedBinaryOp {
        left: Value,
        op: BinOp,
        right: Value,
    },
}

pub struct Interpreter {
    symbols: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }

    pub fn eval(&mut self, ast: AST) -> Result<(), Error> {
        for stmt in &ast.0 {
            self.eval_stmt(stmt)?;
        }
        Ok(())
    }

    pub fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Block(sub_statments) => {
                for sub_stmt in sub_statments {
                    self.eval_stmt(sub_stmt)?;
                }
                Ok(())
            }
            Stmt::Expr(expr) => self.eval_expr(expr).map(drop),
            Stmt::NoOp => Ok(()),
            Stmt::VarDecl { name, value } => {
                if self.symbols.contains_key(name) {
                    return Err(Error::SymbolExists(name.clone()));
                }
                let value = self.eval_expr(value)?;
                self.symbols.insert(name.clone(), value);
                Ok(())
            }
        }
    }

    pub fn eval_expr(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::BinaryOp { left, op, right } => self.eval_binary_op(left, op, right),
            Expr::FunCall(name, args) => {
                // FIXME: handle function calls properly
                if name == "print" {
                    self.eval_print(args).map(|_| Value::Unit)
                } else {
                    Err(Error::UnknownSymbol(name.clone()))
                }
            }
            Expr::Number(value) => Ok(Value::Number(*value)),
            Expr::Variable(name) => self
                .symbols
                .get(name)
                .cloned()
                .ok_or_else(|| Error::UnknownSymbol(name.clone())),
        }
    }

    pub fn eval_binary_op(
        &mut self,
        left: &Expr,
        op: &BinOp,
        right: &Expr,
    ) -> Result<Value, Error> {
        let left = self.eval_expr(left)?;
        let right = self.eval_expr(right)?;

        if let (Value::Number(lhs), Value::Number(rhs)) = (&left, &right) {
            let result = match op {
                BinOp::Add => lhs + rhs,
                BinOp::Sub => lhs - rhs,
                BinOp::Mul => lhs * rhs,
                BinOp::Div => lhs / rhs,
            };
            Ok(Value::Number(result))
        } else {
            Err(Error::UnsupportedBinaryOp {
                left,
                op: *op,
                right,
            })
        }
    }

    pub fn eval_print(&mut self, args: &Vec<Expr>) -> Result<(), Error> {
        for (i, arg) in args.iter().enumerate() {
            let value = self.eval_expr(arg)?;
            if i > 0 {
                print!(", ")
            }
            match value {
                Value::Unit => print!("()"),
                Value::Number(num) => print!("{}", num),
            }
        }
        print!("\n");
        Ok(())
    }
}
