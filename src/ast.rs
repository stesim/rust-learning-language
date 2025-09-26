pub struct Ast(Vec<Stmt>);

impl Ast {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self(statements)
    }

    pub fn statements(&self) -> impl Iterator<Item = &Stmt> {
        self.0.iter()
    }

    pub fn push(&mut self, statement: Stmt) {
        self.0.push(statement);
    }
}

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
pub struct InternId(pub usize);

impl InternId {
    pub fn id(&self) -> usize {
        self.0
    }
}

pub struct Interns(pub Vec<String>);

impl Interns {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn get(&self, id: &InternId) -> Option<&str> {
        self.0.get(id.0).map(|s| s.as_str())
    }
}
