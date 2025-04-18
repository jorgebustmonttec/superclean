#[derive(Debug, Clone, PartialEq)]

pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    IfElse {
        condition: Box<Expr>,
        then_branch: Vec<Expr>,
        else_branch: Option<Vec<Expr>>,
    },
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnaryOp {
        op: UnaryOp,
        expr: Box<Expr>,
    },
    String(String),
    Variable(String),
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
    StmtExpr(Box<Stmt>),
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
    MemberAccess {
        object: Box<Expr>,
        member: String,
    },
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
    },
    TupleAccess {
        tuple: Box<Expr>,
        index: usize,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Let {
        name: String,
        ty: Option<Type>, // ← was just `Type` before
        expr: Expr,
    },
    Fun {
        name: String,
        params: Vec<(String, Type)>,
        return_type: Type,
        body: Vec<Stmt>,
    },
    Return(Option<Expr>),
    Print(Expr),
    Reassignment {
        name: String,
        expr: Expr,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
    Break,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    String,
    Unit,
    Float,
    Tuple(Vec<Type>),
    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
    List(Box<Type>),
}
