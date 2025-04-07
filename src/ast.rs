#[derive(Debug, Clone, PartialEq)]

pub enum Expr {
    Bool(bool),
    Int(i64),
    Float(f64),
    IfElse {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Unit, // Add more later
}
