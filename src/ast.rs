#[derive(Debug, Clone, PartialEq)]

pub enum Expr {
    Bool(bool),
    IfElse {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
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
