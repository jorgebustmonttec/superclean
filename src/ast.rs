#[derive(Debug, Clone, PartialEq)]

/// Expr is the main expression type for the language.
///
/// It can represent boolean literals and if-else expressions.
///
/// The `IfElse` variant contains a condition expression,
/// a `then` branch, and an `else` branch.
///
/// The `Bool` variant represents boolean literals (`true` or `false`).
pub enum Expr {
    Bool(bool),
    IfElse {
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Box<Expr>,
    },
}

/// Stmt is the main statement type for the language.
///
/// It can represent expressions.
///
/// The `Expr` variant contains an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
}

/// Type is the main type type for the language.
///
/// It can represent boolean and unit types.
/// The `Bool` variant represents the boolean type.
/// The `Unit` variant represents the unit type.
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Bool,
    Unit, // Add more later
}
