use super::{Env, Value};
use crate::ast::Stmt;

/// Evaluates a statement and returns an optional value (e.g., for `return`).
pub fn eval_stmt(_stmt: &Stmt, _env: &mut Env) -> Result<Option<Value>, String> {
    // Placeholder for statement evaluation
    Ok(None)
}
