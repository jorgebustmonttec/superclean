pub mod expr;
pub mod stmt;

pub use expr::type_check_expr;
pub use stmt::type_check_stmt;

use crate::ast::{Stmt, Type};
use std::collections::HashMap;

/// Type environment to track variable and function types.
#[derive(Debug, Clone)]
pub struct TypeEnv {
    variables: HashMap<String, Type>,
    functions: HashMap<String, (Vec<Type>, Type)>, // (parameter types, return type)
}

impl TypeEnv {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

/// Type-checks an entire program (list of statements).
pub fn type_check_program(stmts: &[Stmt]) -> Result<(), String> {
    let mut env = TypeEnv::new();
    for stmt in stmts {
        type_check_stmt(stmt, &mut env)?;
    }
    Ok(())
}
