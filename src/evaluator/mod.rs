pub mod expr;
pub mod stmt;

pub use expr::eval_expr;
pub use stmt::eval_stmt;

use crate::ast::{Expr, Stmt};
use std::collections::HashMap;

/// Runtime value representation
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Float(f64),
    String(String),
    Unit,
    Tuple(Vec<Value>),
}

/// Runtime environment
#[derive(Debug, Clone)]
pub struct Env {
    variables: HashMap<String, Value>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }
}
