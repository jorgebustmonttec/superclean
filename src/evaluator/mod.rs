pub mod expr;
pub mod stmt;

pub use expr::eval_expr;
pub use stmt::eval_stmt;

use crate::ast::Stmt;
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
    Function(Function),
    Break, // Added Break variant
}

/// Function representation
#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub params: Vec<(String, crate::ast::Type)>,
    pub return_type: crate::ast::Type,
    pub body: Vec<Stmt>,
}

/// Runtime environment
#[derive(Debug, Clone)]
pub struct Env {
    variables: HashMap<String, Value>,
    functions: HashMap<String, Function>, // Added functions
}

impl Env {
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Value> {
        self.variables.get(name)
    }

    pub fn set(&mut self, name: String, value: Value) {
        self.variables.insert(name, value);
    }

    pub fn add_function(&mut self, name: String, function: Function) {
        self.functions.insert(name, function);
    }

    pub fn get_function(&self, name: &str) -> Option<&Function> {
        self.functions.get(name)
    }
}
