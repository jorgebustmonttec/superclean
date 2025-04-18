pub mod expr;
pub mod stmt;

pub use expr::type_check_expr;
pub use stmt::type_check_stmt;

use crate::ast::{Stmt, Type};
use std::collections::HashMap;

/// Type environment to track variable and function types.
#[derive(Debug, Clone)]
pub struct TypeEnv {
    pub variables: HashMap<String, Type>,
    pub functions: HashMap<String, (Vec<Type>, Type)>, // (parameter types, return type)
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
pub fn type_check_program(stmts: &[Stmt]) -> Result<TypeEnv, String> {
    let mut env = TypeEnv::new();
    for stmt in stmts {
        type_check_stmt(stmt, &mut env)?;
    }
    Ok(env)
}

#[cfg(test)]
mod type_check_tests {
    use super::*;
    use crate::lexer::lex;
    use crate::parser::parse;

    #[test]
    fn test_valid_program() {
        let code = r#"
            let x: Int = 5;
            let y = 10;
            let z = x + y;

            fun add(a: Int, b: Int): Int {
                return a + b;
            }

            let result = add(z, 20);

            while result > 0 {
                print(result);
                break;
            }
        "#;

        let tokens = lex(code).unwrap();
        let stmts = parse(&tokens).unwrap();
        let result = type_check_program(&stmts);
        assert!(result.is_ok());
    }

    #[test]
    fn test_invalid_non_boolean_condition() {
        let code = r#"
            let x: Int = 5;

            while x {
                print(x);
            }
        "#;

        let tokens = lex(code).unwrap();
        let stmts = parse(&tokens).unwrap();
        let result = type_check_program(&stmts);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "While loop condition must be Bool, found Int"
        );
    }

    #[test]
    fn test_invalid_break_outside_loop() {
        let code = r#"
            break;
        "#;

        let tokens = lex(code).unwrap();
        let stmts = parse(&tokens).unwrap();
        let result = type_check_program(&stmts);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "`break` statement outside of a loop");
    }

    #[test]
    fn test_function_with_return_type_mismatch() {
        let code = r#"
            fun add(a: Int, b: Int): Int {
                return true; // Invalid: returning Bool instead of Int
            }
        "#;

        let tokens = lex(code).unwrap();
        let stmts = parse(&tokens).unwrap();
        let result = type_check_program(&stmts);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err(),
            "Function 'add' return type mismatch: expected Int, found Bool"
        );
    }

    #[test]
    fn test_valid_nested_while_loops() {
        let code = r#"
            let x: Int = 0;
            let y: Int = 0;

            while x < 10 {
                while y < 5 {
                    break;
                }
                x = x + 1;
            }
        "#;

        let tokens = lex(code).unwrap();
        let stmts = parse(&tokens).unwrap();
        let result = type_check_program(&stmts);
        assert!(result.is_ok());
    }
}
