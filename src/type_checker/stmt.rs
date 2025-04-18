use super::{TypeEnv, type_check_expr};
use crate::ast::Stmt;

/// Type-checks a statement.
pub fn type_check_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    match stmt {
        Stmt::Let { .. } => type_check_let_stmt(stmt, env),
        Stmt::Reassignment { .. } => type_check_reassignment_stmt(stmt, env),
        _ => Err(format!("Unsupported statement: {:?}", stmt)),
    }
}

fn type_check_let_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    if let Stmt::Let { name, ty, expr } = stmt {
        let expr_type = type_check_expr(expr, env)?;

        if let Some(expected_type) = ty {
            if *expected_type != expr_type {
                return Err(format!(
                    "Type mismatch for variable '{}': expected {:?}, found {:?}",
                    name, expected_type, expr_type
                ));
            }
        }

        env.variables.insert(name.clone(), expr_type);
        Ok(())
    } else {
        Err("Invalid statement for type_check_let_stmt".to_string())
    }
}

fn type_check_reassignment_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    if let Stmt::Reassignment { name, expr } = stmt {
        if let Some(var_type) = env.variables.get(name) {
            let expr_type = type_check_expr(expr, env)?;
            if *var_type != expr_type {
                return Err(format!(
                    "Type mismatch for reassignment to '{}': expected {:?}, found {:?}",
                    name, var_type, expr_type
                ));
            }
            Ok(())
        } else {
            Err(format!("Variable '{}' not declared", name))
        }
    } else {
        Err("Invalid statement for type_check_reassignment_stmt".to_string())
    }
}

#[cfg(test)]
mod stmt_type_tests {
    use super::*;
    use crate::ast::{Expr, Type};
    use crate::type_checker::TypeEnv;

    mod assignment_tests {
        use super::*;

        #[test]
        fn test_let_with_explicit_type() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Let {
                name: "x".to_string(),
                ty: Some(Type::Int),
                expr: Expr::Int(42),
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
            assert_eq!(env.variables.get("x"), Some(&Type::Int));
        }

        #[test]
        fn test_let_with_inferred_type() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Let {
                name: "y".to_string(),
                ty: None,
                expr: Expr::Bool(true),
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
            assert_eq!(env.variables.get("y"), Some(&Type::Bool));
        }

        #[test]
        fn test_let_with_type_mismatch() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Let {
                name: "z".to_string(),
                ty: Some(Type::String),
                expr: Expr::Int(42), // Mismatch: expected String, found Int
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Type mismatch for variable 'z': expected String, found Int"
            );
        }
    }

    mod reassignment_tests {
        use super::*;

        #[test]
        fn test_valid_reassignment() {
            let mut env = TypeEnv::new();
            env.variables.insert("x".to_string(), Type::Int);

            let stmt = Stmt::Reassignment {
                name: "x".to_string(),
                expr: Expr::Int(10),
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
        }

        #[test]
        fn test_reassignment_with_type_mismatch() {
            let mut env = TypeEnv::new();
            env.variables.insert("x".to_string(), Type::Int);

            let stmt = Stmt::Reassignment {
                name: "x".to_string(),
                expr: Expr::Bool(true), // Mismatch: expected Int, found Bool
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Type mismatch for reassignment to 'x': expected Int, found Bool"
            );
        }

        #[test]
        fn test_reassignment_to_undeclared_variable() {
            let mut env = TypeEnv::new();

            let stmt = Stmt::Reassignment {
                name: "y".to_string(),
                expr: Expr::Int(10),
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Variable 'y' not declared");
        }
    }
}
