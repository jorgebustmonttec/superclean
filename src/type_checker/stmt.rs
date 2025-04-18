use super::{TypeEnv, type_check_expr};
use crate::ast::Expr;
use crate::ast::Stmt;
use crate::ast::Type;

/// Type-checks a statement.
pub fn type_check_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    println!(
        "[type_check_stmt] Starting type checking for statement: {:?}",
        stmt
    );
    match stmt {
        Stmt::Let { .. } => type_check_let_stmt(stmt, env),
        Stmt::Reassignment { .. } => type_check_reassignment_stmt(stmt, env),
        Stmt::Fun { .. } => type_check_fun_decl(stmt, env),
        Stmt::Expr(_) => {
            println!("[type_check_stmt] Expression statement, skipping type check");
            Ok(())
        }
        Stmt::Print(expr) => type_check_print_stmt(expr, env),
        _ => Err(format!("Unsupported statement: {:?}", stmt)),
    }
}

fn type_check_let_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    println!("[type_check_let_stmt] Starting let statement type checking...");
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
        if let Some(var_type) = env.variables.get(name).cloned() {
            // Clone `var_type` to avoid overlapping borrows
            let expr_type = type_check_expr(expr, env)?;
            if var_type != expr_type {
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

fn type_check_fun_decl(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    println!("[type_check_fun_decl] Starting function type checking...");
    if let Stmt::Fun {
        name,
        params,
        return_type,
        body,
    } = stmt
    {
        println!("[type_check_fun_decl] Checking function '{}'", name);

        // Check if the function is already declared
        if env.functions.contains_key(name) {
            println!(
                "[type_check_fun_decl] Function '{}' is already declared",
                name
            );
            return Err(format!("Function '{}' is already declared", name));
        }

        // Add the function signature to the environment
        let param_types: Vec<Type> = params.iter().map(|(_, ty)| ty.clone()).collect();
        env.functions
            .insert(name.clone(), (param_types.clone(), return_type.clone()));
        println!(
            "[type_check_fun_decl] Added function '{}' to environment with params: {:?} and return type: {:?}",
            name, param_types, return_type
        );

        // Create a new environment for the function body
        let mut local_env = TypeEnv {
            variables: env.variables.clone(), // Clone variables
            functions: env.functions.clone(), // Clone functions
        };

        // Add parameters to the local environment
        for (param_name, param_type) in params {
            local_env
                .variables
                .insert(param_name.clone(), param_type.clone());
            println!(
                "[type_check_fun_decl] Added parameter '{}' with type {:?} to local environment",
                param_name, param_type
            );
        }
        println!(
            "[type_check_fun_decl] Local environment after adding parameters: {:?}",
            local_env.variables
        );

        // Type-check the function body
        let mut return_types = Vec::new();
        let mut has_return_stmt = false;

        for stmt in body {
            println!(
                "[type_check_fun_decl] Checking statement in function body: {:?}",
                stmt
            );

            // Collect return types from all `Return` statements
            if let Stmt::Return(expr) = stmt {
                println!("[type_check_fun_decl] Found return statement in function body");

                has_return_stmt = true;
                println!(
                    "[type_check_fun_decl] Function '{}' has a return statement",
                    name
                );
                let return_type = if let Some(expr) = expr {
                    println!(
                        "[type_check_fun_decl] Found return statement with expression: {:?}",
                        expr
                    );
                    let expr_type = type_check_expr(expr, &mut local_env)?;
                    println!(
                        "[type_check_fun_decl] Found return statement with expression of type {:?}",
                        expr_type
                    );
                    expr_type
                } else {
                    println!(
                        "[type_check_fun_decl] Found return statement with no expression (Unit)"
                    );
                    Type::Unit // Return with no expression returns Unit
                };
                return_types.push(return_type);
            } else {
                // Type-check other statements in the function body
                type_check_stmt(stmt, &mut local_env)?;
            }
        }

        println!("[type_check_fun_decl] Finished checking function body");

        // Ensure the function has at least one return statement
        if !has_return_stmt {
            println!(
                "[type_check_fun_decl] Function '{}' has no return statement",
                name
            );
            return Err(format!(
                "Function '{}' must have at least one return statement",
                name
            ));
        }

        // Ensure all return types match
        if !return_types.is_empty() {
            let first_return_type = &return_types[0];
            if !return_types.iter().all(|t| t == first_return_type) {
                println!(
                    "[type_check_fun_decl] Function '{}' has inconsistent return types: {:?}",
                    name, return_types
                );
                return Err(format!(
                    "Function '{}' has inconsistent return types: {:?}",
                    name, return_types
                ));
            }

            // Ensure the return type matches the function's declared return type
            if *first_return_type != *return_type {
                println!(
                    "[type_check_fun_decl] Function '{}' return type mismatch: expected {:?}, found {:?}",
                    name, return_type, first_return_type
                );
                return Err(format!(
                    "Function '{}' return type mismatch: expected {:?}, found {:?}",
                    name, return_type, first_return_type
                ));
            }
        }

        println!(
            "[type_check_fun_decl] Function '{}' type checked successfully",
            name
        );
        Ok(())
    } else {
        println!("[type_check_fun_decl] Invalid statement passed to function type checker");
        Err("Invalid statement for type_check_fun_decl".to_string())
    }
}

fn type_check_print_stmt(expr: &Expr, env: &mut TypeEnv) -> Result<(), String> {
    println!("[type_check_print_stmt] Type checking print statement...");
    type_check_expr(expr, env)?; // Ensure the expression is valid
    Ok(())
}

#[cfg(test)]
mod stmt_type_tests {
    use super::*;
    use crate::ast::{Expr, Stmt, Type};
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

    mod function_tests {
        use super::*;

        #[test]
        fn test_valid_function_with_return() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "add".to_string(),
                params: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int)],
                return_type: Type::Int,
                body: vec![Stmt::Return(Some(Expr::BinOp {
                    left: Box::new(Expr::Variable("a".to_string())),
                    op: crate::ast::BinOp::Add,
                    right: Box::new(Expr::Variable("b".to_string())),
                }))],
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
            assert!(env.functions.contains_key("add"));
        }

        #[test]
        fn test_correct_function() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "multiply".to_string(),
                params: vec![("x".to_string(), Type::Int)],
                return_type: Type::Int,
                body: vec![Stmt::Return(Some(Expr::BinOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: crate::ast::BinOp::Mul,
                    right: Box::new(Expr::Int(2)),
                }))],
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
            assert!(env.functions.contains_key("multiply"));
        }

        #[test]
        fn test_function_with_unit_return() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "empty".to_string(),
                params: vec![],
                return_type: Type::Unit,
                body: vec![Stmt::Return(None)], // Explicitly returning Unit
            };
            assert!(type_check_stmt(&stmt, &mut env).is_ok());
            assert!(env.functions.contains_key("empty"));
        }

        #[test]
        fn test_function_with_inconsistent_return_types() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "inconsistent".to_string(),
                params: vec![],
                return_type: Type::Int,
                body: vec![
                    Stmt::Return(Some(Expr::Int(42))),
                    Stmt::Return(Some(Expr::Bool(true))), // Mismatched type
                ],
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Function 'inconsistent' has inconsistent return types: [Int, Bool]"
            );
        }

        #[test]
        fn test_function_with_missing_return() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "missing_return".to_string(),
                params: vec![],
                return_type: Type::Int,
                body: vec![Stmt::Expr(Expr::Int(42))], // No return statement
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Function 'missing_return' must have at least one return statement"
            );
        }

        #[test]
        fn test_function_with_return_type_mismatch() {
            let mut env = TypeEnv::new();
            let stmt = Stmt::Fun {
                name: "mismatch".to_string(),
                params: vec![],
                return_type: Type::Int,
                body: vec![Stmt::Return(Some(Expr::Bool(true)))], // Return type mismatch
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Function 'mismatch' return type mismatch: expected Int, found Bool"
            );
        }

        #[test]
        fn test_function_with_duplicate_declaration() {
            let mut env = TypeEnv::new();
            env.functions
                .insert("duplicate".to_string(), (vec![Type::Int], Type::Int));

            let stmt = Stmt::Fun {
                name: "duplicate".to_string(),
                params: vec![("a".to_string(), Type::Int)],
                return_type: Type::Int,
                body: vec![Stmt::Return(Some(Expr::Variable("a".to_string())))],
            };
            let result = type_check_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Function 'duplicate' is already declared"
            );
        }
    }
}
