use super::{Env, Function, Value};
use crate::ast::Stmt;
use crate::evaluator::eval_expr;

/// Evaluates a statement and returns an optional value (e.g., for `return`).
pub fn eval_stmt(stmt: &Stmt, env: &mut Env) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Let { name, expr, .. } => eval_let_stmt(name, expr, env),
        Stmt::Reassignment { name, expr } => eval_reassignment_stmt(name, expr, env),
        Stmt::Print(expr) => eval_print_stmt(expr, env),
        Stmt::Fun {
            name,
            params,
            return_type,
            body,
        } => eval_fun_decl(name, params, return_type, body, env),
        Stmt::While { condition, body } => eval_while_stmt(condition, body, env),
        Stmt::Break => Err("Break statement outside of a loop".to_string()),
        _ => Ok(None),
    }
}

/// Evaluates a `let` statement by evaluating the expression and storing the result in the environment.
fn eval_let_stmt(
    name: &String,
    expr: &crate::ast::Expr,
    env: &mut Env,
) -> Result<Option<Value>, String> {
    let value = eval_expr(expr, env)?;
    env.set(name.clone(), value);
    Ok(None)
}

/// Evaluates a reassignment statement by updating the value of an existing variable.
fn eval_reassignment_stmt(
    name: &String,
    expr: &crate::ast::Expr,
    env: &mut Env,
) -> Result<Option<Value>, String> {
    if env.get(name).is_none() {
        return Err(format!("Variable '{}' not declared", name));
    }
    let value = eval_expr(expr, env)?;
    env.set(name.clone(), value);
    Ok(None)
}

/// Evaluates a `print` statement by evaluating the expression and printing its value.
fn eval_print_stmt(expr: &crate::ast::Expr, env: &mut Env) -> Result<Option<Value>, String> {
    let value = eval_expr(expr, env)?;
    println!("{}", value_to_string(value));
    Ok(None)
}

/// Evaluates a function declaration by storing it in the environment.
fn eval_fun_decl(
    name: &String,
    params: &Vec<(String, crate::ast::Type)>,
    return_type: &crate::ast::Type,
    body: &Vec<Stmt>,
    env: &mut Env,
) -> Result<Option<Value>, String> {
    let function = Function {
        params: params.clone(),
        return_type: return_type.clone(),
        body: body.clone(),
    };
    env.add_function(name.clone(), function);
    Ok(None)
}

/// Evaluates a `while` loop by repeatedly evaluating the condition and executing the body.
fn eval_while_stmt(
    condition: &crate::ast::Expr,
    body: &Vec<Stmt>,
    env: &mut Env,
) -> Result<Option<Value>, String> {
    println!("[while] Starting while loop");
    let mut is_infinite_warning_printed = false;

    loop {
        println!("[while] Evaluating condition");
        // Evaluate the condition
        let condition_value = eval_expr(condition, env)?;
        match condition_value {
            Value::Bool(true) => {
                println!("[while] Condition is true, executing body");
                // Print a warning once if the loop is infinite
                if !is_infinite_warning_printed {
                    println!("Warning: Potential infinite loop detected.");
                    is_infinite_warning_printed = true;
                }

                // Execute the body
                for stmt in body {
                    println!("[while] Executing statement: {:?}", stmt);
                    if let Stmt::Break = stmt {
                        println!("[while] Break statement encountered, exiting loop");
                        return Ok(None); // Exit the loop on `break`
                    }

                    eval_stmt(stmt, env)?;
                }
            }
            Value::Bool(false) => break, // Exit the loop when the condition is false
            _ => return Err("Condition in while loop must evaluate to a Bool".to_string()),
        }
    }

    Ok(None) // `while` loops always return `Unit`
}

/// Converts a `Value` to a string for printing.
fn value_to_string(value: Value) -> String {
    match value {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Tuple(elements) => {
            let inner = elements
                .into_iter()
                .map(value_to_string)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", inner)
        }
        _ => format!("{:?}", value), // Fallback for unsupported types
    }
}

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::ast::{Expr, Stmt, Type};

    mod let_stmt {
        use super::*;

        #[test]
        fn let_with_literal() {
            let mut env = Env::new();
            let stmt = Stmt::Let {
                name: "x".to_string(),
                ty: Some(Type::Int),
                expr: Expr::Int(42),
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("x"), Some(&Value::Int(42)));
        }

        #[test]
        fn let_with_expression() {
            let mut env = Env::new();
            let stmt = Stmt::Let {
                name: "y".to_string(),
                ty: Some(Type::Int),
                expr: Expr::BinOp {
                    left: Box::new(Expr::Int(5)),
                    op: crate::ast::BinOp::Add,
                    right: Box::new(Expr::Int(3)),
                },
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("y"), Some(&Value::Int(8)));
        }

        #[test]
        fn let_with_tuple() {
            let mut env = Env::new();
            let stmt = Stmt::Let {
                name: "z".to_string(),
                ty: Some(Type::Tuple(vec![Type::Int, Type::Bool])),
                expr: Expr::Tuple(vec![Expr::Int(1), Expr::Bool(true)]),
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(
                env.get("z"),
                Some(&Value::Tuple(vec![Value::Int(1), Value::Bool(true)]))
            );
        }
    }

    mod reassignment_stmt {
        use super::*;

        #[test]
        fn reassignment_to_existing_variable() {
            let mut env = Env::new();
            env.set("x".to_string(), Value::Int(10));
            let stmt = Stmt::Reassignment {
                name: "x".to_string(),
                expr: Expr::Int(42),
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("x"), Some(&Value::Int(42)));
        }

        #[test]
        fn reassignment_with_expression() {
            let mut env = Env::new();
            env.set("y".to_string(), Value::Int(5));
            let stmt = Stmt::Reassignment {
                name: "y".to_string(),
                expr: Expr::BinOp {
                    left: Box::new(Expr::Variable("y".to_string())),
                    op: crate::ast::BinOp::Add,
                    right: Box::new(Expr::Int(3)),
                },
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("y"), Some(&Value::Int(8)));
        }

        #[test]
        fn reassignment_to_undeclared_variable() {
            let mut env = Env::new();
            let stmt = Stmt::Reassignment {
                name: "z".to_string(),
                expr: Expr::Int(42),
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(result.unwrap_err(), "Variable 'z' not declared");
        }
    }

    mod print_stmt {
        use super::*;

        #[test]
        fn print_int() {
            let mut env = Env::new();
            let stmt = Stmt::Print(Expr::Int(42));
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }

        #[test]
        fn print_float() {
            let mut env = Env::new();
            let stmt = Stmt::Print(Expr::Float(3.14));
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }

        #[test]
        fn print_bool() {
            let mut env = Env::new();
            let stmt = Stmt::Print(Expr::Bool(true));
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }

        #[test]
        fn print_string() {
            let mut env = Env::new();
            let stmt = Stmt::Print(Expr::String("hello".to_string()));
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }

        #[test]
        fn print_tuple() {
            let mut env = Env::new();
            let stmt = Stmt::Print(Expr::Tuple(vec![
                Expr::Int(1),
                Expr::String("hello".to_string()),
            ]));
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }
    }

    mod fun_stmt {
        use super::*;

        #[test]
        fn function_declaration() {
            let mut env = Env::new();
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
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            let function = env.get_function("add").unwrap();
            assert_eq!(
                function,
                &Function {
                    params: vec![("a".to_string(), Type::Int), ("b".to_string(), Type::Int),],
                    return_type: Type::Int,
                    body: vec![Stmt::Return(Some(Expr::BinOp {
                        left: Box::new(Expr::Variable("a".to_string())),
                        op: crate::ast::BinOp::Add,
                        right: Box::new(Expr::Variable("b".to_string())),
                    }))],
                }
            );
        }

        #[test]
        fn function_with_unit_return() {
            let mut env = Env::new();
            let stmt = Stmt::Fun {
                name: "main".to_string(),
                params: vec![],
                return_type: Type::Unit,
                body: vec![Stmt::Print(Expr::String("Hello, world!".to_string()))],
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            let function = env.get_function("main").unwrap();
            assert_eq!(
                function,
                &Function {
                    params: vec![],
                    return_type: Type::Unit,
                    body: vec![Stmt::Print(Expr::String("Hello, world!".to_string()))],
                }
            );
        }
    }

    mod while_stmt {
        use super::*;

        #[test]
        fn while_loop_with_condition() {
            let mut env = Env::new();
            env.set("x".to_string(), Value::Int(0));
            let stmt = Stmt::While {
                condition: Expr::BinOp {
                    left: Box::new(Expr::Variable("x".to_string())),
                    op: crate::ast::BinOp::Less,
                    right: Box::new(Expr::Int(3)),
                },
                body: vec![Stmt::Reassignment {
                    name: "x".to_string(),
                    expr: Expr::BinOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: crate::ast::BinOp::Add,
                        right: Box::new(Expr::Int(1)),
                    },
                }],
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("x"), Some(&Value::Int(3)));
        }

        #[test]
        fn while_loop_with_break() {
            let mut env = Env::new();
            env.set("x".to_string(), Value::Int(0));
            let stmt = Stmt::While {
                condition: Expr::Bool(true),
                body: vec![
                    Stmt::Reassignment {
                        name: "x".to_string(),
                        expr: Expr::BinOp {
                            left: Box::new(Expr::Variable("x".to_string())),
                            op: crate::ast::BinOp::Add,
                            right: Box::new(Expr::Int(1)),
                        },
                    },
                    Stmt::Expr(Expr::IfElse {
                        condition: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Variable("x".to_string())),
                            op: crate::ast::BinOp::Equal,
                            right: Box::new(Expr::Int(5)),
                        }),
                        then_branch: vec![Expr::StmtExpr(Box::new(Stmt::Break))],
                        else_branch: None,
                    }),
                ],
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
            assert_eq!(env.get("x"), Some(&Value::Int(5)));
        }

        #[test]
        fn while_loop_with_false_condition() {
            let mut env = Env::new();
            let stmt = Stmt::While {
                condition: Expr::Bool(false),
                body: vec![Stmt::Print(Expr::String("This won't run".to_string()))],
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_ok());
        }

        #[test]
        fn while_loop_invalid_condition() {
            let mut env = Env::new();
            let stmt = Stmt::While {
                condition: Expr::Int(1), // Invalid condition type
                body: vec![Stmt::Print(Expr::String("This won't run".to_string()))],
            };
            let result = eval_stmt(&stmt, &mut env);
            assert!(result.is_err());
            assert_eq!(
                result.unwrap_err(),
                "Condition in while loop must evaluate to a Bool"
            );
        }
    }
}
