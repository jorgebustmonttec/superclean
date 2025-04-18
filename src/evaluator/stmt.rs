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
}
