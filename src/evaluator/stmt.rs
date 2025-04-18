use super::{Env, Value};
use crate::ast::Stmt;
use crate::evaluator::eval_expr;

/// Evaluates a statement and returns an optional value (e.g., for `return`).
pub fn eval_stmt(stmt: &Stmt, env: &mut Env) -> Result<Option<Value>, String> {
    match stmt {
        Stmt::Let { name, expr, .. } => eval_let_stmt(name, expr, env),
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
}
