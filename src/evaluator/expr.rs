use super::{Env, Value};
use crate::ast::{BinOp, Expr, UnaryOp};

/// Evaluates an expression and returns its runtime value.
pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Float(value) => Ok(Value::Float(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::BinOp { left, op, right } => eval_binop(left, op, right, env),
        Expr::UnaryOp { op, expr } => eval_unary_op(op, expr, env),
        _ => Err(format!("Unsupported expression: {:?}", expr)),
    }
}

/// Evaluates a unary operation.
fn eval_unary_op(op: &UnaryOp, expr: &Expr, env: &mut Env) -> Result<Value, String> {
    let value = eval_expr(expr, env)?;

    match (op, value) {
        (UnaryOp::Not, Value::Bool(b)) => Ok(Value::Bool(!b)),
        (UnaryOp::Neg, Value::Int(i)) => Ok(Value::Int(-i)),
        (UnaryOp::Neg, Value::Float(f)) => Ok(Value::Float(-f)),
        _ => Err(format!("Unsupported unary operation: {:?} {:?}", op, expr)),
    }
}

/// Evaluates a binary operation.
fn eval_binop(left: &Expr, op: &BinOp, right: &Expr, env: &mut Env) -> Result<Value, String> {
    let left_val = eval_expr(left, env)?;
    let right_val = eval_expr(right, env)?;

    match (left_val, right_val, op) {
        // Integer operations
        (Value::Int(l), Value::Int(r), BinOp::Add) => Ok(Value::Int(l + r)),
        (Value::Int(l), Value::Int(r), BinOp::Sub) => Ok(Value::Int(l - r)),
        (Value::Int(l), Value::Int(r), BinOp::Mul) => Ok(Value::Int(l * r)),
        (Value::Int(l), Value::Int(r), BinOp::Div) => {
            if r == 0 {
                Err("Division by zero".to_string())
            } else {
                Ok(Value::Int(l / r))
            }
        }
        (Value::Int(l), Value::Int(r), BinOp::Mod) => {
            if r == 0 {
                Err("Modulo by zero".to_string())
            } else {
                Ok(Value::Int(l % r))
            }
        }

        // Float operations
        (Value::Float(l), Value::Float(r), BinOp::Add) => Ok(Value::Float(l + r)),
        (Value::Float(l), Value::Float(r), BinOp::Sub) => Ok(Value::Float(l - r)),
        (Value::Float(l), Value::Float(r), BinOp::Mul) => Ok(Value::Float(l * r)),
        (Value::Float(l), Value::Float(r), BinOp::Div) => {
            if r == 0.0 {
                Err("Division by zero".to_string())
            } else {
                Ok(Value::Float(l / r))
            }
        }

        // Boolean operations
        (Value::Bool(l), Value::Bool(r), BinOp::And) => Ok(Value::Bool(l && r)),
        (Value::Bool(l), Value::Bool(r), BinOp::Or) => Ok(Value::Bool(l || r)),

        // Integer comparisons
        (Value::Int(l), Value::Int(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
        (Value::Int(l), Value::Int(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),
        (Value::Int(l), Value::Int(r), BinOp::Less) => Ok(Value::Bool(l < r)),
        (Value::Int(l), Value::Int(r), BinOp::LessEqual) => Ok(Value::Bool(l <= r)),
        (Value::Int(l), Value::Int(r), BinOp::Greater) => Ok(Value::Bool(l > r)),
        (Value::Int(l), Value::Int(r), BinOp::GreaterEqual) => Ok(Value::Bool(l >= r)),

        // Float comparisons
        (Value::Float(l), Value::Float(r), BinOp::Equal) => Ok(Value::Bool(l == r)),
        (Value::Float(l), Value::Float(r), BinOp::NotEqual) => Ok(Value::Bool(l != r)),
        (Value::Float(l), Value::Float(r), BinOp::Less) => Ok(Value::Bool(l < r)),
        (Value::Float(l), Value::Float(r), BinOp::LessEqual) => Ok(Value::Bool(l <= r)),
        (Value::Float(l), Value::Float(r), BinOp::Greater) => Ok(Value::Bool(l > r)),
        (Value::Float(l), Value::Float(r), BinOp::GreaterEqual) => Ok(Value::Bool(l >= r)),

        // Unsupported operations
        _ => Err(format!(
            "Unsupported binary operation: {:?} {:?} {:?}",
            left, op, right
        )),
    }
}

#[cfg(test)]
mod eval_test {
    use super::*;
    use crate::ast::{BinOp, UnaryOp};

    mod int {
        use super::*;

        #[test]
        fn literal() {
            let mut env = Env::new();
            let expr = Expr::Int(42);
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(42)));
        }

        #[test]
        fn add() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(5)),
                op: BinOp::Add,
                right: Box::new(Expr::Int(3)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(8)));
        }

        #[test]
        fn sub() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(10)),
                op: BinOp::Sub,
                right: Box::new(Expr::Int(4)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(6)));
        }

        #[test]
        fn mul() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(7)),
                op: BinOp::Mul,
                right: Box::new(Expr::Int(6)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(42)));
        }

        #[test]
        fn div() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(20)),
                op: BinOp::Div,
                right: Box::new(Expr::Int(4)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(5)));
        }

        #[test]
        fn div_by_zero() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(10)),
                op: BinOp::Div,
                right: Box::new(Expr::Int(0)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Err("Division by zero".to_string()));
        }

        #[test]
        fn modulo() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(10)),
                op: BinOp::Mod,
                right: Box::new(Expr::Int(3)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(1)));
        }

        #[test]
        fn modulo_by_zero() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(10)),
                op: BinOp::Mod,
                right: Box::new(Expr::Int(0)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Err("Modulo by zero".to_string()));
        }

        #[test]
        fn comparisons() {
            let mut env = Env::new();

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(5)),
                op: BinOp::Equal,
                right: Box::new(Expr::Int(5)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(5)),
                op: BinOp::NotEqual,
                right: Box::new(Expr::Int(3)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(3)),
                op: BinOp::Less,
                right: Box::new(Expr::Int(5)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(5)),
                op: BinOp::LessEqual,
                right: Box::new(Expr::Int(5)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(7)),
                op: BinOp::Greater,
                right: Box::new(Expr::Int(5)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(5)),
                op: BinOp::GreaterEqual,
                right: Box::new(Expr::Int(5)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));
        }
    }

    mod float {
        use super::*;

        #[test]
        fn literal() {
            let mut env = Env::new();
            let expr = Expr::Float(3.14);
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(3.14)));
        }

        #[test]
        fn add() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.5)),
                op: BinOp::Add,
                right: Box::new(Expr::Float(2.5)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(4.0)));
        }

        #[test]
        fn sub() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(5.5)),
                op: BinOp::Sub,
                right: Box::new(Expr::Float(2.0)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(3.5)));
        }

        #[test]
        fn mul() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(2.0)),
                op: BinOp::Mul,
                right: Box::new(Expr::Float(3.5)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(7.0)));
        }

        #[test]
        fn div() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(7.0)),
                op: BinOp::Div,
                right: Box::new(Expr::Float(2.0)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(3.5)));
        }

        #[test]
        fn div_by_zero() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(7.0)),
                op: BinOp::Div,
                right: Box::new(Expr::Float(0.0)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Err("Division by zero".to_string()));
        }

        #[test]
        fn comparisons() {
            let mut env = Env::new();

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.14)),
                op: BinOp::Equal,
                right: Box::new(Expr::Float(3.14)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.14)),
                op: BinOp::NotEqual,
                right: Box::new(Expr::Float(2.71)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(2.71)),
                op: BinOp::Less,
                right: Box::new(Expr::Float(3.14)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.14)),
                op: BinOp::LessEqual,
                right: Box::new(Expr::Float(3.14)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.14)),
                op: BinOp::Greater,
                right: Box::new(Expr::Float(2.71)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.14)),
                op: BinOp::GreaterEqual,
                right: Box::new(Expr::Float(3.14)),
            };
            assert_eq!(eval_expr(&expr, &mut env), Ok(Value::Bool(true)));
        }
    }

    mod bool {
        use super::*;

        #[test]
        fn literal() {
            let mut env = Env::new();
            let expr = Expr::Bool(true);
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(true)));

            let expr = Expr::Bool(false);
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(false)));
        }

        #[test]
        fn and() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(true)),
                op: BinOp::And,
                right: Box::new(Expr::Bool(false)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(false)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(true)),
                op: BinOp::And,
                right: Box::new(Expr::Bool(true)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(true)));
        }

        #[test]
        fn or() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(false)),
                op: BinOp::Or,
                right: Box::new(Expr::Bool(true)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(true)));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(false)),
                op: BinOp::Or,
                right: Box::new(Expr::Bool(false)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(false)));
        }
    }

    mod unary {
        use super::*;

        #[test]
        fn not() {
            let mut env = Env::new();
            let expr = Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Bool(true)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(false)));

            let expr = Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Bool(false)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(true)));
        }

        #[test]
        fn neg_int() {
            let mut env = Env::new();
            let expr = Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Int(42)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(-42)));
        }

        #[test]
        fn neg_float() {
            let mut env = Env::new();
            let expr = Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Float(3.14)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Float(-3.14)));
        }
    }
}
