use super::{Env, Value};
use crate::ast::{BinOp, Expr, UnaryOp};

/// Evaluates an expression and returns its runtime value.
pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Float(value) => Ok(Value::Float(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::BinOp { left, op, right } => eval_binop(left, op, right, env),
        Expr::UnaryOp { op, expr } => eval_unary_op(op, expr, env),
        Expr::Tuple(elements) => eval_tuple(elements, env),
        Expr::TupleAccess { tuple, index } => eval_tuple_access(tuple, *index, env),
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

        // String concatenation
        (Value::String(l), r, BinOp::Add) => Ok(Value::String(l + &value_to_string(r))),
        (l, Value::String(r), BinOp::Add) => Ok(Value::String(value_to_string(l) + &r)),
        (Value::String(l), Value::String(r), BinOp::Add) => Ok(Value::String(l + &r)),

        // Unsupported operations
        _ => Err(format!(
            "Unsupported binary operation: {:?} {:?} {:?}",
            left, op, right
        )),
    }
}

/// Evaluates a tuple creation expression.
fn eval_tuple(elements: &[Expr], env: &mut Env) -> Result<Value, String> {
    let mut values = Vec::new();
    for element in elements {
        values.push(eval_expr(element, env)?);
    }
    Ok(Value::Tuple(values))
}

/// Evaluates a tuple access expression.
fn eval_tuple_access(tuple: &Expr, index: usize, env: &mut Env) -> Result<Value, String> {
    let tuple_value = eval_expr(tuple, env)?;
    if let Value::Tuple(elements) = tuple_value {
        if index < elements.len() {
            Ok(elements[index].clone())
        } else {
            Err(format!(
                "Tuple index out of bounds: {} (tuple has {} elements)",
                index,
                elements.len()
            ))
        }
    } else {
        Err(format!("Expected a tuple, found {:?}", tuple_value))
    }
}

/// Converts a `Value` to a string for concatenation.
fn value_to_string(value: Value) -> String {
    match value {
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::String(s) => s,
        _ => format!("{:?}", value), // Fallback for unsupported types
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

    mod string {
        use super::*;

        #[test]
        fn literal() {
            let mut env = Env::new();
            let expr = Expr::String("hello".to_string());
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::String("hello".to_string())));
        }

        #[test]
        fn concat_with_string() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::String("hello".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::String(" world".to_string())),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::String("hello world".to_string())));
        }

        #[test]
        fn concat_with_int() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::String("result: ".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::Int(42)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::String("result: 42".to_string())));
        }

        #[test]
        fn concat_with_float() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::String("value: ".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::Float(3.14)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::String("value: 3.14".to_string())));
        }

        #[test]
        fn concat_with_bool() {
            let mut env = Env::new();
            let expr = Expr::BinOp {
                left: Box::new(Expr::String("is true: ".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::Bool(true)),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::String("is true: true".to_string())));
        }
    }

    mod tuple {
        use super::*;

        #[test]
        fn create_tuple() {
            let mut env = Env::new();
            let expr = Expr::Tuple(vec![
                Expr::Int(1),
                Expr::Bool(true),
                Expr::String("hello".to_string()),
            ]);
            let result = eval_expr(&expr, &mut env);
            assert_eq!(
                result,
                Ok(Value::Tuple(vec![
                    Value::Int(1),
                    Value::Bool(true),
                    Value::String("hello".to_string())
                ]))
            );
        }

        #[test]
        fn access_tuple_element() {
            let mut env = Env::new();
            let tuple_expr = Expr::Tuple(vec![
                Expr::Int(1),
                Expr::Bool(true),
                Expr::String("hello".to_string()),
            ]);
            let tuple_value = eval_expr(&tuple_expr, &mut env).unwrap();

            let expr = Expr::TupleAccess {
                tuple: Box::new(tuple_expr),
                index: 1,
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Bool(true)));
        }

        #[test]
        fn access_out_of_bounds() {
            let mut env = Env::new();
            let tuple_expr = Expr::Tuple(vec![Expr::Int(1), Expr::Bool(true)]);
            let expr = Expr::TupleAccess {
                tuple: Box::new(tuple_expr),
                index: 5, // Out of bounds
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(
                result,
                Err("Tuple index out of bounds: 5 (tuple has 2 elements)".to_string())
            );
        }

        #[test]
        fn access_non_tuple() {
            let mut env = Env::new();
            let expr = Expr::TupleAccess {
                tuple: Box::new(Expr::Int(42)), // Not a tuple
                index: 0,
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Err("Expected a tuple, found Int(42)".to_string()));
        }

        #[test]
        fn nested_tuple_access() {
            let mut env = Env::new();
            let expr = Expr::TupleAccess {
                tuple: Box::new(Expr::Tuple(vec![
                    Expr::Int(1),
                    Expr::Tuple(vec![Expr::Int(2), Expr::Int(3)]),
                ])),
                index: 1,
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Tuple(vec![Value::Int(2), Value::Int(3)])));
        }
    }
}
