use super::{Env, Value, eval_stmt};
use crate::ast::{BinOp, Expr, Stmt, UnaryOp}; // Added Stmt

/// Evaluates an expression and returns its runtime value.
pub fn eval_expr(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(value) => Ok(Value::Int(*value)),
        Expr::Float(value) => Ok(Value::Float(*value)),
        Expr::Bool(value) => Ok(Value::Bool(*value)),
        Expr::String(value) => Ok(Value::String(value.clone())),
        Expr::Variable(name) => {
            if let Some(value) = env.get(name) {
                Ok(value.clone())
            } else {
                Err(format!("Variable '{}' not found", name))
            }
        }
        Expr::BinOp { left, op, right } => eval_binop(left, op, right, env),
        Expr::UnaryOp { op, expr } => eval_unary_op(op, expr, env),
        Expr::Tuple(elements) => eval_tuple(elements, env),
        Expr::TupleAccess { tuple, index } => eval_tuple_access(tuple, *index, env),
        Expr::IfElse {
            condition,
            then_branch,
            else_branch,
        } => eval_if_else(condition, then_branch, else_branch, env),
        Expr::Call { function, args } => eval_call(function, args, env),
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
    let _tuple_value = eval_expr(tuple, env)?; // Prefix unused variable with an underscore
    if let Value::Tuple(elements) = _tuple_value {
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
        Err(format!("Expected a tuple, found {:?}", _tuple_value))
    }
}

/// Evaluates an `if-else` expression.
fn eval_if_else(
    condition: &Expr,
    then_branch: &[Expr],
    else_branch: &Option<Vec<Expr>>,
    env: &mut Env,
) -> Result<Value, String> {
    // Evaluate the condition
    let condition_value = eval_expr(condition, env)?;
    let branch = match condition_value {
        Value::Bool(true) => then_branch,
        Value::Bool(false) => match else_branch {
            Some(branch) => branch,
            None => return Ok(Value::Unit), // No else branch, return Unit
        },
        _ => return Err("Condition in if-else must evaluate to a Bool".to_string()),
    };

    // Evaluate each expression in the selected branch
    for expr in branch {
        let result = match expr {
            Expr::StmtExpr(stmt) => {
                // Evaluate the statement and propagate its result if it is not Unit
                if let Some(value) = eval_stmt(stmt, env)? {
                    return Ok(value); // Propagate the result (e.g., Break)
                }
                Value::Unit // Otherwise, return Unit
            }
            _ => eval_expr(expr, env)?, // Evaluate the expression
        };

        // If the result is a meaningful value (e.g., Break), propagate it
        if result != Value::Unit {
            return Ok(result);
        }
    }

    Ok(Value::Unit) // Default to Unit if no meaningful value is returned
}

/// Evaluates a function call by substituting arguments into the function body.
fn eval_call(function: &Expr, args: &[Expr], env: &mut Env) -> Result<Value, String> {
    // Evaluate the function expression
    let func_name = match function {
        Expr::Variable(name) => name,
        _ => return Err(format!("Expected a function name, found {:?}", function)),
    };

    // Retrieve the function from the environment
    let func = match env.get_function(func_name) {
        Some(f) => f.clone(),
        None => return Err(format!("Function '{}' not found", func_name)),
    };

    // Ensure the number of arguments matches the number of parameters
    if func.params.len() != args.len() {
        return Err(format!(
            "Function '{}' expected {} arguments but got {}",
            func_name,
            func.params.len(),
            args.len()
        ));
    }

    // Create a new environment for the function call
    let mut func_env = env.clone();

    // Bind arguments to parameters in the new environment
    for ((param_name, _param_type), arg_expr) in func.params.iter().zip(args.iter()) {
        let arg_value = eval_expr(arg_expr, env)?;
        func_env.set(param_name.clone(), arg_value);
    }

    // Evaluate the function body
    for stmt in &func.body {
        if let Some(value) = eval_stmt(stmt, &mut func_env)? {
            return Ok(value); // Return the value if a return statement is encountered
        }
    }

    Ok(Value::Unit) // Default return value is Unit
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

    mod if_else {
        use super::*;

        #[test]
        fn if_else_with_expressions() {
            let mut env = Env::new();
            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(true)),
                then_branch: vec![Expr::Int(42)],
                else_branch: Some(vec![Expr::Int(0)]),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Int(42)));
        }

        #[test]
        fn if_else_with_statements() {
            let mut env = Env::new();
            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(false)),
                then_branch: vec![Expr::StmtExpr(Box::new(Stmt::Print(Expr::String(
                    "This won't print".to_string(),
                ))))],
                else_branch: Some(vec![Expr::StmtExpr(Box::new(Stmt::Print(Expr::String(
                    "This will print".to_string(),
                ))))]),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Unit)); // Statement expressions return Unit
        }

        #[test]
        fn if_without_else() {
            let mut env = Env::new();
            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(false)),
                then_branch: vec![Expr::Int(42)],
                else_branch: None,
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(result, Ok(Value::Unit)); // No else branch, return Unit
        }

        #[test]
        fn invalid_condition_type() {
            let mut env = Env::new();
            let expr = Expr::IfElse {
                condition: Box::new(Expr::Int(1)), // Invalid condition type
                then_branch: vec![Expr::Int(42)],
                else_branch: Some(vec![Expr::Int(0)]),
            };
            let result = eval_expr(&expr, &mut env);
            assert_eq!(
                result,
                Err("Condition in if-else must evaluate to a Bool".to_string())
            );
        }
    }
}
