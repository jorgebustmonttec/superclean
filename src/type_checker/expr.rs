use super::TypeEnv;
use crate::ast::{BinOp, Expr, Type, UnaryOp};

/// Type-checks an expression and returns its type.
pub fn type_check_expr(expr: &Expr, env: &TypeEnv) -> Result<Type, String> {
    match expr {
        Expr::Int(_) => type_check_literal(expr),
        Expr::Bool(_) => type_check_literal(expr),
        Expr::Float(_) => type_check_literal(expr),
        Expr::String(_) => type_check_literal(expr),
        Expr::BinOp { left, op, right } => type_check_binop(left, op, right, env),
        Expr::UnaryOp { op, expr } => type_check_unary_op(op, expr, env),
        _ => Err(format!("Unsupported expression: {:?}", expr)),
    }
}

/// Type-checks a literal (e.g., integers, booleans, floats).
fn type_check_literal(expr: &Expr) -> Result<Type, String> {
    match expr {
        Expr::Int(_) => Ok(Type::Int),
        Expr::Bool(_) => Ok(Type::Bool),
        Expr::Float(_) => Ok(Type::Float),
        Expr::String(_) => Ok(Type::String),
        _ => Err(format!("Unsupported literal: {:?}", expr)),
    }
}

/// Type-checks a binary operation.
fn type_check_binop(left: &Expr, op: &BinOp, right: &Expr, env: &TypeEnv) -> Result<Type, String> {
    let left_type = type_check_expr(left, env)?;
    let right_type = type_check_expr(right, env)?;

    match (left_type.clone(), right_type.clone(), op) {
        // Arithmetic operations for integers
        (Type::Int, Type::Int, BinOp::Add)
        | (Type::Int, Type::Int, BinOp::Sub)
        | (Type::Int, Type::Int, BinOp::Mul)
        | (Type::Int, Type::Int, BinOp::Div)
        | (Type::Int, Type::Int, BinOp::Mod) => Ok(Type::Int),

        // Arithmetic operations for floats
        (Type::Float, Type::Float, BinOp::Add)
        | (Type::Float, Type::Float, BinOp::Sub)
        | (Type::Float, Type::Float, BinOp::Mul)
        | (Type::Float, Type::Float, BinOp::Div) => Ok(Type::Float),

        // Comparison operations for integers
        (Type::Int, Type::Int, BinOp::Equal)
        | (Type::Int, Type::Int, BinOp::NotEqual)
        | (Type::Int, Type::Int, BinOp::Less)
        | (Type::Int, Type::Int, BinOp::LessEqual)
        | (Type::Int, Type::Int, BinOp::Greater)
        | (Type::Int, Type::Int, BinOp::GreaterEqual) => Ok(Type::Bool),

        // Comparison operations for floats
        (Type::Float, Type::Float, BinOp::Equal)
        | (Type::Float, Type::Float, BinOp::NotEqual)
        | (Type::Float, Type::Float, BinOp::Less)
        | (Type::Float, Type::Float, BinOp::LessEqual)
        | (Type::Float, Type::Float, BinOp::Greater)
        | (Type::Float, Type::Float, BinOp::GreaterEqual) => Ok(Type::Bool),

        // Logical operations for booleans
        (Type::Bool, Type::Bool, BinOp::And) | (Type::Bool, Type::Bool, BinOp::Or) => {
            Ok(Type::Bool)
        }

        // Concatenation for strings
        (Type::String, Type::String, BinOp::Add) => Ok(Type::String),

        // Equality and inequality for strings
        (Type::String, Type::String, BinOp::Equal)
        | (Type::String, Type::String, BinOp::NotEqual) => Ok(Type::Bool),

        // Invalid operations
        _ => Err(format!(
            "Invalid operation: {:?} between {:?} and {:?}",
            op, left_type, right_type
        )),
    }
}

/// Type-checks a unary operation.
fn type_check_unary_op(op: &UnaryOp, expr: &Expr, env: &TypeEnv) -> Result<Type, String> {
    let expr_type = type_check_expr(expr, env)?;

    match (op, expr_type.clone()) {
        (UnaryOp::Neg, Type::Int) => Ok(Type::Int), // Negation for integers
        (UnaryOp::Neg, Type::Float) => Ok(Type::Float), // Negation for floats
        (UnaryOp::Not, Type::Bool) => Ok(Type::Bool), // Logical NOT for booleans
        _ => Err(format!(
            "Invalid unary operation: {:?} on {:?}",
            op, expr_type
        )),
    }
}

// ========================= Tests =========================

#[cfg(test)]
mod expr_type_tests {
    use super::*;
    use crate::ast::{BinOp, Expr, UnaryOp};

    mod literal_tests {
        use super::*;

        #[test]
        fn test_int_literal() {
            let expr = Expr::Int(42);
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Int));
        }

        #[test]
        fn test_boolean_literals() {
            let env = TypeEnv::new();

            let expr = Expr::Bool(true);
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            let expr = Expr::Bool(false);
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));
        }

        #[test]
        fn test_float_literal() {
            let expr = Expr::Float(3.14);
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Float));
        }

        #[test]
        fn test_string_literal() {
            let expr = Expr::String("Hello".to_string());
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::String));
        }
    }

    mod binary_op_tests {
        use super::*;

        #[test]
        fn test_addition() {
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(1)),
                op: BinOp::Add,
                right: Box::new(Expr::Int(2)),
            };
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Int));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.0)),
                op: BinOp::Add,
                right: Box::new(Expr::Float(2.0)),
            };
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Float));

            let expr = Expr::BinOp {
                left: Box::new(Expr::String("Hello".to_string())),
                op: BinOp::Add,
                right: Box::new(Expr::String(" World".to_string())),
            };
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::String));
        }

        #[test]
        fn test_invalid_operation() {
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(1)),
                op: BinOp::Add,
                right: Box::new(Expr::Bool(true)), // Invalid: Int + Bool
            };
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }

        #[test]
        fn test_comparisons() {
            let env = TypeEnv::new();

            // Valid comparisons
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(1)),
                op: BinOp::Equal,
                right: Box::new(Expr::Int(2)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(3)),
                op: BinOp::Less,
                right: Box::new(Expr::Int(4)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            let expr = Expr::BinOp {
                left: Box::new(Expr::String("Hello".to_string())),
                op: BinOp::Equal,
                right: Box::new(Expr::String("Hello".to_string())),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            // Invalid comparison
            let expr = Expr::BinOp {
                left: Box::new(Expr::Int(1)),
                op: BinOp::Equal,
                right: Box::new(Expr::Bool(true)), // Invalid: Int == Bool
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }

        #[test]
        fn test_logical_operations() {
            let env = TypeEnv::new();

            // Valid logical operations
            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(true)),
                op: BinOp::And,
                right: Box::new(Expr::Bool(false)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(true)),
                op: BinOp::Or,
                right: Box::new(Expr::Bool(false)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            // Invalid logical operation
            let expr = Expr::BinOp {
                left: Box::new(Expr::Bool(true)),
                op: BinOp::And,
                right: Box::new(Expr::Int(1)), // Invalid: Bool && Int
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }

        #[test]
        fn test_float_operations() {
            let env = TypeEnv::new();

            // Valid float operations
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.1)),
                op: BinOp::Add,
                right: Box::new(Expr::Float(2.2)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Float));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.3)),
                op: BinOp::Mul,
                right: Box::new(Expr::Float(4.4)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Float));

            // Invalid float operation with int
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.1)),
                op: BinOp::Add,
                right: Box::new(Expr::Int(2)), // Invalid: Float + Int
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }

        #[test]
        fn test_float_comparisons() {
            let env = TypeEnv::new();

            // Valid float comparisons
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.1)),
                op: BinOp::Less,
                right: Box::new(Expr::Float(2.2)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(3.3)),
                op: BinOp::Equal,
                right: Box::new(Expr::Float(3.3)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            // Invalid float comparison with int
            let expr = Expr::BinOp {
                left: Box::new(Expr::Float(1.1)),
                op: BinOp::Less,
                right: Box::new(Expr::Int(2)), // Invalid: Float < Int
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }
    }

    mod unary_op_tests {
        use super::*;

        #[test]
        fn test_negation() {
            let env = TypeEnv::new();

            // Negation for integers
            let expr = Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Int(42)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Int));

            // Negation for floats
            let expr = Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Float(3.14)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Float));

            // Invalid negation
            let expr = Expr::UnaryOp {
                op: UnaryOp::Neg,
                expr: Box::new(Expr::Bool(true)), // Invalid: Negation on Bool
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }

        #[test]
        fn test_logical_not() {
            let env = TypeEnv::new();

            // Logical NOT for booleans
            let expr = Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Bool(true)),
            };
            assert_eq!(type_check_expr(&expr, &env), Ok(Type::Bool));

            // Invalid logical NOT
            let expr = Expr::UnaryOp {
                op: UnaryOp::Not,
                expr: Box::new(Expr::Int(1)), // Invalid: Logical NOT on Int
            };
            assert!(type_check_expr(&expr, &env).is_err());
        }
    }

    mod nested_expr_tests {
        use super::*;

        #[test]
        fn test_nested_expressions() {
            let expr = Expr::BinOp {
                left: Box::new(Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Int(2)),
                }),
                op: BinOp::Mul,
                right: Box::new(Expr::Int(3)),
            };
            let env = TypeEnv::new();
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Int));
        }
    }
}
