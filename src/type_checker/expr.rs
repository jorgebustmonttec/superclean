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
        Expr::Tuple(elements) => type_check_tuple(elements, env),
        Expr::TupleAccess { tuple, index } => type_check_tuple_access(tuple, *index, env),
        Expr::IfElse {
            condition,
            then_branch,
            else_branch,
        } => type_check_if_else(condition, then_branch, else_branch, env),
        Expr::StmtExpr(stmt) => {
            //type_check_stmt(stmt, env)?; // Ensure the statement is valid
            Ok(Type::Unit) // StmtExpr always evaluates to Unit
        }
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

/// Type-checks a tuple expression.
fn type_check_tuple(elements: &[Expr], env: &TypeEnv) -> Result<Type, String> {
    let mut types = Vec::new();
    for element in elements {
        types.push(type_check_expr(element, env)?);
    }
    Ok(Type::Tuple(types))
}

/// Type-checks tuple access expressions.
fn type_check_tuple_access(tuple: &Expr, index: usize, env: &TypeEnv) -> Result<Type, String> {
    let tuple_type = type_check_expr(tuple, env)?;
    if let Type::Tuple(types) = tuple_type {
        if index < types.len() {
            Ok(types[index].clone())
        } else {
            Err(format!(
                "Tuple index out of bounds: {} (tuple has {} elements)",
                index,
                types.len()
            ))
        }
    } else {
        Err(format!(
            "Cannot access index {} on non-tuple type: {:?}",
            index, tuple_type
        ))
    }
}

/// Type-checks an `IfElse` expression.
fn type_check_if_else(
    condition: &Expr,
    then_branch: &[Expr],
    else_branch: &Option<Vec<Expr>>,
    env: &TypeEnv,
) -> Result<Type, String> {
    // Check that the condition is a boolean
    let condition_type = type_check_expr(condition, env)?;
    if condition_type != Type::Bool {
        return Err(format!(
            "IfElse condition must be Bool, found {:?}",
            condition_type
        ));
    }

    // Check the type of the last expression in the then branch
    let then_type = if let Some(last_expr) = then_branch.last() {
        type_check_expr(last_expr, env)?
    } else {
        Type::Unit // Empty branch defaults to Unit
    };

    // Check the type of the last expression in the else branch (if it exists)
    let else_type = if let Some(else_branch) = else_branch {
        if let Some(last_expr) = else_branch.last() {
            type_check_expr(last_expr, env)?
        } else {
            Type::Unit // Empty branch defaults to Unit
        }
    } else {
        Type::Unit // No else branch defaults to Unit
    };

    // Ensure the then and else branches have the same type
    if then_type != else_type {
        return Err(format!(
            "IfElse branches must have the same type, found {:?} and {:?}",
            then_type, else_type
        ));
    }

    Ok(then_type) // The type of the IfElse expression is the type of its branches
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

    mod tuple_tests {
        use super::*;

        #[test]
        fn test_tuple_creation() {
            let env = TypeEnv::new();

            let expr = Expr::Tuple(vec![Expr::Int(1), Expr::Bool(true)]);
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Tuple(vec![Type::Int, Type::Bool])));
        }

        #[test]
        fn test_tuple_access_valid() {
            let env = TypeEnv::new();

            let expr = Expr::TupleAccess {
                tuple: Box::new(Expr::Tuple(vec![Expr::Int(1), Expr::Bool(true)])),
                index: 1,
            };
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Bool));
        }

        #[test]
        fn test_tuple_access_out_of_bounds() {
            let env = TypeEnv::new();

            let expr = Expr::TupleAccess {
                tuple: Box::new(Expr::Tuple(vec![Expr::Int(1), Expr::Bool(true)])),
                index: 2, // Out of bounds
            };
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }

        #[test]
        fn test_tuple_access_non_tuple() {
            let env = TypeEnv::new();

            let expr = Expr::TupleAccess {
                tuple: Box::new(Expr::Int(42)), // Not a tuple
                index: 0,
            };
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }
    }

    mod if_else_tests {
        use super::*;

        #[test]
        fn test_if_else_with_same_branch_types() {
            let env = TypeEnv::new();

            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(true)),
                then_branch: vec![Expr::Int(1)],
                else_branch: Some(vec![Expr::Int(2)]),
            };
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Int));
        }

        #[test]
        fn test_if_else_with_unit_branches() {
            let env = TypeEnv::new();

            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(false)),
                then_branch: vec![],
                else_branch: None,
            };
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Unit));
        }

        #[test]
        fn test_if_else_with_mismatched_branch_types() {
            let env = TypeEnv::new();

            let expr = Expr::IfElse {
                condition: Box::new(Expr::Bool(true)),
                then_branch: vec![Expr::Int(1)],
                else_branch: Some(vec![Expr::Bool(false)]), // Mismatched types
            };
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }

        #[test]
        fn test_if_else_with_non_boolean_condition() {
            let env = TypeEnv::new();

            let expr = Expr::IfElse {
                condition: Box::new(Expr::Int(1)), // Invalid: condition is not Bool
                then_branch: vec![Expr::Int(1)],
                else_branch: Some(vec![Expr::Int(2)]),
            };
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }
    }

    mod stmt_expr_tests {
        use super::*;
        use crate::ast::Stmt;

        #[test]
        fn test_stmt_expr_with_valid_statement() {
            let env = TypeEnv::new();

            let expr = Expr::StmtExpr(Box::new(Stmt::Print(Expr::String(
                "Hello, world!".to_string(),
            ))));
            let result = type_check_expr(&expr, &env);
            assert_eq!(result, Ok(Type::Unit)); // StmtExpr always evaluates to Unit
        }

        #[test]
        fn test_stmt_expr_with_invalid_statement() {
            let env = TypeEnv::new();

            let expr = Expr::StmtExpr(Box::new(Stmt::Reassignment {
                name: "x".to_string(),
                expr: Expr::Bool(true), // Assume `x` is not declared in the environment
            }));
            let result = type_check_expr(&expr, &env);
            assert!(result.is_err());
        }
    }
}
