use super::{TypeEnv, type_check_expr};
use crate::ast::Stmt;

/// Type-checks a statement.
pub fn type_check_stmt(stmt: &Stmt, env: &mut TypeEnv) -> Result<(), String> {
    match stmt {
        // `let` statements.
        Stmt::Let { name, ty, expr } => {
            let expr_type = type_check_expr(expr, env)?;

            // If a type annotation is provided, ensure it matches the expression's type.
            if let Some(expected_type) = ty {
                if *expected_type != expr_type {
                    return Err(format!(
                        "Type mismatch for variable '{}': expected {:?}, found {:?}",
                        name, expected_type, expr_type
                    ));
                }
            }

            // Add the variable to the environment.
            env.variables.insert(name.clone(), expr_type);
            Ok(())
        }

        // Unsupported statements for now.
        _ => Err(format!("Unsupported statement: {:?}", stmt)),
    }
}
