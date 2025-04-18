use crate::ast::{Stmt, Type};
use crate::parser::Tokens;
use crate::parser::{parse_expr, skip_ignored, tag_token};
use crate::token::Token;
use nom::{IResult, Parser, branch::alt, error::ErrorKind};

/// ------------------------------------------------------------------
/// Statement Parser
/// ------------------------------------------------------------------
/// #### Parses any statement (e.g., variable declarations)
/// ----------------------------------------------------
pub fn parse_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    //println!("[parse_stmt] Current token: {:?}", input.first());
    alt((
        parse_let_stmt,
        parse_return_stmt,
        parse_print_stmt,
        parse_fun_stmt,
        parse_expr_stmt,
        parse_reassignment_stmt,
        parse_break_stmt,
        parse_while_stmt,
    ))
    .parse(input)
}

/// ------------------------------------------------------------------
/// Simplified Let Statement Parser
/// ------------------------------------------------------------------
/// #### Parses variable declarations like `let x = 5;` or `let x: Int = 5;`
fn parse_let_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Let)(input)?;
    let input = skip_ignored(input);
    let (input, name) = match input.split_first() {
        Some((Token::Identifier(name), rest)) => (rest, name.clone()),
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };
    let input = skip_ignored(input);

    let (input, ty) = if let Some((Token::Colon, rest)) = input.split_first() {
        let input = skip_ignored(rest);

        let (input, ty) = parse_type(input)?;
        (input, Some(ty))
    } else {
        (input, None)
    };

    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Equal)(input)?;
    let input = skip_ignored(input);
    let (input, expr) = parse_expr(input)?;
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Semicolon)(input)?;

    Ok((input, Stmt::Let { name, ty, expr }))
}

/// ------------------------------------------------------------------
/// Expression Statement Parser
/// ------------------------------------------------------------------
/// #### Wraps expressions as standalone statements.
fn parse_expr_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    //println!("[parse_expr_stmt] Parsing expression statement...");
    //println!("[parse_expr_stmt] Current token: {:?}", input.first());
    let input = skip_ignored(input);
    let (input, expr) = parse_expr(input)?;
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Semicolon)(input)?;
    Ok((input, Stmt::Expr(expr)))
}

/// ------------------------------------------------------------------
/// Function Statement Parser
/// ------------------------------------------------------------------
/// #### Parses function definitions like `fun add(a: Int, b: Int): Int { ... }`
/// or `fun main() { ... }` with an optional return type.
fn parse_fun_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Fun)(input)?;
    let input = skip_ignored(input);

    // Parse function name
    let (input, name) = match input.split_first() {
        Some((Token::Identifier(name), rest)) => (rest, name.clone()),
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };
    let input = skip_ignored(input);

    // Parse parameters
    let (input, _) = tag_token(Token::LParen)(input)?;
    let mut params = Vec::new();
    let mut input = skip_ignored(input);

    while let Some(tok) = input.first() {
        if *tok == Token::RParen {
            break;
        }

        // Parse parameter name
        let (new_input, param_name) = match input.split_first() {
            Some((Token::Identifier(name), rest)) => (rest, name.clone()),
            _ => {
                return Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    ErrorKind::Tag,
                )));
            }
        };
        input = skip_ignored(new_input);

        // Parse colon and type
        let (new_input, _) = tag_token(Token::Colon)(input)?;
        input = skip_ignored(new_input);
        let (new_input, param_type) = parse_type(input)?;
        input = skip_ignored(new_input);

        params.push((param_name, param_type));

        // Check for comma or closing parenthesis
        if let Some((Token::Comma, rest)) = input.split_first() {
            input = skip_ignored(rest);
        } else {
            break;
        }
    }

    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::RParen)(input)?;
    let input = skip_ignored(input);

    // Parse optional return type
    let (input, return_type) = if let Some((Token::Colon, rest)) = input.split_first() {
        let input = skip_ignored(rest);
        let (input, ty) = parse_type(input)?;
        (input, ty)
    } else {
        (input, Type::Unit) // Default to Unit type if no colon is found
    };
    let input = skip_ignored(input);

    // Parse function body
    let (input, body) = crate::parser::parse_block_stmt(input)?;

    Ok((
        input,
        Stmt::Fun {
            name,
            params,
            return_type,
            body,
        },
    ))
}

/// ------------------------------------------------------------------
/// Print Statement Parser
/// ------------------------------------------------------------------
/// #### Parses print statements like `print(expr);`
fn parse_print_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    //println!("[parse_print_stmt] Parsing print statement...");
    //println!("[parse_print_stmt] Current token: {:?}", input.first());
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Print)(input)?;
    let input = skip_ignored(input);

    // Parse the single argument inside parentheses
    let (input, _) = tag_token(Token::LParen)(input)?;
    let input = skip_ignored(input);

    let (input, arg) = parse_expr(input)?;
    let input = skip_ignored(input);

    let (input, _) = tag_token(Token::RParen)(input)?;
    let input = skip_ignored(input);

    let (input, _) = tag_token(Token::Semicolon)(input)?;
    let input = skip_ignored(input);

    Ok((input, Stmt::Print(arg)))
}

/// ------------------------------------------------------------------
/// Return Statement Parser
/// ------------------------------------------------------------------
/// #### Parses return statements like `return;` or `return expr;`
fn parse_return_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    //println!("[parse_return_stmt] Parsing return statement...");
    //println!("[parse_return_stmt] Current token: {:?}", input.first());
    let (input, _) = tag_token(Token::Return)(input)?;
    let input = skip_ignored(input);

    // Check if there's an expression after `return`
    if let Ok((input, expr)) = parse_expr(input) {
        let input = skip_ignored(input);
        let (input, _) = tag_token(Token::Semicolon)(input)?;

        Ok((input, Stmt::Return(Some(expr))))
    } else {
        let (input, _) = tag_token(Token::Semicolon)(input)?;

        Ok((input, Stmt::Return(None)))
    }
}

/// ------------------------------------------------------------------
/// Break Statement Parser
/// ------------------------------------------------------------------
/// #### Parses break statements like `break;`
fn parse_break_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Break)(input)?;
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Semicolon)(input)?;
    Ok((input, Stmt::Break))
}

/// ------------------------------------------------------------------
/// While Statement Parser
/// ------------------------------------------------------------------
/// #### Parses while loops like `while <condition> { <body> }`
fn parse_while_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::While)(input)?;
    let input = skip_ignored(input);

    // Parse the condition expression
    let (input, condition) = parse_expr(input)?;
    let input = skip_ignored(input);

    // Parse the block statement
    let (input, body) = crate::parser::parse_block_stmt(input)?;

    Ok((input, Stmt::While { condition, body }))
}

/// ------------------------------------------------------------------
/// Type Parser
/// ------------------------------------------------------------------
/// #### Parses type annotations like `Int`, `Bool`, `String`, etc.
fn parse_type(input: Tokens) -> IResult<Tokens, Type> {
    match input.split_first() {
        Some((Token::IntType, rest)) => Ok((rest, Type::Int)),
        Some((Token::BoolType, rest)) => Ok((rest, Type::Bool)),
        Some((Token::StringType, rest)) => Ok((rest, Type::String)),
        Some((Token::UnitType, rest)) => Ok((rest, Type::Unit)),
        //tuple type:
        Some((Token::LParen, rest)) => {
            let (rest, types) = parse_type_list(rest)?;
            Ok((rest, Type::Tuple(types)))
        }

        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses a list of types enclosed in parentheses, e.g., `(Int, Bool)`
fn parse_type_list(input: Tokens) -> IResult<Tokens, Vec<Type>> {
    let mut types = Vec::new();
    let mut input = skip_ignored(input);

    while let Some(tok) = input.first() {
        if *tok == Token::RParen {
            break;
        }

        // Parse the type
        let (new_input, ty) = parse_type(input)?;
        types.push(ty);
        input = skip_ignored(new_input);

        // Check for comma or closing parenthesis
        if let Some((Token::Comma, rest)) = input.split_first() {
            input = skip_ignored(rest);
        } else {
            break;
        }
    }

    let (input, _) = tag_token(Token::RParen)(input)?;
    Ok((input, types))
}

/// ------------------------------------------------------------------
/// reassignment Parser
/// ------------------------------------------------------------------
/// #### Parses variable reassignments like `x = 5;` or `y = y+ 1;`
fn parse_reassignment_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let input = skip_ignored(input);
    let (input, name) = match input.split_first() {
        Some((Token::Identifier(name), rest)) => (rest, name.clone()),
        _ => {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    };
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Equal)(input)?;
    let input = skip_ignored(input);
    let (input, expr) = parse_expr(input)?;
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::Semicolon)(input)?;

    Ok((input, Stmt::Reassignment { name, expr }))
}

// ============================== Tests ==============================

#[cfg(test)]
mod stmt_tests {
    use super::*;
    use crate::lexer::lex;

    mod let_stmt_tests {
        use super::*;

        #[test]
        fn test_let_with_type() {
            let code = "let x: Int = 5;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Let {
                    name: "x".to_string(),
                    ty: Some(Type::Int),
                    expr: crate::ast::Expr::Int(5),
                }
            );
        }

        #[test]
        fn test_let_without_type() {
            let code = "let x = 5;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Let {
                    name: "x".to_string(),
                    ty: None,
                    expr: crate::ast::Expr::Int(5),
                }
            );
        }

        #[test]
        fn test_let_with_tuple_type() {
            let code = "let x: (Int, Bool) = (5, true);";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Let {
                    name: "x".to_string(),
                    ty: Some(Type::Tuple(vec![Type::Int, Type::Bool])),
                    expr: crate::ast::Expr::Tuple(vec![
                        crate::ast::Expr::Int(5),
                        crate::ast::Expr::Bool(true)
                    ]),
                }
            );
        }
    }

    mod return_stmt_tests {
        use super::*;

        #[test]
        fn test_return_with_expr() {
            let code = "return 42;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(result.1, Stmt::Return(Some(crate::ast::Expr::Int(42))));
        }

        #[test]
        fn test_return_without_expr() {
            let code = "return;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(result.1, Stmt::Return(None));
        }
    }

    mod print_stmt_tests {
        use super::*;

        #[test]
        fn test_print_with_string() {
            let code = "print(\"Hello, world!\");";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Print(crate::ast::Expr::String("Hello, world!".to_string()))
            );
        }

        #[test]
        fn test_print_with_variable() {
            let code = "print(x);";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Print(crate::ast::Expr::Variable("x".to_string()))
            );
        }
    }

    mod fun_stmt_tests {
        use super::*;

        #[test]
        fn test_function_with_return_type() {
            let code = "fun add(a: Int, b: Int): Int { return a + b; }";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert!(matches!(result.1, Stmt::Fun { .. }));
        }

        #[test]
        fn test_function_without_return_type() {
            let code = "fun main() { print(\"Hello\"); }";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert!(matches!(result.1, Stmt::Fun { .. }));
        }
    }

    mod reassignment_stmt_tests {
        use super::*;

        #[test]
        fn test_reassignment() {
            let code = "x = x + 1;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::Reassignment {
                    name: "x".to_string(),
                    expr: crate::ast::Expr::BinOp {
                        left: Box::new(crate::ast::Expr::Variable("x".to_string())),
                        op: crate::ast::BinOp::Add,
                        right: Box::new(crate::ast::Expr::Int(1)),
                    },
                }
            );
        }
    }

    mod while_stmt_tests {
        use super::*;

        #[test]
        fn test_while_loop() {
            let code = "while x < 10 { x = x + 1; }";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::While {
                    condition: crate::ast::Expr::BinOp {
                        left: Box::new(crate::ast::Expr::Variable("x".to_string())),
                        op: crate::ast::BinOp::Less,
                        right: Box::new(crate::ast::Expr::Int(10)),
                    },
                    body: vec![Stmt::Reassignment {
                        name: "x".to_string(),
                        expr: crate::ast::Expr::BinOp {
                            left: Box::new(crate::ast::Expr::Variable("x".to_string())),
                            op: crate::ast::BinOp::Add,
                            right: Box::new(crate::ast::Expr::Int(1)),
                        },
                    }],
                }
            );
        }

        #[test]
        fn test_while_with_break() {
            let code = "while true { break; }";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::While {
                    condition: crate::ast::Expr::Bool(true),
                    body: vec![Stmt::Break],
                }
            );
        }

        #[test]
        fn test_nested_while_loops() {
            let code = "while x < 10 { while y < 5 { break; } x = x + 1; }";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(
                result.1,
                Stmt::While {
                    condition: crate::ast::Expr::BinOp {
                        left: Box::new(crate::ast::Expr::Variable("x".to_string())),
                        op: crate::ast::BinOp::Less,
                        right: Box::new(crate::ast::Expr::Int(10)),
                    },
                    body: vec![
                        Stmt::While {
                            condition: crate::ast::Expr::BinOp {
                                left: Box::new(crate::ast::Expr::Variable("y".to_string())),
                                op: crate::ast::BinOp::Less,
                                right: Box::new(crate::ast::Expr::Int(5)),
                            },
                            body: vec![Stmt::Break],
                        },
                        Stmt::Reassignment {
                            name: "x".to_string(),
                            expr: crate::ast::Expr::BinOp {
                                left: Box::new(crate::ast::Expr::Variable("x".to_string())),
                                op: crate::ast::BinOp::Add,
                                right: Box::new(crate::ast::Expr::Int(1)),
                            },
                        },
                    ],
                }
            );
        }
    }

    mod break_stmt_tests {
        use super::*;

        #[test]
        fn test_break_statement() {
            let code = "break;";
            let tokens = lex(code).unwrap();
            let result = parse_stmt(&tokens).unwrap();
            assert_eq!(result.1, Stmt::Break);
        }
    }
}
