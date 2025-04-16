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
fn parse_fun_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    //println!("[parse_fun_stmt] Parsing function statement...");
    //println!("[parse_fun_stmt] Current token: {:?}", input.first());
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

    let (input, _) = tag_token(Token::RParen)(input)?;
    let input = skip_ignored(input);

    // Parse return type
    let (input, _) = tag_token(Token::Colon)(input)?;
    let input = skip_ignored(input);
    let (input, return_type) = parse_type(input)?;
    let input = skip_ignored(input);

    /*
    println!("[parse_fun_stmt] Parsing function: {:?}", name);
    println!("[parse_fun_stmt] Parameters: {:?}", params);
    println!("[parse_fun_stmt] Return type: {:?}", return_type);
    println!("[parse_fun_stmt] Parsing body...");
    */

    // Parse function body
    let (input, body) = crate::parser::parse_block_stmt(input)?;

    /*
    println!("[parse_fun_stmt] Function body parsed.");
    println!("[parse_fun_stmt] Parsing complete.");
    println!("[parse_fun_stmt] Function: {:?}", name);
    println!("[parse_fun_stmt] Parameters: {:?}", params);
    println!("[parse_fun_stmt] Return type: {:?}", return_type);
    println!("[parse_fun_stmt] Body: {:?}", body);
    println!("[parse_fun_stmt] Parsing complete.");
    */

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
/// Type Parser
/// ------------------------------------------------------------------
/// #### Parses type annotations like `Int`, `Bool`, `String`, etc.
fn parse_type(input: Tokens) -> IResult<Tokens, Type> {
    match input.split_first() {
        Some((Token::IntType, rest)) => Ok((rest, Type::Int)),
        Some((Token::BoolType, rest)) => Ok((rest, Type::Bool)),
        Some((Token::StringType, rest)) => Ok((rest, Type::String)),
        Some((Token::UnitType, rest)) => Ok((rest, Type::Unit)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

// ============================== Tests ==============================

#[cfg(test)]
mod stmt_ests {
    use super::*;
    //use crate::lexer::lex;
    use crate::token::Token;

    mod let_stmt {
        use super::*;

        // testing let x: Int = 5;
        #[test]
        fn test_parse_let_stmt() {
            let input = vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Colon,
                Token::IntType,
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
            ];
            let expected = Stmt::Let {
                name: "x".to_string(),
                ty: Some(Type::Int),
                expr: crate::ast::Expr::Int(5),
            };
            let result = parse_stmt(&input).unwrap();
            assert_eq!(result.1, expected);
        }

        // testing let x = 5; (without type)
        #[test]
        fn test_parse_let_stmt_without_type() {
            let input = vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
            ];
            let expected = Stmt::Let {
                name: "x".to_string(),
                ty: None,
                expr: crate::ast::Expr::Int(5),
            };
            let result = parse_stmt(&input).unwrap();
            assert_eq!(result.1, expected);
        }
    }
}
