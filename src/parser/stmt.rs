use crate::ast::{Stmt, Type};
use crate::parser::Tokens;
use crate::parser::{parse_expr, tag_token};
use crate::token::Token;
use nom::{IResult, Parser, branch::alt, error::ErrorKind};

/// ------------------------------------------------------------------
/// Statement Parser
/// ------------------------------------------------------------------
/// #### Parses any statement (e.g., variable declarations)
/// ----------------------------------------------------
pub fn parse_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    alt((parse_let_stmt, parse_expr_stmt)).parse(input)
}

/// ------------------------------------------------------------------
/// Let Statement Parser
/// ------------------------------------------------------------------
/// #### Parses variable declarations like `let x = 5;` or `let x: Int = 5;`
fn parse_let_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let (input, _) = tag_token(Token::Let)(input)?;

    let token = input
        .first()
        .ok_or_else(|| nom::Err::Error(nom::error::Error::new(input, ErrorKind::Tag)))?;

    let name = if let Token::Identifier(name) = token {
        name.clone()
    } else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        )));
    };

    let mut input = &input[1..];

    let (new_input, ty) = if let Some((Token::Colon, rest)) = input.split_first() {
        let (input, ty) = parse_type(&rest)?;
        (input, Some(ty))
    } else {
        (input, None)
    };
    input = new_input;

    let (input, _) = tag_token(Token::Equal)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::Semicolon)(input)?;

    Ok((input, Stmt::Let { name, ty, expr }))
}

/// ------------------------------------------------------------------
/// Expression Statement Parser
/// ------------------------------------------------------------------
/// #### Wraps expressions as standalone statements.
fn parse_expr_stmt(input: Tokens) -> IResult<Tokens, Stmt> {
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::Semicolon)(input)?;
    Ok((input, Stmt::Expr(expr)))
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
