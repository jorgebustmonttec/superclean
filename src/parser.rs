use nom::{IResult, Parser, branch::alt, error::ErrorKind};

use crate::ast::{BinOp, Expr};
use crate::token::Token;

type Tokens<'a> = &'a [Token];

/// Entry point for expression parsing. Dispatches to the appropriate expression parser.
///
/// # Arguments
/// - `input`: A slice of tokens.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: Remaining tokens and parsed expression.
/// Entry point for parsing any expression.
/// This includes control flow (`if`), literals (bool, int, float),
/// and binary math expressions.
///
/// # Arguments
/// - `input`: A slice of tokens.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: The parsed expression and remaining tokens.
pub fn parse_expr(input: Tokens) -> IResult<Tokens, Expr> {
    alt((
        parse_if_else,
        parse_add_sub,
        parse_int,
        parse_float,
        parse_bool,
    ))
    .parse(input)
}

/// Parses addition and subtraction (lowest precedence)
fn parse_add_sub(input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input, mut expr) = parse_mul_div_mod.parse(input)?;

    loop {
        let op = if let Some((Token::Plus, rest)) = input.split_first() {
            input = rest;
            Some(BinOp::Add)
        } else if let Some((Token::Minus, rest)) = input.split_first() {
            input = rest;
            Some(BinOp::Sub)
        } else {
            break;
        };

        let (new_input, rhs) = parse_mul_div_mod.parse(input)?;
        input = new_input;
        expr = Expr::BinOp {
            left: Box::new(expr),
            op: op.unwrap(),
            right: Box::new(rhs),
        };
    }

    Ok((input, expr))
}

/// Parses multiplication, division, modulo (medium precedence)
fn parse_mul_div_mod(input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input, mut expr) = parse_primary.parse(input)?;

    loop {
        let op = match input.split_first() {
            Some((Token::Star, rest)) => {
                input = rest;
                Some(BinOp::Mul)
            }
            Some((Token::Slash, rest)) => {
                input = rest;
                Some(BinOp::Div)
            }
            Some((Token::Percent, rest)) => {
                input = rest;
                Some(BinOp::Mod)
            }
            _ => break,
        };

        let (new_input, rhs) = parse_primary.parse(input)?;
        input = new_input;
        expr = Expr::BinOp {
            left: Box::new(expr),
            op: op.unwrap(),
            right: Box::new(rhs),
        };
    }

    Ok((input, expr))
}

/// Parses primitive expressions like ints, floats, bools, or if-else
fn parse_primary(input: Tokens) -> IResult<Tokens, Expr> {
    alt((parse_if_else, parse_int, parse_float, parse_bool)).parse(input)
}

/// Parses integer literals
fn parse_int(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::Integer(n), rest)) => Ok((rest, Expr::Int(*n))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses float literals
fn parse_float(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::Float(f), rest)) => Ok((rest, Expr::Float(*f))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses boolean literals (`true` or `false`)
fn parse_bool(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::True, rest)) => Ok((rest, Expr::Bool(true))),
        Some((Token::False, rest)) => Ok((rest, Expr::Bool(false))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses `if cond { then } else { else }` expressions
fn parse_if_else(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, _) = tag_token(Token::If)(input)?;
    let (input, condition) = parse_expr(input)?;
    let (input, then_branch) = parse_block_expr(input)?;
    let (input, _) = tag_token(Token::Else)(input)?;
    let (input, else_branch) = parse_block_expr(input)?;

    Ok((
        input,
        Expr::IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        },
    ))
}

/// Matches a specific token
fn tag_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| match input.split_first() {
        Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// Parses a block expression `{ expr }`
fn parse_block_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, _) = tag_token(Token::LBrace)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, expr))
}

// ======================== Tests =========================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    // ========================== Boolean Literal ==========================

    /// Test cases for the expression parser
    ///
    /// Takes in a token for a boolean literal and checks if it is parsed correctly.
    ///
    /// The expected result is an `Expr::Bool` variant with the corresponding boolean value.
    #[test]
    fn test_parse_bool() {
        let tokens = vec![Token::True];
        let result = parse_bool(&tokens);
        assert!(result.is_ok());
        let (remaining, expr) = result.unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(expr, Expr::Bool(true));
    }

    // ========================== If-Else Expression ==========================

    /// Test cases for the if-else expression parser
    ///
    /// Takes in a series of tokens representing an if-else expression
    /// ( if true { true } else { false } )
    /// and checks if it is parsed correctly.
    ///
    /// The expected result is an `Expr::IfElse` variant with the corresponding condition and branches.
    #[test]
    fn test_parse_if_else() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LBrace,
            Token::True,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::False,
            Token::RBrace,
        ];
        let result = parse_if_else(&tokens);
        assert!(result.is_ok());
        let (remaining, expr) = result.unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(
            expr,
            Expr::IfElse {
                condition: Box::new(Expr::Bool(true)),
                then_branch: Box::new(Expr::Bool(true)),
                else_branch: Box::new(Expr::Bool(false)),
            }
        );
    }

    /// Test for nested if-else expressions
    ///
    /// Takes in a series of tokens representing a nested if-else expression
    /// ( if true { if false { true } else { false } } else { false } )
    /// and checks if it is parsed correctly.
    ///
    /// The expected result is an `Expr::IfElse` variant with the corresponding condition and branches.
    #[test]
    fn test_parse_nested_if_else() {
        let tokens = vec![
            Token::If,
            Token::True,
            Token::LBrace,
            Token::If,
            Token::False,
            Token::LBrace,
            Token::True,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::False,
            Token::RBrace,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::False,
            Token::RBrace,
        ];
        let result = parse_if_else(&tokens);
        assert!(result.is_ok());
        let (remaining, expr) = result.unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(
            expr,
            Expr::IfElse {
                condition: Box::new(Expr::Bool(true)),
                then_branch: Box::new(Expr::IfElse {
                    condition: Box::new(Expr::Bool(false)),
                    then_branch: Box::new(Expr::Bool(true)),
                    else_branch: Box::new(Expr::Bool(false)),
                }),
                else_branch: Box::new(Expr::Bool(false)),
            }
        );
    }

    // test for the following code:
    //
}
