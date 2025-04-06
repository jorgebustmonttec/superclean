use nom::{IResult, Parser, branch::alt};

use crate::ast::BinOp;
use crate::ast::Expr;
use crate::token::Token;
use nom::combinator::map;
use nom::multi::fold_many0;

type Tokens<'a> = &'a [Token];

/// ---------------------------------------------------------
/// Parser for expressions
/// ---------------------------------------------------------
/// This parser handles the parsing of expressions in the language.
/// It can parse boolean literals, integer literals, binary operations, and if-else expressions.
/// The order of parsing reflects precedence: binary ops > literals > if-else.
///
/// # Arguments
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: A result containing the remaining tokens and the parsed expression.
pub fn parse_expr(input: Tokens) -> IResult<Tokens, Expr> {
    alt((parse_if_else, parse_add_sub)).parse(input)
}

/// ---------------------------------------------------------
/// Parser for boolean literals
/// ---------------------------------------------------------
/// This parser handles the parsing of boolean literals (`true` and `false`).
/// It matches the tokens `Token::True` and `Token::False`
/// and returns the corresponding `Expr::Bool` variant.
///
/// # Arguments
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: A result containing the remaining tokens and the parsed boolean expression.
/// If the input does not match either `true` or `false`, it returns an error.
fn parse_bool(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::True, rest)) => Ok((rest, Expr::Bool(true))),
        Some((Token::False, rest)) => Ok((rest, Expr::Bool(false))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// ---------------------------------------------------------
/// Parser for if-else expressions
/// ---------------------------------------------------------
/// This parser handles the parsing of if-else expressions.
/// It matches the `if` keyword, followed by a condition expression,
/// a `then` branch, an `else` keyword, and an `else` branch.
/// It returns an `Expr::IfElse` variant containing the condition and branches.
///
/// # Arguments
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: A result containing the remaining tokens and the parsed if-else expression.
/// If the input does not match the expected structure, it returns an error.
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

/// ---------------------------------------------------------
/// Parser for integer literals
/// ---------------------------------------------------------
/// This parser handles parsing of integer literals.
/// It matches a `Token::Integer` and wraps the value in `Expr::Int`.
///
/// # Arguments
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: The remaining input and the parsed integer expression.
fn parse_int(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::Integer(n), rest)) => Ok((rest, Expr::Int(*n))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// ---------------------------------------------------------
/// Parses addition and subtraction expressions
/// ---------------------------------------------------------
/// Handles expressions like `1 + 2 - 3`, respecting left-to-right associativity.
fn parse_add_sub(mut input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input_, mut expr) = parse_mul_div(input)?;

    loop {
        match input_.split_first() {
            Some((Token::Plus, rest)) => {
                let (rest, rhs) = parse_mul_div(rest)?;
                expr = Expr::BinaryOp {
                    left: Box::new(expr),
                    op: BinOp::Add,
                    right: Box::new(rhs),
                };
                input_ = rest;
            }
            Some((Token::Minus, rest)) => {
                let (rest, rhs) = parse_mul_div(rest)?;
                expr = Expr::BinaryOp {
                    left: Box::new(expr),
                    op: BinOp::Sub,
                    right: Box::new(rhs),
                };
                input_ = rest;
            }
            _ => break,
        }
    }

    Ok((input_, expr))
}

/// ---------------------------------------------------------
/// Parses multiplication and division expressions
/// ---------------------------------------------------------
/// Handles expressions like `2 * 3 / 4`, respecting left-to-right associativity.
fn parse_mul_div(mut input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input_, mut expr) = parse_primary(input)?;

    loop {
        match input_.split_first() {
            Some((Token::Star, rest)) => {
                let (rest, rhs) = parse_primary(rest)?;
                expr = Expr::BinaryOp {
                    left: Box::new(expr),
                    op: BinOp::Mul,
                    right: Box::new(rhs),
                };
                input_ = rest;
            }
            Some((Token::Slash, rest)) => {
                let (rest, rhs) = parse_primary(rest)?;
                expr = Expr::BinaryOp {
                    left: Box::new(expr),
                    op: BinOp::Div,
                    right: Box::new(rhs),
                };
                input_ = rest;
            }
            _ => break,
        }
    }

    Ok((input_, expr))
}

/// ---------------------------------------------------------
/// Parses literals or parenthesized expressions
/// ---------------------------------------------------------
/// This is the base level of expression parsing, supporting
/// integer literals, booleans, and expressions in parentheses.
fn parse_primary(input: Tokens) -> IResult<Tokens, Expr> {
    alt((parse_paren_expr, parse_bool, parse_int)).parse(input)
}

/// ---------------------------------------------------------
/// Parses expressions inside parentheses
/// ---------------------------------------------------------
/// Matches a parenthesized expression like `(1 + 2)`.
fn parse_paren_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, _) = tag_token(Token::LParen)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::RParen)(input)?;
    Ok((input, expr))
}

/// ---------------------------------------------------------
/// Helper function to match a specific token
/// ---------------------------------------------------------
/// This function checks if the first token in the input matches the expected token.
/// If it does, it returns the remaining tokens; otherwise, it returns an error.
/// It is used to match specific keywords and symbols in the language.
///
/// # Arguments
/// - `expected`: The expected token to match.
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Token>`: A result containing the remaining tokens and the matched token.
/// If the input does not match the expected token, it returns an error.
fn tag_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| match input.split_first() {
        Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

/// ---------------------------------------------------------
/// Parser for block expressions
/// ---------------------------------------------------------
/// This parser handles the parsing of block expressions enclosed in braces (`{}`).
/// It matches the opening brace, parses an expression inside the braces,
/// and then matches the closing brace.
/// It returns the parsed expression.
///
/// # Arguments
/// - `input`: A slice of tokens to parse.
///
/// # Returns
/// - `IResult<Tokens, Expr>`: A result containing the remaining tokens and the parsed block expression.
/// If the input does not match the expected structure, it returns an error.
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

    #[test]
    fn test_operator_precedence() {
        use BinOp::*;
        use Expr::*;

        let tokens = vec![
            Token::Integer(1),
            Token::Plus,
            Token::Integer(2),
            Token::Star,
            Token::Integer(3),
        ];

        let result = parse_expr(&tokens);
        assert!(result.is_ok());
        let (_, expr) = result.unwrap();

        assert_eq!(
            expr,
            BinaryOp {
                left: Box::new(Int(1)),
                op: Add,
                right: Box::new(BinaryOp {
                    left: Box::new(Int(2)),
                    op: Mul,
                    right: Box::new(Int(3)),
                })
            }
        );
    }

    #[test]
    fn test_parentheses_override_precedence() {
        use BinOp::*;
        use Expr::*;

        let tokens = vec![
            Token::LParen,
            Token::Integer(1),
            Token::Plus,
            Token::Integer(2),
            Token::RParen,
            Token::Star,
            Token::Integer(3),
        ];

        let result = parse_expr(&tokens);
        assert!(result.is_ok());
        let (_, expr) = result.unwrap();

        assert_eq!(
            expr,
            BinaryOp {
                left: Box::new(BinaryOp {
                    left: Box::new(Int(1)),
                    op: Add,
                    right: Box::new(Int(2)),
                }),
                op: Mul,
                right: Box::new(Int(3)),
            }
        );
    }
}
