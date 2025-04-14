use crate::ast::{BinOp, Expr, UnaryOp};
use crate::parser::{parse_block_expr, skip_ignored, tag_token};
use crate::token::Token;
use nom::{IResult, Parser, branch::alt, error::ErrorKind};

type Tokens<'a> = &'a [Token];

/// ------------------------------------------------------------------
/// Expression Parser
/// ------------------------------------------------------------------
///
/// #### Entry point for expression parsing. Dispatches to the appropriate expression parser.
///
/// ----------------------------------------------------
///
/// ### Arguments
/// - `input`: A slice of tokens.
///
/// ### Returns
/// - `IResult<Tokens, Expr>`: The parsed expression and remaining tokens.
pub fn parse_expr(input: Tokens) -> IResult<Tokens, Expr> {
    parse_logical_or.parse(input)
}

/// ------------------------------------------------------------------
/// Logical OR Parser
/// ------------------------------------------------------------------
/// #### Parses logical OR (`||`) expressions. Lowest precedence.
fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (mut input, mut expr) = parse_logical_and.parse(input)?;

    loop {
        input = skip_ignored(input);
        if let Some((Token::Or, rest)) = input.split_first() {
            input = skip_ignored(rest);
            let (new_input, rhs) = parse_logical_and.parse(input)?;
            input = new_input;
            expr = Expr::BinOp {
                left: Box::new(expr),
                op: BinOp::Or,
                right: Box::new(rhs),
            };
        } else {
            break;
        }
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Logical AND Parser
/// ------------------------------------------------------------------
/// #### Parses logical AND (`&&`) expressions. Higher than OR.
fn parse_logical_and(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (mut input, mut expr) = parse_comparison.parse(input)?;

    loop {
        input = skip_ignored(input);
        if let Some((Token::And, rest)) = input.split_first() {
            input = skip_ignored(rest);
            let (new_input, rhs) = parse_comparison.parse(input)?;
            input = new_input;
            expr = Expr::BinOp {
                left: Box::new(expr),
                op: BinOp::And,
                right: Box::new(rhs),
            };
        } else {
            break;
        }
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Comparison Parser
/// ------------------------------------------------------------------
/// #### Parses comparison expressions: ==, !=, <, <=, >, >=
fn parse_comparison(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (mut input, mut expr) = parse_add_sub.parse(input)?;

    loop {
        input = skip_ignored(input);
        let (op, rest) = match input.split_first() {
            Some((Token::EqualEqual, r)) => (Some(BinOp::Equal), r),
            Some((Token::NotEqual, r)) => (Some(BinOp::NotEqual), r),
            Some((Token::Less, r)) => (Some(BinOp::Less), r),
            Some((Token::LessEqual, r)) => (Some(BinOp::LessEqual), r),
            Some((Token::Greater, r)) => (Some(BinOp::Greater), r),
            Some((Token::GreaterEqual, r)) => (Some(BinOp::GreaterEqual), r),
            _ => break,
        };

        input = skip_ignored(rest);
        let (new_input, rhs) = parse_add_sub.parse(input)?;
        input = new_input;
        expr = Expr::BinOp {
            left: Box::new(expr),
            op: op.unwrap(),
            right: Box::new(rhs),
        };
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Addition and Subtraction Parser
/// ------------------------------------------------------------------
/// #### Parses addition and subtraction expressions. Has low precedence
/// and is chained with multiplication and division (which has higher precedence).
fn parse_add_sub(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (mut input, mut expr) = parse_mul_div_mod.parse(input)?;

    loop {
        input = skip_ignored(input);
        let op = if let Some((Token::Plus, rest)) = input.split_first() {
            input = rest;
            Some(BinOp::Add)
        } else if let Some((Token::Minus, rest)) = input.split_first() {
            input = rest;
            Some(BinOp::Sub)
        } else {
            break;
        };

        input = skip_ignored(input);
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

/// ------------------------------------------------------------------
/// Multiplication / Division / Modulo Parser
/// ------------------------------------------------------------------
/// #### Parses multiplication, division, and modulo operations. Has medium precedence.
fn parse_mul_div_mod(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (mut input, mut expr) = parse_unary.parse(input)?;

    loop {
        input = skip_ignored(input);
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

        input = skip_ignored(input);
        let (new_input, rhs) = parse_unary.parse(input)?;
        input = new_input;
        expr = Expr::BinOp {
            left: Box::new(expr),
            op: op.unwrap(),
            right: Box::new(rhs),
        };
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Logical NOT and Unary Minus Parser
/// ------------------------------------------------------------------
/// #### Parses unary expressions like `-x` or `!x`.
/// `-` becomes UnaryOp::Neg and `!` becomes UnaryOp::Not.
fn parse_unary(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Minus, rest)) => {
            let rest = skip_ignored(rest);
            let (input, expr) = parse_unary(rest)?;
            Ok((
                input,
                Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(expr),
                },
            ))
        }
        Some((Token::Not, rest)) => {
            let rest = skip_ignored(rest);
            let (input, expr) = parse_unary(rest)?;
            Ok((
                input,
                Expr::UnaryOp {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                },
            ))
        }
        _ => parse_primary(input),
    }
}

/// ------------------------------------------------------------------
/// Primary Expression Parser
/// ------------------------------------------------------------------
/// #### Parses primary expressions. This includes literals and parenthesized expressions.
fn parse_primary(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    alt((
        parse_parens,
        parse_if_else,
        parse_int,
        parse_float,
        parse_bool,
        parse_string,
        parse_identifier,
    ))
    .parse(input)
}

/// ------------------------------------------------------------------
/// Parentheses Parser
/// ------------------------------------------------------------------
/// #### Parses parenthesized expressions. This is used to group expressions.
/// Parses parentheses on either side and returns the expression inside.
fn parse_parens(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::LParen)(input)?;
    let input = skip_ignored(input);
    let (input, expr) = parse_expr(input)?;
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::RParen)(input)?;
    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// If-Else Parser
/// ------------------------------------------------------------------
/// #### Parses if-else expressions.
/// uses the `parse_expr` function to parse the condition and branches.
/// The branches are expected to be block expressions.
fn parse_if_else(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::If)(input)?;
    let input = skip_ignored(input);
    let (input, condition) = parse_expr(input)?;
    let input = skip_ignored(input);
    let (input, then_branch) = parse_block_expr(input)?;
    let input = skip_ignored(input);

    let (input, else_branch) = if let Some((Token::Else, rest)) = input.split_first() {
        let input = skip_ignored(&rest);
        let (input, block) = parse_block_expr(input)?;
        (input, Some(Box::new(block)))
    } else {
        (input, None)
    };

    Ok((
        input,
        Expr::IfElse {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        },
    ))
}

/// ------------------------------------------------------------------
/// Integer Parser
/// ------------------------------------------------------------------
/// #### Parses integer literals.
fn parse_int(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Integer(n), rest)) => Ok((rest, Expr::Int(*n))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// ------------------------------------------------------------------
/// Float Parser
/// ------------------------------------------------------------------
/// #### Parses float literals.
fn parse_float(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Float(f), rest)) => Ok((rest, Expr::Float(*f))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// ------------------------------------------------------------------
/// Boolean Parser
/// ------------------------------------------------------------------
/// #### Parses boolean literals.
fn parse_bool(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::True, rest)) => Ok((rest, Expr::Bool(true))),
        Some((Token::False, rest)) => Ok((rest, Expr::Bool(false))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// ------------------------------------------------------------------
/// String Literal Parser
/// ------------------------------------------------------------------
/// #### Parses string literals like `"hello"`
fn parse_string(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::StringLiteral(s), rest)) => Ok((rest, Expr::String(s.clone()))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// ------------------------------------------------------------------
/// Identifier / Variable Parser
/// ------------------------------------------------------------------
/// #### Parses bare identifiers like `x`, `foo`, etc.
fn parse_identifier(input: Tokens) -> IResult<Tokens, Expr> {
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Identifier(name), rest)) => Ok((rest, Expr::Variable(name.clone()))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

// ======================== Tests =========================

#[cfg(test)]
mod expr_tests {
    use super::*;
    use crate::lexer::lex;
    use crate::token::Token;

    mod literal_tests {
        use super::*;

        #[test]
        fn test_parse_int() {
            let code = "42";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::Int(42));
        }

        #[test]
        fn test_parse_float() {
            let code = "3.14";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::Float(3.14));
        }

        #[test]
        fn test_parse_bool() {
            let code = "true";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::Bool(true));

            let code = "false";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::Bool(false));
        }

        #[test]
        fn test_parse_string() {
            let code = "\"hello\"";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::String("hello".to_string()));
        }
    }

    mod binary_op_tests {
        use super::*;

        #[test]
        fn test_addition() {
            let code = "1 + 2";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }

        #[test]
        fn test_multiplication_precedence() {
            let code = "1 + 2 * 3";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Add,
                    right: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Int(2)),
                        op: BinOp::Mul,
                        right: Box::new(Expr::Int(3)),
                    }),
                }
            );
        }

        #[test]
        fn test_logical_and_or() {
            let code = "true && false || true";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::BinOp {
                    left: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Bool(true)),
                        op: BinOp::And,
                        right: Box::new(Expr::Bool(false)),
                    }),
                    op: BinOp::Or,
                    right: Box::new(Expr::Bool(true)),
                }
            );
        }
    }

    mod unary_op_tests {
        use super::*;

        #[test]
        fn test_unary_minus() {
            let code = "-42";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::UnaryOp {
                    op: UnaryOp::Neg,
                    expr: Box::new(Expr::Int(42)),
                }
            );
        }

        #[test]
        fn test_logical_not() {
            let code = "!true";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::UnaryOp {
                    op: UnaryOp::Not,
                    expr: Box::new(Expr::Bool(true)),
                }
            );
        }
    }

    mod grouping_and_precedence_tests {
        use super::*;

        #[test]
        fn test_parentheses() {
            let code = "(1 + 2) * 3";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::BinOp {
                    left: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Int(1)),
                        op: BinOp::Add,
                        right: Box::new(Expr::Int(2)),
                    }),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Int(3)),
                }
            );
        }
    }

    mod conditional_tests {
        use super::*;

        #[test]
        fn test_if_else() {
            let code = "if true { 1 } else { 2 }";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::IfElse {
                    condition: Box::new(Expr::Bool(true)),
                    then_branch: Box::new(Expr::Int(1)),
                    else_branch: Some(Box::new(Expr::Int(2))),
                }
            );
        }

        #[test]
        fn test_if_without_else() {
            let code = "if false { 42 }";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(
                result.1,
                Expr::IfElse {
                    condition: Box::new(Expr::Bool(false)),
                    then_branch: Box::new(Expr::Int(42)),
                    else_branch: None,
                }
            );
        }
    }

    mod variable_and_identifier_tests {
        use super::*;

        #[test]
        fn test_variable() {
            let code = "x";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens).unwrap();
            assert_eq!(result.1, Expr::Variable("x".to_string()));
        }
    }
}
