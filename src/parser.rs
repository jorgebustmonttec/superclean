use nom::{IResult, Parser, branch::alt, error::ErrorKind};

use crate::ast::{BinOp, Expr, UnaryOp};
use crate::token::Token;

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
/// Addition and Subtraction Parser
/// ------------------------------------------------------------------
/// #### Parses addition and subtraction expressions. Has low precedence
/// and is chained with multiplication and division (which has higher precedence).
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

/// ------------------------------------------------------------------
/// Multiplication / Division / Modulo Parser
/// ------------------------------------------------------------------
/// #### Parses multiplication, division, and modulo operations. Has medium precedence.
fn parse_mul_div_mod(input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input, mut expr) = parse_unary.parse(input)?;

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
/// Primary Expression Parser
/// ------------------------------------------------------------------
/// #### Parses primary expressions. This includes literals and parenthesized expressions.
fn parse_primary(input: Tokens) -> IResult<Tokens, Expr> {
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
    let (input, _) = tag_token(Token::LParen)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::RParen)(input)?;
    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Integer Parser
/// ------------------------------------------------------------------
/// #### Parses integer literals.
fn parse_int(input: Tokens) -> IResult<Tokens, Expr> {
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
/// If-Else Parser
/// ------------------------------------------------------------------
/// #### Parses if-else expressions.
/// uses the `parse_expr` function to parse the condition and branches.
/// The branches are expected to be block expressions.
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

/// ------------------------------------------------------------------
/// Tag Token Parser
/// ------------------------------------------------------------------
/// #### Parses a specific token.
/// This is used to match specific tokens in the input without consuming them
/// and returning the remaining tokens.
fn tag_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| match input.split_first() {
        Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            ErrorKind::Tag,
        ))),
    }
}

/// ------------------------------------------------------------------
/// Block Expression Parser
/// ------------------------------------------------------------------
/// #### Parses block expressions.
/// This is used to group expressions inside braces `{}`.
fn parse_block_expr(input: Tokens) -> IResult<Tokens, Expr> {
    let (input, _) = tag_token(Token::LBrace)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Logical OR Parser
/// ------------------------------------------------------------------
/// #### Parses logical OR (`||`) expressions. Lowest precedence.
fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expr> {
    let (mut input, mut expr) = parse_logical_and.parse(input)?;

    loop {
        if let Some((Token::Or, rest)) = input.split_first() {
            input = rest;
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
    let (mut input, mut expr) = parse_comparison.parse(input)?;

    loop {
        if let Some((Token::And, rest)) = input.split_first() {
            input = rest;
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
    let (mut input, mut expr) = parse_add_sub.parse(input)?;

    loop {
        let (op, rest) = match input.split_first() {
            Some((Token::EqualEqual, r)) => (Some(BinOp::Equal), r),
            Some((Token::NotEqual, r)) => (Some(BinOp::NotEqual), r),
            Some((Token::Less, r)) => (Some(BinOp::Less), r),
            Some((Token::LessEqual, r)) => (Some(BinOp::LessEqual), r),
            Some((Token::Greater, r)) => (Some(BinOp::Greater), r),
            Some((Token::GreaterEqual, r)) => (Some(BinOp::GreaterEqual), r),
            _ => break,
        };

        input = rest;
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
/// Logical NOT and Unary Minus Parser
/// ------------------------------------------------------------------
/// #### Parses unary expressions like `-x` or `!x`.
/// `-` becomes UnaryOp::Neg and `!` becomes UnaryOp::Not.
fn parse_unary(input: Tokens) -> IResult<Tokens, Expr> {
    match input.split_first() {
        Some((Token::Minus, rest)) => {
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
/// String Literal Parser
/// ------------------------------------------------------------------
/// #### Parses string literals like `"hello"`
fn parse_string(input: Tokens) -> IResult<Tokens, Expr> {
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
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::token::Token;

    mod large_tests {
        use super::*;

        // ========================== Large tests ==============================
        // These tests are designed to cover multiple cases in a single test case,
        // and are meant to emulate snippets of code that would be parsed in a
        // real-world scenario.
        //
        // as opposed to other tests, these will use the 'lex' function to
        // simulate the pipeline from text to tokens to AST.
        //
        // More tests will be added as the parser is developed further.

        // Mock code test 1
        // Test for a complex expression:
        //if true{
        //    if (false || true) {
        //        1 + 2 * 3
        //    } else {
        //        4 / 2
        //    }
        //} else {
        //    5 % 2
        //}
        #[test]
        fn mock_code_test1() {
            let code = " if true {
                if (false || true) {
                    1 + 2 * 3
                } else {
                    4 / 2
                }
                } else {
                    5 % 2
                }";
            let tokens = lex(code).unwrap();
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::IfElse {
                    condition: Box::new(Expr::Bool(true)),
                    then_branch: Box::new(Expr::IfElse {
                        condition: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Bool(false)),
                            op: BinOp::Or,
                            right: Box::new(Expr::Bool(true)),
                        }),
                        then_branch: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Int(1)),
                            op: BinOp::Add,
                            right: Box::new(Expr::BinOp {
                                left: Box::new(Expr::Int(2)),
                                op: BinOp::Mul,
                                right: Box::new(Expr::Int(3)),
                            }),
                        }),
                        else_branch: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Int(4)),
                            op: BinOp::Div,
                            right: Box::new(Expr::Int(2)),
                        }),
                    }),
                    else_branch: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Int(5)),
                        op: BinOp::Mod,
                        right: Box::new(Expr::Int(2)),
                    }),
                }
            );
        }
    }

    mod boolean_literal_tests {
        use super::*;

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
    }

    mod if_else_tests {
        use super::*;

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
            let result = parse_expr(&tokens);
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
    }

    mod float_literal_tests {
        use super::*;

        // ========================== Float Literal ==========================

        // Test for a float literal (3.14)
        #[test]
        fn test_parse_float() {
            let tokens = vec![Token::Float(3.14)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(expr, Expr::Float(3.14));
        }
    }

    mod integer_literal_tests {
        use super::*;

        // ========================== Integer Literal ==========================

        // Test parsing number 42
        #[test]
        fn test_parse_int() {
            let tokens = vec![Token::Integer(42)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(expr, Expr::Int(42));
        }
    }

    mod arithmetic_tests {
        use super::*;

        // ========================== Arithmetic tests ==========================

        // Test for addition (1 + 2)
        #[test]
        fn test_parse_addition() {
            let tokens = vec![Token::Integer(1), Token::Plus, Token::Integer(2)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Add,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }

        // Test for subtraction (5 - 3)
        #[test]
        fn test_parse_subtraction() {
            let tokens = vec![Token::Integer(5), Token::Minus, Token::Integer(3)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::Int(5)),
                    op: BinOp::Sub,
                    right: Box::new(Expr::Int(3)),
                }
            );
        }

        // Test for multiplication (3 * 4)
        #[test]
        fn test_parse_multiplication() {
            let tokens = vec![Token::Integer(3), Token::Star, Token::Integer(4)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::Int(3)),
                    op: BinOp::Mul,
                    right: Box::new(Expr::Int(4)),
                }
            );
        }

        // Test for division (8 / 2)
        #[test]
        fn test_parse_division() {
            let tokens = vec![Token::Integer(8), Token::Slash, Token::Integer(2)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::Int(8)),
                    op: BinOp::Div,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }
        // Test for modulus (10 % 3)
        #[test]
        fn test_parse_modulus() {
            let tokens = vec![Token::Integer(10), Token::Percent, Token::Integer(3)];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::Int(10)),
                    op: BinOp::Mod,
                    right: Box::new(Expr::Int(3)),
                }
            );
        }
        // Test for mixed operations (1 + 2 * 3)
        #[test]
        fn test_parse_mixed_operations() {
            let tokens = vec![
                Token::Integer(1),
                Token::Plus,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
            ];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
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
        // Test for parentheses (1 + (2 * 3))
        #[test]
        fn test_parse_parentheses() {
            let tokens = vec![
                Token::Integer(1),
                Token::Plus,
                Token::LParen,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
                Token::RParen,
            ];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
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
        // Test for parentheses over precedence (1 + 2) * 3
        #[test]
        fn test_parse_parentheses_over_precedence() {
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
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
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

        // Extra test 1 for precedence (1 + 2 * 3)
        #[test]
        fn test_parse_precedence() {
            let tokens = vec![
                Token::Integer(1),
                Token::Plus,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
            ];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
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
        // Extra test 2 for precedence (1 * 2 + 3)
        #[test]
        fn test_parse_precedence_2() {
            let tokens = vec![
                Token::Integer(1),
                Token::Star,
                Token::Integer(2),
                Token::Plus,
                Token::Integer(3),
            ];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Int(1)),
                        op: BinOp::Mul,
                        right: Box::new(Expr::Int(2)),
                    }),
                    op: BinOp::Add,
                    right: Box::new(Expr::Int(3)),
                }
            );
        }

        //test for 1+1+1+1
        #[test]
        fn test_parse_multiple_additions() {
            let tokens = vec![
                Token::Integer(1),
                Token::Plus,
                Token::Integer(1),
                Token::Plus,
                Token::Integer(1),
                Token::Plus,
                Token::Integer(1),
            ];
            let result = parse_expr(&tokens);
            assert!(result.is_ok());
            let (remaining, expr) = result.unwrap();
            assert_eq!(remaining, &[]);
            assert_eq!(
                expr,
                Expr::BinOp {
                    left: Box::new(Expr::BinOp {
                        left: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Int(1)),
                            op: BinOp::Add,
                            right: Box::new(Expr::Int(1)),
                        }),
                        op: BinOp::Add,
                        right: Box::new(Expr::Int(1)),
                    }),
                    op: BinOp::Add,
                    right: Box::new(Expr::Int(1)),
                }
            );
        }
    }

    mod negative_operator_tests {
        use super::*;

        // =========================== Negative Operator ==========================

        /// Test cases for negative integer parsing (-5)
        #[test]
        fn test_parse_negative_integer() {
            let tokens = vec![Token::Minus, Token::Integer(5)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::UnaryOp {
                    op: crate::ast::UnaryOp::Neg,
                    expr: Box::new(Expr::Int(5)),
                }
            );
        }

        ///  Test cases for double negative integer parsing (-8)
        #[test]
        fn test_parse_double_negation() {
            let tokens = vec![Token::Minus, Token::Minus, Token::Integer(8)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::UnaryOp {
                    op: crate::ast::UnaryOp::Neg,
                    expr: Box::new(Expr::UnaryOp {
                        op: crate::ast::UnaryOp::Neg,
                        expr: Box::new(Expr::Int(8)),
                    })
                }
            );
        }

        /// Test case for arithmetic with negative integers
        /// (1 - -2)
        #[test]
        fn test_negative_inside_math() {
            let tokens = vec![
                Token::Integer(1),
                Token::Minus,
                Token::Minus,
                Token::Integer(2),
            ];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Sub,
                    right: Box::new(Expr::UnaryOp {
                        op: crate::ast::UnaryOp::Neg,
                        expr: Box::new(Expr::Int(2)),
                    })
                }
            );
        }

        /// Test cases for negative float parsing (-2.5)
        #[test]
        fn test_parse_negative_float() {
            let tokens = vec![Token::Minus, Token::Float(2.5)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::UnaryOp {
                    op: crate::ast::UnaryOp::Neg,
                    expr: Box::new(Expr::Float(2.5)),
                }
            );
        }

        // Test for negation of if-else expression that evaluates to int
        #[test]
        fn test_negation_of_if_else() {
            let tokens = vec![
                Token::Minus,
                Token::If,
                Token::True,
                Token::LBrace,
                Token::Integer(1),
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Integer(0),
                Token::RBrace,
            ];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::UnaryOp {
                    op: crate::ast::UnaryOp::Neg,
                    expr: Box::new(Expr::IfElse {
                        condition: Box::new(Expr::Bool(true)),
                        then_branch: Box::new(Expr::Int(1)),
                        else_branch: Box::new(Expr::Int(0)),
                    }),
                }
            );
        }
    }

    mod logical_operator_tests {
        use super::*;

        // =========================== Logical Operators ==========================

        /// Test for logical OR (true || false)
        #[test]
        fn test_parse_logical_or() {
            let tokens = vec![Token::True, Token::Or, Token::False];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Bool(true)),
                    op: BinOp::Or,
                    right: Box::new(Expr::Bool(false)),
                }
            );
        }
        /// Test for logical AND (true && false)
        #[test]
        fn test_parse_logical_and() {
            let tokens = vec![Token::True, Token::And, Token::False];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Bool(true)),
                    op: BinOp::And,
                    right: Box::new(Expr::Bool(false)),
                }
            );
        }
        /// Test for logical OR with nested expressions (true || false && true)
        #[test]
        fn test_parse_nested_logical_or() {
            let tokens = vec![
                Token::True,
                Token::Or,
                Token::False,
                Token::And,
                Token::True,
            ];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Bool(true)),
                    op: BinOp::Or,
                    right: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Bool(false)),
                        op: BinOp::And,
                        right: Box::new(Expr::Bool(true)),
                    }),
                }
            );
        }
    }

    mod comparison_operator_tests {
        use super::*;

        // =========================== Comparison Operators ==========================

        /// Test for equality (1 == 2)
        #[test]
        fn test_parse_equality() {
            let tokens = vec![Token::Integer(1), Token::EqualEqual, Token::Integer(2)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Equal,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }
        /// Test for inequality (1 != 2)
        #[test]
        fn test_parse_inequality() {
            let tokens = vec![Token::Integer(1), Token::NotEqual, Token::Integer(2)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::NotEqual,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }
        /// Test for less than (1 < 2)
        #[test]
        fn test_parse_less_than() {
            let tokens = vec![Token::Integer(1), Token::Less, Token::Integer(2)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::Less,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }
        /// Test for less than or equal to (1 <= 2)
        #[test]
        fn test_parse_less_than_or_equal() {
            let tokens = vec![Token::Integer(1), Token::LessEqual, Token::Integer(2)];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(
                result,
                Expr::BinOp {
                    left: Box::new(Expr::Int(1)),
                    op: BinOp::LessEqual,
                    right: Box::new(Expr::Int(2)),
                }
            );
        }
    }

    mod string_literal_tests {
        use super::*;

        // =========================== String Literal ==========================

        /// Test for string literal ("hello")
        #[test]
        fn test_parse_string() {
            let tokens = vec![Token::StringLiteral("hello".to_string())];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(result, Expr::String("hello".to_string()));
        }

        /// Test for string literal with escape characters ("hello\nworld")
        #[test]
        fn test_parse_string_with_escape() {
            let tokens = vec![Token::StringLiteral("hello\nworld".to_string())];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(result, Expr::String("hello\nworld".to_string()));
        }

        /// Test for string literal with quotes ("\"hello\"")
        #[test]
        fn test_parse_string_with_quotes() {
            let tokens = vec![Token::StringLiteral("\"hello\"".to_string())];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(result, Expr::String("\"hello\"".to_string()));
        }
    }
    mod identifier_tests {
        use super::*;

        // =========================== Identifier ==========================

        /// Test for identifier (x)
        #[test]
        fn test_parse_identifier() {
            let tokens = vec![Token::Identifier("x".to_string())];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(result, Expr::Variable("x".to_string()));
        }

        /// Test for identifier with underscore (foo_bar)
        #[test]
        fn test_parse_identifier_with_underscore() {
            let tokens = vec![Token::Identifier("foo_bar".to_string())];
            let result = parse_expr(&tokens).unwrap().1;
            assert_eq!(result, Expr::Variable("foo_bar".to_string()));
        }
    }
}
