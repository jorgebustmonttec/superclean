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
    println!("[DEBUG] parse_expr: current token: {:?}", input.first());
    parse_logical_or.parse(input)
}

/// ------------------------------------------------------------------
/// Logical OR Parser
/// ------------------------------------------------------------------
/// #### Parses logical OR (`||`) expressions. Lowest precedence.
fn parse_logical_or(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_logical_or: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_logical_or after skip_ignored: {:?}",
        input.first()
    );
    let (mut input, mut expr) = parse_logical_and.parse(input)?;
    println!(
        "[DEBUG] parse_logical_or after parse_logical_and: {:?}",
        input.first()
    );

    loop {
        println!("[DEBUG] parse_logical_or loop: {:?}", input.first());
        input = skip_ignored(input);
        if let Some((Token::Or, rest)) = input.split_first() {
            println!("[DEBUG] Found Token::Or, current token: {:?}", rest.first());
            input = skip_ignored(rest);
            let (new_input, rhs) = parse_logical_and.parse(input)?;
            input = new_input;
            expr = Expr::BinOp {
                left: Box::new(expr),
                op: BinOp::Or,
                right: Box::new(rhs),
            };
            println!(
                "[DEBUG] Updated expr in parse_logical_or: {:?}",
                input.first()
            );
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
    println!("[DEBUG] Entering parse_logical_and: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_logical_and after skip_ignored: {:?}",
        input.first()
    );
    let (mut input, mut expr) = parse_comparison.parse(input)?;
    println!(
        "[DEBUG] parse_logical_and after parse_comparison: {:?}",
        input.first()
    );

    loop {
        println!("[DEBUG] parse_logical_and loop: {:?}", input.first());
        input = skip_ignored(input);
        if let Some((Token::And, rest)) = input.split_first() {
            println!(
                "[DEBUG] Found Token::And, current token: {:?}",
                rest.first()
            );
            input = skip_ignored(rest);
            let (new_input, rhs) = parse_comparison.parse(input)?;
            input = new_input;
            expr = Expr::BinOp {
                left: Box::new(expr),
                op: BinOp::And,
                right: Box::new(rhs),
            };
            println!(
                "[DEBUG] Updated expr in parse_logical_and: {:?}",
                input.first()
            );
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
    println!("[DEBUG] Entering parse_comparison: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_comparison after skip_ignored: {:?}",
        input.first()
    );
    let (mut input, mut expr) = parse_add_sub.parse(input)?;
    println!(
        "[DEBUG] parse_comparison after parse_add_sub: {:?}",
        input.first()
    );

    loop {
        println!("[DEBUG] parse_comparison loop: {:?}", input.first());
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

        println!(
            "[DEBUG] Found comparison operator, current token after operator: {:?}",
            rest.first()
        );
        input = skip_ignored(rest);
        let (new_input, rhs) = parse_add_sub.parse(input)?;
        input = new_input;
        expr = Expr::BinOp {
            left: Box::new(expr),
            op: op.unwrap(),
            right: Box::new(rhs),
        };
        println!(
            "[DEBUG] Updated expr in parse_comparison: {:?}",
            input.first()
        );
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Addition and Subtraction Parser
/// ------------------------------------------------------------------
/// #### Parses addition and subtraction expressions. Has low precedence
/// and is chained with multiplication and division (which has higher precedence).
fn parse_add_sub(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_add_sub: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_add_sub after skip_ignored: {:?}",
        input.first()
    );
    let (mut input, mut expr) = parse_mul_div_mod.parse(input)?;
    println!(
        "[DEBUG] parse_add_sub after parse_mul_div_mod: {:?}",
        input.first()
    );

    loop {
        println!("[DEBUG] parse_add_sub loop: {:?}", input.first());
        input = skip_ignored(input);
        let op = if let Some((Token::Plus, rest)) = input.split_first() {
            println!(
                "[DEBUG] Found Token::Plus, current token: {:?}",
                rest.first()
            );
            input = rest;
            Some(BinOp::Add)
        } else if let Some((Token::Minus, rest)) = input.split_first() {
            println!(
                "[DEBUG] Found Token::Minus, current token: {:?}",
                rest.first()
            );
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
        println!("[DEBUG] Updated expr in parse_add_sub: {:?}", input.first());
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Multiplication / Division / Modulo Parser
/// ------------------------------------------------------------------
/// #### Parses multiplication, division, and modulo operations. Has medium precedence.
fn parse_mul_div_mod(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_mul_div_mod: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_mul_div_mod after skip_ignored: {:?}",
        input.first()
    );
    let (mut input, mut expr) = parse_unary.parse(input)?;
    println!(
        "[DEBUG] parse_mul_div_mod after parse_unary: {:?}",
        input.first()
    );

    loop {
        println!("[DEBUG] parse_mul_div_mod loop: {:?}", input.first());
        input = skip_ignored(input);
        let op = match input.split_first() {
            Some((Token::Star, rest)) => {
                println!(
                    "[DEBUG] Found Token::Star, current token: {:?}",
                    rest.first()
                );
                input = rest;
                Some(BinOp::Mul)
            }
            Some((Token::Slash, rest)) => {
                println!(
                    "[DEBUG] Found Token::Slash, current token: {:?}",
                    rest.first()
                );
                input = rest;
                Some(BinOp::Div)
            }
            Some((Token::Percent, rest)) => {
                println!(
                    "[DEBUG] Found Token::Percent, current token: {:?}",
                    rest.first()
                );
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
        println!(
            "[DEBUG] Updated expr in parse_mul_div_mod: {:?}",
            input.first()
        );
    }

    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// Logical NOT and Unary Minus Parser
/// ------------------------------------------------------------------
/// #### Parses unary expressions like `-x` or `!x`.
/// `-` becomes UnaryOp::Neg and `!` becomes UnaryOp::Not.
fn parse_unary(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_unary: {:?}", input.first());
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_unary after skip_ignored: {:?}",
        input.first()
    );
    match input.split_first() {
        Some((Token::Minus, rest)) => {
            println!(
                "[DEBUG] Found Token::Minus in parse_unary, current token: {:?}",
                rest.first()
            );
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
            println!(
                "[DEBUG] Found Token::Not in parse_unary, current token: {:?}",
                rest.first()
            );
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
        _ => {
            println!("[DEBUG] Falling back to parse_primary in parse_unary");
            parse_primary(input)
        }
    }
}

/// ------------------------------------------------------------------
/// Primary Expression Parser
/// ------------------------------------------------------------------
/// #### Parses primary expressions. This includes literals and parenthesized expressions.
fn parse_primary(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_primary: {:?}", input.first());
    alt((
        parse_parens,
        parse_if_else,
        parse_int,
        parse_float,
        parse_bool,
        parse_string,
        parse_call_expr,
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
    println!("[DEBUG] Entering parse_parens: {:?}", input.first());
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::LParen)(input)?;
    println!(
        "[DEBUG] parse_parens found LParen, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);
    let (input, expr) = parse_expr(input)?;
    println!(
        "[DEBUG] parse_parens parsed inner expr, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::RParen)(input)?;
    println!(
        "[DEBUG] parse_parens found RParen, current token: {:?}",
        input.first()
    );
    Ok((input, expr))
}

/// ------------------------------------------------------------------
/// If-Else Parser
/// ------------------------------------------------------------------
/// #### Parses if-else expressions.
/// uses the `parse_expr` function to parse the condition and branches.
/// The branches are expected to be block expressions.
fn parse_if_else(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_if_else: {:?}", input.first());
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::If)(input)?;
    println!(
        "[DEBUG] parse_if_else found If, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);
    let (input, condition) = parse_expr(input)?;
    println!(
        "[DEBUG] parse_if_else parsed condition, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);
    let (input, then_branch) = parse_block_expr(input)?;
    println!(
        "[DEBUG] parse_if_else parsed then_branch, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);

    let (input, else_branch) = if let Some((Token::Else, rest)) = input.split_first() {
        println!(
            "[DEBUG] parse_if_else found Else, current token: {:?}",
            rest.first()
        );
        let input = skip_ignored(&rest);
        let (input, block) = parse_block_expr(input)?;
        (input, Some(block))
    } else {
        (input, None)
    };

    Ok((
        input,
        Expr::IfElse {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        },
    ))
}

/// ------------------------------------------------------------------
/// Integer Parser
/// ------------------------------------------------------------------
/// #### Parses integer literals.
fn parse_int(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_int: {:?}", input.first());
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Integer(n), rest)) => {
            println!(
                "[DEBUG] parse_int found Integer {} current token: {:?}",
                n,
                rest.first()
            );
            Ok((rest, Expr::Int(*n)))
        }
        _ => {
            println!("[DEBUG] parse_int error: token not an Integer");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

/// ------------------------------------------------------------------
/// Float Parser
/// ------------------------------------------------------------------
/// #### Parses float literals.
fn parse_float(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_float: {:?}", input.first());
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Float(f), rest)) => {
            println!(
                "[DEBUG] parse_float found Float {} current token: {:?}",
                f,
                rest.first()
            );
            Ok((rest, Expr::Float(*f)))
        }
        _ => {
            println!("[DEBUG] parse_float error: token not a Float");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

/// ------------------------------------------------------------------
/// Boolean Parser
/// ------------------------------------------------------------------
/// #### Parses boolean literals.
fn parse_bool(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_bool: {:?}", input.first());
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::True, rest)) => {
            println!(
                "[DEBUG] parse_bool found True, current token: {:?}",
                rest.first()
            );
            Ok((rest, Expr::Bool(true)))
        }
        Some((Token::False, rest)) => {
            println!(
                "[DEBUG] parse_bool found False, current token: {:?}",
                rest.first()
            );
            Ok((rest, Expr::Bool(false)))
        }
        _ => {
            println!("[DEBUG] parse_bool error: token not a boolean");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

/// ------------------------------------------------------------------
/// String Literal Parser
/// ------------------------------------------------------------------
/// #### Parses string literals like `"hello"`
fn parse_string(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_string: {:?}", input.first());
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::StringLiteral(s), rest)) => {
            println!(
                "[DEBUG] parse_string found StringLiteral {} current token: {:?}",
                s,
                rest.first()
            );
            Ok((rest, Expr::String(s.clone())))
        }
        _ => {
            println!("[DEBUG] parse_string error: token not a StringLiteral");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

/// ------------------------------------------------------------------
/// Identifier / Variable Parser
/// ------------------------------------------------------------------
/// #### Parses bare identifiers like `x`, `foo`, etc.
fn parse_identifier(input: Tokens) -> IResult<Tokens, Expr> {
    println!("[DEBUG] Entering parse_identifier: {:?}", input.first());
    let input = skip_ignored(input);
    match input.split_first() {
        Some((Token::Identifier(name), rest)) => {
            println!(
                "[DEBUG] parse_identifier found Identifier {} current token: {:?}",
                name,
                rest.first()
            );
            Ok((rest, Expr::Variable(name.clone())))
        }
        _ => {
            println!("[DEBUG] parse_identifier error: token not an Identifier");
            Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )))
        }
    }
}

/// ------------------------------------------------------------------
/// Function Call Parser
/// ------------------------------------------------------------------
/// #### Parses function calls like `add(5, 3)`
fn parse_call_expr(input: Tokens) -> IResult<Tokens, Expr> {
    println!(
        "[DEBUG] Entering parse_call_expr: current token: {:?}",
        input.first()
    );
    // Parse function name
    let (input, function) = parse_identifier(input)?;
    println!(
        "[DEBUG] parse_call_expr parsed function name: {:?}",
        function
    );
    let input = skip_ignored(input);
    let (input, _) = tag_token(Token::LParen)(input)?;
    println!(
        "[DEBUG] parse_call_expr found LParen, current token: {:?}",
        input.first()
    );
    let input = skip_ignored(input);
    println!(
        "[DEBUG] parse_call_expr after LParen, current token: {:?}",
        input.first()
    );
    let mut args = Vec::new();
    let mut input = skip_ignored(input);

    while let Some(tok) = input.first() {
        println!("[DEBUG] parse_call_expr loop, current token: {:?}", tok);
        if *tok == Token::RParen {
            println!("[DEBUG] parse_call_expr reached RParen");
            break;
        }

        let (new_input, arg) = parse_expr(input)?;
        args.push(arg);
        input = skip_ignored(new_input);

        // Check for comma or closing parenthesis
        if let Some((Token::Comma, rest)) = input.split_first() {
            println!(
                "[DEBUG] parse_call_expr found Comma, current token: {:?}",
                rest.first()
            );
            input = skip_ignored(rest);
        } else {
            break;
        }
    }

    let (input, _) = tag_token(Token::RParen)(input)?;
    println!(
        "[DEBUG] parse_call_expr completed, current token: {:?}",
        input.first()
    );

    Ok((
        input,
        Expr::Call {
            function: Box::new(function),
            args,
        },
    ))
}

// ======================== Tests =========================

#[cfg(test)]
mod expr_tests {
    use super::*;
    use crate::lexer::lex;
    //use crate::token::Token;

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
                    then_branch: vec![Expr::Int(1)],
                    else_branch: Some(vec![Expr::Int(2)]),
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
                    then_branch: vec![Expr::Int(42)],
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
