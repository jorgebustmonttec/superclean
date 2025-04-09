// Directory structure you should aim for:
//
// src/
// ├── parser/
// │   ├── mod.rs
// │   ├── expr.rs
// │   └── stmt.rs
// └── ast.rs

// In src/parser/mod.rs:

pub mod expr;
pub mod stmt;

pub use expr::parse_expr;
pub use stmt::parse_stmt;

use crate::ast::Stmt;
/// Utility shared by expr/stmt
use crate::token::Token;
use nom::{IResult, error::ErrorKind};

type Tokens<'a> = &'a [Token];

/// Skips over any whitespace, newlines, or comments.
/// Returns the remaining slice after skipping them.
pub fn skip_ignored(mut input: Tokens) -> Tokens {
    while let Some(tok) = input.first() {
        match tok {
            Token::Whitespace(_)
            | Token::Newline
            | Token::LineComment(_)
            | Token::BlockComment(_) => {
                input = &input[1..];
            }
            _ => break,
        }
    }
    input
}

pub fn tag_token(expected: Token) -> impl Fn(Tokens) -> IResult<Tokens, Token> {
    move |input: Tokens| {
        let input = skip_ignored(input);
        match input.split_first() {
            Some((tok, rest)) if *tok == expected => Ok((rest, tok.clone())),
            _ => Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            ))),
        }
    }
}

pub fn parse_block_expr(input: Tokens) -> IResult<Tokens, crate::ast::Expr> {
    let (input, _) = tag_token(Token::LBrace)(input)?;
    let (input, expr) = parse_expr(input)?;
    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, expr))
}

/// ------------------------------------------------------
/// Top-level entry point for parsing a program or file.
/// ------------------------------------------------------
pub fn parse(tokens: Tokens) -> IResult<Tokens, Vec<Stmt>> {
    let mut input = tokens;
    let mut stmts = Vec::new();

    while !input.is_empty() {
        input = skip_ignored(input); // ← Skip comments/whitespace/newlines

        if input.is_empty() {
            break;
        }

        println!("\n[parse] Input before stmt parse: {:#?}", input);
        let (rest, stmt) = parse_stmt(input)?;
        println!("[parse] Parsed statement: {:#?}", stmt);

        stmts.push(stmt);
        input = skip_ignored(rest); // ← Skip again after parsing a stmt
    }

    Ok((input, stmts))
}

// ========================= Tests =========================

#[cfg(test)]
mod parser_tests {
    use super::*;
    use crate::ast::BinOp;
    use crate::ast::Expr;
    use crate::lexer::lex;
    //use crate::token::Token;

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
                    else_branch: Some(Box::new(Expr::BinOp {
                        left: Box::new(Expr::Int(4)),
                        op: BinOp::Div,
                        right: Box::new(Expr::Int(2)),
                    })),
                }),
                else_branch: Some(Box::new(Expr::BinOp {
                    left: Box::new(Expr::Int(5)),
                    op: BinOp::Mod,
                    right: Box::new(Expr::Int(2)),
                })),
            }
        );
    }

    #[test]
    fn mock_code_test2() {
        let code = "
        let x: Int = 5;

        let y = 10;

        let z = x + y;
        let result = z * 2;
        ";
        let tokens = lex(code).unwrap();
        let result = parse(&tokens);
        assert!(result.is_ok());
        let (remaining, stmts) = result.unwrap();
        assert_eq!(remaining, &[]);
        assert_eq!(
            stmts,
            vec![
                Stmt::Let {
                    name: "x".to_string(),
                    ty: Some(crate::ast::Type::Int),
                    expr: Expr::Int(5),
                },
                Stmt::Let {
                    name: "y".to_string(),
                    ty: None,
                    expr: Expr::Int(10),
                },
                Stmt::Let {
                    name: "z".to_string(),
                    ty: None,
                    expr: Expr::BinOp {
                        left: Box::new(Expr::Variable("x".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Variable("y".to_string())),
                    },
                },
                Stmt::Let {
                    name: "result".to_string(),
                    ty: None,
                    expr: Expr::BinOp {
                        left: Box::new(Expr::Variable("z".to_string())),
                        op: BinOp::Mul,
                        right: Box::new(Expr::Int(2)),
                    },
                },
            ]
        );
    }
}
