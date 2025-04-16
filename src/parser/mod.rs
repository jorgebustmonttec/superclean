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

use crate::ast::Expr;
use crate::ast::Stmt;
/// Utility shared by expr/stmt
use crate::token::Token;
use nom::{IResult, error::ErrorKind};

//error stuf
use crate::error::ParserError;
use crate::position::compute_position;

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

/// --------------------------------------
/// Bloxk expression parser
/// --------------------------------------
/// Parses a block expression, which is a sequence of expressions enclosed in braces.
/// The block expression can contain multiple statements or expressions.
pub fn parse_block_expr(input: Tokens) -> IResult<Tokens, Vec<Expr>> {
    let (mut input, _) = tag_token(Token::LBrace)(input)?;
    let mut exprs = Vec::new();

    while let Some(tok) = input.first() {
        if *tok == Token::RBrace {
            break;
        }

        // Try parsing a statement first
        if let Ok((new_input, stmt)) = crate::parser::stmt::parse_stmt(input) {
            exprs.push(Expr::StmtExpr(Box::new(stmt)));
            input = new_input;
        } else if let Ok((new_input, expr)) = parse_expr(input) {
            // If statement parsing fails, try parsing an expression
            exprs.push(expr);
            input = new_input;
        } else {
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
    }

    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, exprs))
}

/// ------------------------------------------------------------------
/// Block Statement Parser
/// ------------------------------------------------------------------
/// #### Parses a block of statements enclosed in `{}`
/// Returns a `Vec<Stmt>`.
pub fn parse_block_stmt(input: Tokens) -> IResult<Tokens, Vec<Stmt>> {
    println!("[parse_block_stmt] Parsing block statement...");
    println!("[parse_block_stmt] Current token: {:?}", input.first());
    let (mut input, _) = tag_token(Token::LBrace)(input)?;
    let mut stmts = Vec::new();

    while let Some(tok) = input.first() {
        input = skip_ignored(input);
        println!("[parse_block_stmt[while] Current token: {:?}", tok);

        if *tok == Token::RBrace {
            println!("[parse_block_stmt] Found RBrace, breaking out of loop.");
            break; // Stop parsing when we encounter RBrace
        }

        let (new_input, stmt) = parse_stmt(input)?;
        println!("[parse_block_stmt] Parsed statement: {:?}", stmt);
        println!("[parse_block_stmt] Remaining tokens: {:?}", new_input);
        stmts.push(stmt);
        input = new_input;
        input = skip_ignored(input);
    }

    println!("[parse_block_stmt] Finished parsing block statement.");
    println!("[parse_block_stmt] Remaining tokens: {:?}", input);
    println!("[parse_block_stmt] Parsed statements: {:?}", stmts);
    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, stmts))
}

/// ------------------------------------------------------
/// Top-level entry point for parsing a program or file.
/// ------------------------------------------------------
pub fn parse(tokens: &[Token]) -> Result<Vec<Stmt>, ParserError> {
    let mut input = tokens;
    let mut stmts = Vec::new();

    while !input.is_empty() {
        let token_index = tokens.len() - input.len();
        match crate::parser::parse_stmt(input) {
            Ok((rest, stmt)) => {
                stmts.push(stmt);
                input = rest;
            }
            Err(_) => {
                let (line, column) = compute_position(tokens, token_index);
                let unexpected = tokens.get(token_index);
                let msg = match unexpected {
                    Some(tok) => format!("Unexpected token: {:?}", tok),
                    None => "Unexpected end of input".to_string(),
                };
                return Err(ParserError::new(msg, line, column));
            }
        }
    }

    Ok(stmts)
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
                then_branch: vec![Expr::IfElse {
                    condition: Box::new(Expr::BinOp {
                        left: Box::new(Expr::Bool(false)),
                        op: BinOp::Or,
                        right: Box::new(Expr::Bool(true)),
                    }),
                    then_branch: vec![Expr::BinOp {
                        left: Box::new(Expr::Int(1)),
                        op: BinOp::Add,
                        right: Box::new(Expr::BinOp {
                            left: Box::new(Expr::Int(2)),
                            op: BinOp::Mul,
                            right: Box::new(Expr::Int(3)),
                        }),
                    }],
                    else_branch: Some(vec![Expr::BinOp {
                        left: Box::new(Expr::Int(4)),
                        op: BinOp::Div,
                        right: Box::new(Expr::Int(2)),
                    }]),
                }],
                else_branch: Some(vec![Expr::BinOp {
                    left: Box::new(Expr::Int(5)),
                    op: BinOp::Mod,
                    right: Box::new(Expr::Int(2)),
                }]),
            }
        );
    }

    #[test]
    fn mock_code_test2() {
        let code = "
        let x: Int = 5;
        let y = 10;
        let z = x + y;
        let result = z * 2;";
        let tokens = lex(code).unwrap();
        let result = parse(&tokens);
        assert!(result.is_ok());
        let stmts = result.unwrap();
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
