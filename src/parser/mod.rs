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
    println!("[parse_block_expr] Parsing block expression...");
    let (mut input, _) = tag_token(Token::LBrace)(input)?;
    let mut exprs = Vec::new();

    while let Some(tok) = input.first() {
        if *tok == Token::RBrace {
            break;
        }

        // Try parsing a statement first
        if let Ok((new_input, stmt)) = crate::parser::stmt::parse_stmt(input) {
            println!("[parse_block_expr] Parsed statement: {:?}", stmt);
            exprs.push(Expr::StmtExpr(Box::new(stmt)));
            input = new_input;
        } else if let Ok((new_input, expr)) = parse_expr(input) {
            println!("[parse_block_expr] Parsed expression: {:?}", expr);
            // If statement parsing fails, try parsing an expression
            exprs.push(expr);
            input = new_input;
        } else {
            println!("[parse_block_expr] Failed to parse statement or expression.");
            return Err(nom::Err::Error(nom::error::Error::new(
                input,
                ErrorKind::Tag,
            )));
        }
        input = skip_ignored(input);
    }

    println!("[parse_block_expr] Finished parsing block expression.");
    let (input, _) = tag_token(Token::RBrace)(input)?;
    Ok((input, exprs))
}

/// ------------------------------------------------------------------
/// Block Statement Parser
/// ------------------------------------------------------------------
/// #### Parses a block of statements enclosed in `{}`
/// Returns a `Vec<Stmt>`.
pub fn parse_block_stmt(input: Tokens) -> IResult<Tokens, Vec<Stmt>> {
    //println!("[parse_block_stmt] Parsing block statement...");
    //println!("[parse_block_stmt] Current token: {:?}", input.first());
    let (mut input, _) = tag_token(Token::LBrace)(input)?;
    let mut stmts = Vec::new();

    while let Some(tok) = input.first() {
        input = skip_ignored(input);

        if *tok == Token::RBrace {
            break; // Stop parsing when we encounter RBrace
        }

        let (new_input, stmt) = parse_stmt(input)?;

        stmts.push(stmt);
        input = new_input;
        input = skip_ignored(input);
    }

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
    use crate::ast;
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

    #[test]
    fn mock_code_complex_function() {
        let code = "
fun add(a: Int, b: Int) :Int {
    return a + b;
}

fun main() {
    let result = add(5, 10);
    print(\"the result is: \" + result);
    if result > 10 {
        print(\"the result is greater than 10\");
    };
}";
        let tokens = lex(code).unwrap();
        let result = parse(&tokens);
        println!("{:?}", result);
        assert!(result.is_ok());
        let stmts = result.unwrap();
        assert_eq!(stmts.len(), 2); // Two top-level functions
        assert_eq!(
            stmts,
            vec![
                Stmt::Fun {
                    name: "add".to_string(),
                    params: vec![
                        ("a".to_string(), crate::ast::Type::Int),
                        ("b".to_string(), crate::ast::Type::Int)
                    ],
                    return_type: crate::ast::Type::Int,
                    body: vec![Stmt::Return(Some(Expr::BinOp {
                        left: Box::new(Expr::Variable("a".to_string())),
                        op: BinOp::Add,
                        right: Box::new(Expr::Variable("b".to_string())),
                    })),],
                },
                Stmt::Fun {
                    name: "main".to_string(),
                    params: vec![],
                    return_type: ast::Type::Unit,
                    body: vec![
                        Stmt::Let {
                            name: "result".to_string(),
                            ty: None,
                            expr: Expr::Call {
                                function: Box::new(Expr::Variable("add".to_string())),
                                args: vec![Expr::Int(5), Expr::Int(10)],
                            },
                        },
                        Stmt::Print(Expr::BinOp {
                            left: Box::new(Expr::String("the result is: ".to_string())),
                            op: BinOp::Add,
                            right: Box::new(Expr::Variable("result".to_string())),
                        }),
                        Stmt::Expr(Expr::IfElse {
                            condition: Box::new(Expr::BinOp {
                                left: Box::new(Expr::Variable("result".to_string())),
                                op: BinOp::Greater,
                                right: Box::new(Expr::Int(10)),
                            }),
                            then_branch: vec![Expr::StmtExpr(Box::new(Stmt::Print(Expr::String(
                                "the result is greater than 10".to_string()
                            ),)))],
                            else_branch: None,
                        }),
                    ],
                },
            ]
        );
    }
}
