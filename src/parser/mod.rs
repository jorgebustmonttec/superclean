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

use expr::parse_expr;
use stmt::parse_stmt;

use crate::ast::Stmt;
use crate::token::Token;
use nom::IResult;

type Tokens<'a> = &'a [Token];

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

/// ------------------------------------------------------
/// Top-level entry point for parsing a program or file.
/// ------------------------------------------------------
pub fn parse(tokens: Tokens) -> IResult<Tokens, Vec<Stmt>> {
    let mut input = tokens;
    let mut stmts = Vec::new();

    while !input.is_empty() {
        let (rest, stmt) = parse_stmt(input)?;
        stmts.push(stmt);
        input = rest;
    }

    Ok((input, stmts))
}
