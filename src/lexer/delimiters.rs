use crate::token::Token;
use nom::IResult;
use nom::Parser;
use nom::character::complete::char;

/// Lexes the `'('` character as a `Token::LParen`.
pub fn lex_lparen(input: &str) -> IResult<&str, Token> {
    char('(').map(|_| Token::LParen).parse(input)
}

/// Lexes the `')'` character as a `Token::RParen`.
pub fn lex_rparen(input: &str) -> IResult<&str, Token> {
    char(')').map(|_| Token::RParen).parse(input)
}

/// Lexes the `'{'` character as a `Token::LBrace`.
pub fn lex_lbrace(input: &str) -> IResult<&str, Token> {
    char('{').map(|_| Token::LBrace).parse(input)
}

/// Lexes the `'}'` character as a `Token::RBrace`.
pub fn lex_rbrace(input: &str) -> IResult<&str, Token> {
    char('}').map(|_| Token::RBrace).parse(input)
}

/// Lexes the `','` character as a `Token::Comma`.
pub fn lex_comma(input: &str) -> IResult<&str, Token> {
    char(',').map(|_| Token::Comma).parse(input)
}

/// Lexes the `';'` character as a `Token::Semicolon`.
pub fn lex_semicolon(input: &str) -> IResult<&str, Token> {
    char(';').map(|_| Token::Semicolon).parse(input)
}

/// Lexes the `':'` character as a `Token::Colon`.
pub fn lex_colon(input: &str) -> IResult<&str, Token> {
    char(':').map(|_| Token::Colon).parse(input)
}
