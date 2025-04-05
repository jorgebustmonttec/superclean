use crate::token::Token;
use nom::{
    IResult, Parser,
    bytes::complete::{tag, take_until, take_while},
    sequence::preceded,
};

/// Lexes a line comment starting with `//`.
///
/// Consumes all characters until the end of the line and wraps it
/// as a `Token::LineComment`.
pub fn lex_line_comment(input: &str) -> IResult<&str, Token> {
    preceded(tag("//"), take_while(|c| c != '\n'))
        .map(|text: &str| Token::LineComment(text.to_string()))
        .parse(input)
}

/// Lexes a block comment delimited by `/* ... */`.
///
/// Consumes everything between the opening and closing delimiters.
/// Returns a `Token::BlockComment` with the raw content inside.
pub fn lex_block_comment(input: &str) -> IResult<&str, Token> {
    nom::sequence::delimited(tag("/*"), take_until("*/"), tag("*/"))
        .map(|text: &str| Token::BlockComment(text.to_string()))
        .parse(input)
}
