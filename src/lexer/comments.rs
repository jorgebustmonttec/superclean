use nom::{bytes::complete::{tag, take_until, take_while}, sequence::preceded, IResult, Parser};
use crate::token::Token;

pub fn lex_line_comment(input: &str) -> IResult<&str, Token> {
    preceded(
        tag("//"),
        take_while(|c| c != '\n'),
    )
    .map(|text: &str| Token::LineComment(text.to_string()))
    .parse(input)
}

pub fn lex_block_comment(input: &str) -> IResult<&str, Token> {
    nom::sequence::delimited(
        tag("/*"),
        take_until("*/"),
        tag("*/"),
    )
    .map(|text: &str| Token::BlockComment(text.to_string()))
    .parse(input)
}
