use nom::{IResult, bytes::complete::take_while1, character::complete::char};

use crate::token::Token;

// Parses space and tab chunks like "    " or "\t\t"
pub fn lex_whitespace(input: &str) -> IResult<&str, Token> {
    let (rest, matched) = take_while1(|c| c == ' ' || c == '\t')(input)?;
    Ok((rest, Token::Whitespace(matched.to_string())))
}

// Parses a single newline
pub fn lex_newline(input: &str) -> IResult<&str, Token> {
    let (rest, _) = char('\n')(input)?;
    Ok((rest, Token::Newline))
}
