use nom::{character::complete::char, bytes::complete::tag, IResult, Parser};
use crate::token::Token;

pub fn lex_plus(input: &str) -> IResult<&str, Token> {
    char('+').map(|_| Token::Plus).parse(input)
}

pub fn lex_minus(input: &str) -> IResult<&str, Token> {
    char('-').map(|_| Token::Minus).parse(input)
}

pub fn lex_star(input: &str) -> IResult<&str, Token> {
    char('*').map(|_| Token::Star).parse(input)
}

pub fn lex_slash(input: &str) -> IResult<&str, Token> {
    char('/').map(|_| Token::Slash).parse(input)
}

pub fn lex_equal_equal(input: &str) -> IResult<&str, Token> {
    tag("==").map(|_| Token::EqualEqual).parse(input)
}

pub fn lex_not_equal(input: &str) -> IResult<&str, Token> {
    tag("!=").map(|_| Token::NotEqual).parse(input)
}

pub fn lex_less_equal(input: &str) -> IResult<&str, Token> {
    tag("<=").map(|_| Token::LessEqual).parse(input)
}

pub fn lex_greater_equal(input: &str) -> IResult<&str, Token> {
    tag(">=").map(|_| Token::GreaterEqual).parse(input)
}

pub fn lex_less(input: &str) -> IResult<&str, Token> {
    char('<').map(|_| Token::Less).parse(input)
}

pub fn lex_greater(input: &str) -> IResult<&str, Token> {
    char('>').map(|_| Token::Greater).parse(input)
}

pub fn lex_equal(input: &str) -> IResult<&str, Token> {
    char('=').map(|_| Token::Equal).parse(input)
}
