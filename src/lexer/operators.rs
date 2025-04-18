use crate::token::Token;
use nom::{IResult, Parser, bytes::complete::tag, character::complete::char};

/// Lexes the `'+'` character as a `Token::Plus`.
pub fn lex_plus(input: &str) -> IResult<&str, Token> {
    char('+').map(|_| Token::Plus).parse(input)
}

/// Lexes the `'-'` character as a `Token::Minus`.
pub fn lex_minus(input: &str) -> IResult<&str, Token> {
    char('-').map(|_| Token::Minus).parse(input)
}

/// Lexes the `'%'` character as a `Token::Percent`.
pub fn lex_star(input: &str) -> IResult<&str, Token> {
    char('*').map(|_| Token::Star).parse(input)
}

/// Lexes the `'/'` character as a `Token::Slash`.
pub fn lex_slash(input: &str) -> IResult<&str, Token> {
    char('/').map(|_| Token::Slash).parse(input)
}

/// Lexes the  '==' character as a `Token::EqualEqual`.
pub fn lex_equal_equal(input: &str) -> IResult<&str, Token> {
    tag("==").map(|_| Token::EqualEqual).parse(input)
}

/// Lexes the '!=' character as a `Token::NotEqual`.
pub fn lex_not_equal(input: &str) -> IResult<&str, Token> {
    tag("!=").map(|_| Token::NotEqual).parse(input)
}

/// Lexes the '<=' character as a `Token::LessEqual`.
pub fn lex_less_equal(input: &str) -> IResult<&str, Token> {
    tag("<=").map(|_| Token::LessEqual).parse(input)
}

/// Lexes the '>=' character as a `Token::GreaterEqual`.
pub fn lex_greater_equal(input: &str) -> IResult<&str, Token> {
    tag(">=").map(|_| Token::GreaterEqual).parse(input)
}

/// Lexes the '<' character as a `Token::Less`.
pub fn lex_less(input: &str) -> IResult<&str, Token> {
    char('<').map(|_| Token::Less).parse(input)
}

/// Lexes the '>' character as a `Token::Greater`.
pub fn lex_greater(input: &str) -> IResult<&str, Token> {
    char('>').map(|_| Token::Greater).parse(input)
}

/// Lexes the '=' character as a `Token::Equal`.
/// This is a single equal sign, not to be confused with `Token::EqualEqual`.
pub fn lex_equal(input: &str) -> IResult<&str, Token> {
    char('=').map(|_| Token::Equal).parse(input)
}

/// Lexes the '%' character as a `Token::Percent`.
pub fn lex_percent(input: &str) -> IResult<&str, Token> {
    char('%').map(|_| Token::Percent).parse(input)
}

/// Lexes the `':'` character as a `Token::Colon`.
pub fn lex_colon(input: &str) -> IResult<&str, Token> {
    char(':').map(|_| Token::Colon).parse(input)
}

/// Lexes the `&&` character as a `Token::And`.
pub fn lex_and(input: &str) -> IResult<&str, Token> {
    tag("&&").map(|_| Token::And).parse(input)
}
/// Lexes the `||` character as a `Token::Or`.
pub fn lex_or(input: &str) -> IResult<&str, Token> {
    tag("||").map(|_| Token::Or).parse(input)
}
/// Lexes the `!` character as a `Token::Not`.
pub fn lex_not(input: &str) -> IResult<&str, Token> {
    char('!').map(|_| Token::Not).parse(input)
}

/// Lexes the `.` character as a `Token::Dot`.
pub fn lex_dot(input: &str) -> IResult<&str, Token> {
    char('.').map(|_| Token::Dot).parse(input)
}
