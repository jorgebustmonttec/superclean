use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::map_res,
    multi::many0,
    sequence::delimited,
    IResult, Parser,
};

use crate::token::Token;

/// Tokenize an integer literal
fn lex_int(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| s.parse::<i64>().map(Token::Integer)).parse(input)
}

/// Tokenize the `+` symbol
fn lex_plus(input: &str) -> IResult<&str, Token> {
    char('+').map(|_| Token::Plus).parse(input)
}

/// Tokenizer for a single token (only int or plus for now)
fn lex_token(input: &str) -> IResult<&str, Token> {
    delimited(multispace0, alt((lex_int, lex_plus)), multispace0).parse(input)
}

/// Tokenize the full input string
pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    many0(lex_token).parse(input)
        .map(|(_rest, tokens)| tokens)
        .map_err(|e| format!("Lex error: {e:?}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    fn test_lex_int() {
        assert_eq!(lex("1"), Ok(vec![Token::Integer(1)]));
        assert_eq!(lex("42"), Ok(vec![Token::Integer(42)]));
    }

    #[test]
    fn test_lex_plus() {
        assert_eq!(lex("+"), Ok(vec![Token::Plus]));
    }

    #[test]
    fn test_lex_simple_expr() {
        assert_eq!(
            lex("1 + 1"),
            Ok(vec![Token::Integer(1), Token::Plus, Token::Integer(1)])
        );
    }

    #[test]
    fn test_lex_with_spaces() {
        assert_eq!(
            lex("   3    +   9  "),
            Ok(vec![Token::Integer(3), Token::Plus, Token::Integer(9)])
        );
    }
}
