use crate::token::Token;
use nom::{
    IResult, Parser,
    character::complete::{char, digit1},
    combinator::{map_res, recognize},
};

/// Lexes an integer literal composed of digits (`0-9`).
///
/// Parses the digit sequence into an `i64` and wraps it in a `Token::Integer`.
pub fn lex_int(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| s.parse::<i64>().map(Token::Integer)).parse(input)
}

/// Lexes a float literal like `3.14` or `10.0`
///
/// Parses the digit sequence before and after the decimal point into a `f64`
/// and wraps it in a `Token::Float`.
/// The decimal point is mandatory, and the function will fail if the input
pub fn lex_float(input: &str) -> IResult<&str, Token> {
    recognize((digit1, char('.'), digit1))
        .map_res(|s: &str| s.parse::<f64>().map(Token::Float))
        .parse(input)
}

/// Lexes a string literal surrounded by double quotes.
///
/// Supports common escape sequences such as `\\`, `\"`, `\n`, and `\t`.
/// Returns a `Token::StringLiteral` with the interpreted contents.
///
/// Returns an error if the string is unterminated.
pub fn lex_string(input: &str) -> IResult<&str, Token> {
    println!("===> TRYING TO PARSE STRING from: {input:?}");

    let mut escaped = false;
    let mut result = String::new();
    let mut chars = input.chars().enumerate();

    let Some((_, '"')) = chars.next() else {
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Char,
        )));
    };

    while let Some((j, c)) = chars.next() {
        if escaped {
            let unescaped = match c {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                other => {
                    result.push('\\');
                    result.push(other);
                    escaped = false;
                    continue;
                }
            };
            result.push(unescaped);
            escaped = false;
        } else if c == '\\' {
            escaped = true;
        } else if c == '"' {
            let rest = &input[j + 1..];
            println!("===> STRING SUCCESSFULLY PARSED: {result:?}");
            return Ok((rest, Token::StringLiteral(result)));
        } else {
            result.push(c);
        }
    }

    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Char,
    )))
}
