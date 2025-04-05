use nom::{character::complete::digit1, combinator::map_res, IResult, Parser};
use crate::token::Token;

pub fn lex_int(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| s.parse::<i64>().map(Token::Integer)).parse(input)
}

pub fn lex_string(input: &str) -> IResult<&str, Token> {
    println!("===> TRYING TO PARSE STRING from: {input:?}");

    let mut escaped = false;
    let mut result = String::new();
    let mut chars = input.chars().enumerate();

    let Some((mut i, '"')) = chars.next() else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char)));
    };

    while let Some((j, c)) = chars.next() {
        i = j;
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
            let rest = &input[i + 1..];
            println!("===> STRING SUCCESSFULLY PARSED: {result:?}");
            return Ok((rest, Token::StringLiteral(result)));
        } else {
            result.push(c);
        }
    }

    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char)))
}
