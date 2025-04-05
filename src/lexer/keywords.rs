use nom::{character::complete::{alpha1, alphanumeric0}, IResult, Parser};
use crate::token::Token;

pub fn lex_identifier_or_keyword(input: &str) -> IResult<&str, Token> {
    let ident_parser = (alpha1, alphanumeric0)
        .map(|(first, rest): (&str, &str)| format!("{first}{rest}"));

    ident_parser
        .map(|word| match word.as_str() {
            "let" => Token::Let,
            "fun" => Token::Fun,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "print" => Token::Print,
            "true" => Token::True,
            "false" => Token::False,
            "Int" => Token::IntType,
            "Bool" => Token::BoolType,
            "String" => Token::StringType,
            "Unit" => Token::UnitType,
            _ => Token::Identifier(word),
        })
        .parse(input)
}
