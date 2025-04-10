use crate::token::Token;
use nom::{
    IResult, Parser,
    character::complete::{alpha1, alphanumeric0},
};

/// Lexes an identifier or keyword from the input.
///
/// If the matched string corresponds to a reserved keyword (like `let`, `fun`, or `if`),
/// the appropriate token is returned. Otherwise, the string is returned as an `Identifier`.
///
/// # Example
/// - `"let"` → `Token::Let`
/// - `"foo123"` → `Token::Identifier("foo123".to_string())``
pub fn lex_identifier_or_keyword(input: &str) -> IResult<&str, Token> {
    let ident_parser =
        (alpha1, alphanumeric0).map(|(first, rest): (&str, &str)| format!("{first}{rest}"));

    ident_parser
        .map(|word| match word.as_str() {
            "let" => Token::Let,
            "var" => Token::Var,
            "const" => Token::Const,
            "fun" => Token::Fun,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "print" => Token::Print,
            "true" => Token::True,
            "false" => Token::False,
            "Int" => Token::IntType,
            "Float" => Token::FloatType,
            "Bool" => Token::BoolType,
            "String" => Token::StringType,
            "Unit" => Token::UnitType,
            "break" => Token::Break,
            "for" => Token::For,
            "in" => Token::In,
            _ => Token::Identifier(word),
        })
        .parse(input)
}
