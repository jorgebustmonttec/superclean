use nom::character::complete::char;
use nom::IResult;
use nom::Parser;
use crate::token::Token;

pub fn lex_lparen(input: &str) -> IResult<&str, Token> {
    char('(').map(|_| Token::LParen).parse(input)
}

pub fn lex_rparen(input: &str) -> IResult<&str, Token> {
    char(')').map(|_| Token::RParen).parse(input)
}

pub fn lex_lbrace(input: &str) -> IResult<&str, Token> {
    char('{').map(|_| Token::LBrace).parse(input)
}

pub fn lex_rbrace(input: &str) -> IResult<&str, Token> {
    char('}').map(|_| Token::RBrace).parse(input)
}

pub fn lex_comma(input: &str) -> IResult<&str, Token> {
    char(',').map(|_| Token::Comma).parse(input)
}

pub fn lex_semicolon(input: &str) -> IResult<&str, Token> {
    char(';').map(|_| Token::Semicolon).parse(input)
}

pub fn lex_colon(input: &str) -> IResult<&str, Token> {
    char(':').map(|_| Token::Colon).parse(input)
}
