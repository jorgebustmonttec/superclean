use nom::{
    Err as NomErr, IResult, Parser, branch::alt, character::complete::multispace0,
    sequence::delimited,
};

use crate::error::LexError;
use crate::token::Token;

use crate::lexer::comments::{lex_block_comment, lex_line_comment};
use crate::lexer::delimiters::{
    lex_colon, lex_comma, lex_lbrace, lex_lparen, lex_rbrace, lex_rparen, lex_semicolon,
};
use crate::lexer::keywords::lex_identifier_or_keyword;
use crate::lexer::literals::{lex_int, lex_string};
use crate::lexer::operators::{
    lex_equal, lex_equal_equal, lex_greater, lex_greater_equal, lex_less, lex_less_equal,
    lex_minus, lex_not_equal, lex_plus, lex_slash, lex_star,
};

/// Tokenize a single token from the input string.
fn lex_token(input: &str) -> IResult<&str, Token> {
    let token_parser = alt((
        lex_line_comment,
        lex_block_comment,
        lex_string,
        lex_equal_equal,
        lex_not_equal,
        lex_less_equal,
        lex_greater_equal,
        lex_equal,
        lex_plus,
    ));

    let token_parser2 = alt((
        lex_minus,
        lex_star,
        lex_slash,
        lex_less,
        lex_greater,
        lex_identifier_or_keyword,
        lex_int,
        lex_lparen,
        lex_rparen,
        lex_lbrace,
        lex_rbrace,
        lex_comma,
        lex_semicolon,
        lex_colon,
        lex_unrecognized, // ← ADD THIS LAST
    ));

    delimited(multispace0, alt((token_parser, token_parser2)), multispace0).parse(input)
}

/// Tokenize unrecognized input
fn lex_unrecognized(_input: &str) -> IResult<&str, Token> {
    Err(nom::Err::Error(nom::error::Error::new(
        _input,
        nom::error::ErrorKind::Tag,
    )))
}

/// Helper function to find the line and column of a given position in the input string
pub fn find_line_and_column(input: &str, position: usize) -> (usize, usize) {
    let mut line = 1;
    let mut column = 1;

    for (_i, c) in input.char_indices().take(position) {
        if c == '\n' {
            line += 1;
            column = 1;
        } else {
            column += 1;
        }
    }

    (line, column)
}

// ================= Main Lexing Function =================

/// Tokenize the full input string
pub fn lex(input: &str) -> Result<Vec<Token>, LexError> {
    let mut remaining = input;
    let mut tokens = Vec::new();

    while !remaining.trim_start().is_empty() {
        let original_len = remaining.len();

        match lex_token(remaining) {
            Ok((rest, token)) => {
                // Push token and continue
                tokens.push(token);
                remaining = rest;

                // Make sure we made progress
                if rest.len() == original_len {
                    break; // prevent infinite loop
                }
            }
            Err(NomErr::Error(e)) | Err(NomErr::Failure(e)) => {
                let offset = input.len() - e.input.len();
                let (line, column) = find_line_and_column(input, offset);
                return Err(LexError::new("Unrecognized token", line, column));
            }
            Err(NomErr::Incomplete(_)) => {
                return Err(LexError::new("Incomplete input", 0, 0));
            }
        }
    }

    Ok(tokens)
}
