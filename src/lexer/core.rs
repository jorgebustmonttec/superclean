use nom::{
    Err as NomErr, IResult, Parser, branch::alt, character::complete::multispace0,
    sequence::delimited,
};

use crate::error::LexError;
use crate::token::Token;

use crate::lexer::comments::{lex_block_comment, lex_line_comment};
use crate::lexer::delimiters::{
    lex_comma, lex_lbrace, lex_lbracket, lex_lparen, lex_rbrace, lex_rbracket, lex_rparen,
    lex_semicolon,
};
use crate::lexer::keywords::lex_identifier_or_keyword;
use crate::lexer::literals::{lex_float, lex_int, lex_string};
use crate::lexer::operators::{
    lex_and, lex_colon, lex_equal, lex_equal_equal, lex_greater, lex_greater_equal, lex_less,
    lex_less_equal, lex_minus, lex_not, lex_not_equal, lex_or, lex_percent, lex_plus, lex_slash,
    lex_star,
};

/// Tries to parse a single token from the beginning of the input string.
///
/// This function combines all available token parsers (comments, strings, operators,
/// delimiters, keywords, etc.) using `alt` and `delimited` to skip surrounding whitespace.
///
/// It returns the matched token along with the remaining unparsed input.
///
/// # Arguments
/// - `input`: The input string to parse from.
///
/// # Returns
/// - `IResult<&str, Token>`: A nom-style result containing the remaining input and the parsed token.
fn lex_token(input: &str) -> IResult<&str, Token> {
    let token_parser = alt((
        lex_line_comment,
        lex_block_comment,
        lex_string,
        lex_equal_equal,
        lex_not_equal,
        lex_not,
        lex_less_equal,
        lex_greater_equal,
        lex_and,
        lex_or,
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
        lex_float,
        lex_int,
        lex_lparen,
        lex_rparen,
        lex_lbrace,
        lex_rbrace,
        lex_comma,
        lex_semicolon,
        lex_lbracket,
        lex_rbracket,
        lex_percent,
        lex_colon,
        lex_unrecognized, // ← ADD THIS LAST
    ));

    delimited(multispace0, alt((token_parser, token_parser2)), multispace0).parse(input)
}

/// Fails to parse any valid token, returning an error intentionally.
///
/// This function is used as a fallback to catch unrecognized tokens
/// and trigger a `LexError` with position tracking during lexing.
fn lex_unrecognized(_input: &str) -> IResult<&str, Token> {
    Err(nom::Err::Error(nom::error::Error::new(
        _input,
        nom::error::ErrorKind::Tag,
    )))
}

/// Calculates the line and column number corresponding to a byte offset in the input.
///
/// This helper is used for error reporting by determining where a tokenizing failure occurred.
///
/// # Arguments
/// - `input`: The full input string.
/// - `position`: The offset (in bytes) where the error occurred.
///
/// # Returns
/// - A tuple `(line, column)` indicating the location of the error.
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

/// -----------------------------------------------
/// Tokenize the full input string
/// -----------------------------------------------
///
/// This is the main lexing function that processes the entire input string
/// and produces a list of tokens. It repeatedly calls `lex_token` to extract
/// tokens one by one until the input is fully consumed or an error occurs.
///
/// If an error occurs it uses `find_line_and_column` to determine the
/// location of the error in the input string and returns a `LexError`
///
/// # Arguments
/// - `input`: The input string to be tokenized.
///
/// # Returns
/// - `Result<Vec<Token>, LexError>`: A result containing either a vector of tokens
///   or a `LexError` if an error occurred during lexing.
///
/// # Example
/// ```
/// use superclean::lexer::lex;
/// use superclean::token::Token;
///
/// fn main() {
///     let input = "let x = 42;";
///     let tokens = lex(input);
///     assert_eq!(tokens, Ok(vec![
///         Token::Let,
///         Token::Identifier("x".to_string()),
///         Token::Equal,
///         Token::Integer(42),
///         Token::Semicolon,
///     ]));
/// }
/// ```
/// # Errors
/// - `LexError`: If an unrecognized token is found or if the input is incomplete.
///
/// # Panics
/// - This function does not panic, but it may return an error if the input is invalid.
///
/// # Notes
/// - The function uses the `nom` library for parsing and lexing.
/// - The function is designed to be extensible, allowing for additional token types
///  to be added easily.
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
