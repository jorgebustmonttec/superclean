use nom::{
    branch::alt,
    character::complete::{char, digit1, multispace0},
    combinator::map_res,
    multi::many0,
    sequence::delimited,
    IResult, Parser,
};

use crate::token::Token;


// ======================= Literals =======================
//
/// Tokenize an integer literal
fn lex_int(input: &str) -> IResult<&str, Token> {
    map_res(digit1, |s: &str| s.parse::<i64>().map(Token::Integer)).parse(input)
}


// ================ Mathematical Operators ================

/// Tokenize the `+` symbol
fn lex_plus(input: &str) -> IResult<&str, Token> {
    char('+').map(|_| Token::Plus).parse(input)
}

/// Tokenize the `-` symbol
fn lex_minus(input: &str) -> IResult<&str, Token> {
    char('-').map(|_| Token::Minus).parse(input)
}

/// Tokenize the `*` symbol
fn lex_star(input: &str) -> IResult<&str, Token> {
    char('*').map(|_| Token::Star).parse(input)
}

/// Tokenize the `/` symbol
fn lex_slash(input: &str) -> IResult<&str, Token> {
    char('/').map(|_| Token::Slash).parse(input)
}


// ================= Comparison Operators =================

/// Tokenize the `==' symbol
fn lex_equal_equal(input: &str) -> IResult<&str, Token> {
    nom::bytes::complete::tag("==").map(|_| Token::EqualEqual).parse(input)
}

/// Tokenize the `!=` symbol
fn lex_not_equal(input: &str) -> IResult<&str, Token> {
    nom::bytes::complete::tag("!=").map(|_| Token::NotEqual).parse(input)
}

/// Tokenize the `<=` symbol
fn lex_less_equal(input: &str) -> IResult<&str, Token> {
    nom::bytes::complete::tag("<=").map(|_| Token::LessEqual).parse(input)
}

/// Tokenize the `>=` symbol
fn lex_greater_equal(input: &str) -> IResult<&str, Token> {
    nom::bytes::complete::tag(">=").map(|_| Token::GreaterEqual).parse(input)
}

/// Tokenize the `<` symbol
fn lex_less(input: &str) -> IResult<&str, Token> {
    char('<').map(|_| Token::Less).parse(input)
}

/// Tokenize the `>` symbol
fn lex_greater(input: &str) -> IResult<&str, Token> {
    char('>').map(|_| Token::Greater).parse(input)
}



// ======================= Tokenization =======================
fn lex_token(input: &str) -> IResult<&str, Token> {
    delimited(
        multispace0,
        alt((
            lex_equal_equal,
            lex_not_equal,
            lex_less_equal,
            lex_greater_equal,
            lex_plus,
            lex_minus,
            lex_star,
            lex_slash,
            lex_less,
            lex_greater,
            lex_int,
            lex_lparen,
            lex_rparen,
            lex_lbrace,
            lex_rbrace,
            lex_comma,
            lex_semicolon,
            lex_colon,
            
        )),
        multispace0,
    )
    .parse(input)
}

// ====================== Delimiters ======================

/// Tokenize the `(` symbol
fn lex_lparen(input: &str) -> IResult<&str, Token> {
    char('(').map(|_| Token::LParen).parse(input)
}

/// Tokenize the `)` symbol
fn lex_rparen(input: &str) -> IResult<&str, Token> {
    char(')').map(|_| Token::RParen).parse(input)
}

/// Tokenize the `{` symbol
fn lex_lbrace(input: &str) -> IResult<&str, Token> {
    char('{').map(|_| Token::LBrace).parse(input)
}

/// Tokenize the `}` symbol
fn lex_rbrace(input: &str) -> IResult<&str, Token> {
    char('}').map(|_| Token::RBrace).parse(input)
}

/// Tokenize the `,` symbol
fn lex_comma(input: &str) -> IResult<&str, Token> {
    char(',').map(|_| Token::Comma).parse(input)
}

/// Tokenize the `;` symbol
fn lex_semicolon(input: &str) -> IResult<&str, Token> {
    char(';').map(|_| Token::Semicolon).parse(input)
}

/// Tokenize the `:` symbol
fn lex_colon(input: &str) -> IResult<&str, Token> {
    char(':').map(|_| Token::Colon).parse(input)
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


    // ======================= Tests =======================

    // Test the lexing of a single integer
    #[test]
    fn test_lex_int() {
        assert_eq!(lex("1"), Ok(vec![Token::Integer(1)]));
        assert_eq!(lex("42"), Ok(vec![Token::Integer(42)]));
    }

    // Test the lexing of a single `+` symbol
    #[test]
    fn test_lex_plus() {
        assert_eq!(lex("+"), Ok(vec![Token::Plus]));
    }

    // Test the lexing of a single `==` symbol
    #[test]
    fn test_lex_simple_expr() {
        assert_eq!(
            lex("1 + 1"),
            Ok(vec![Token::Integer(1), Token::Plus, Token::Integer(1)])
        );
    }

    // Test the lexing of a simple expression with spaces
    #[test]
    fn test_lex_with_spaces() {
        assert_eq!(
            lex("   3    +   9  "),
            Ok(vec![Token::Integer(3), Token::Plus, Token::Integer(9)])
        );
    }

    // Test rest of the operators
    #[test]
    fn test_lex_ops() {
        assert_eq!(lex("-"), Ok(vec![Token::Minus]));
        assert_eq!(lex("*"), Ok(vec![Token::Star]));
        assert_eq!(lex("/"), Ok(vec![Token::Slash]));
        assert_eq!(lex("=="), Ok(vec![Token::EqualEqual]));
        assert_eq!(lex("!="), Ok(vec![Token::NotEqual]));
        assert_eq!(lex("<="), Ok(vec![Token::LessEqual]));
        assert_eq!(lex(">="), Ok(vec![Token::GreaterEqual]));
        assert_eq!(lex("<"), Ok(vec![Token::Less]));
        assert_eq!(lex(">"), Ok(vec![Token::Greater]));
    }

    // Test lexing of a complex expression
    #[test]
    fn test_lex_ops_mixed() {
        assert_eq!(
            lex("1 + 2 * 3 - 4 / 5"),
            Ok(vec![
                Token::Integer(1),
                Token::Plus,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
                Token::Minus,
                Token::Integer(4),
                Token::Slash,
                Token::Integer(5)
            ])
        );

        /*
        assert_eq!(
            lex("a == b != c <= d >= e < f > g"),
            Ok(vec![
                Token::Identifier("a".to_string()), // placeholder for now
                Token::EqualEqual,
                Token::Identifier("b".to_string()),
                Token::NotEqual,
                Token::Identifier("c".to_string()),
                Token::LessEqual,
                Token::Identifier("d".to_string()),
                Token::GreaterEqual,
                Token::Identifier("e".to_string()),
                Token::Less,
                Token::Identifier("f".to_string()),
                Token::Greater,
                Token::Identifier("g".to_string()),
            ])
        );
        */
    }

    // Test lexing of delimiters
    #[test]
    fn test_lex_delimiters() {
        assert_eq!(lex("("), Ok(vec![Token::LParen]));
        assert_eq!(lex(")"), Ok(vec![Token::RParen]));
        assert_eq!(lex("{"), Ok(vec![Token::LBrace]));
        assert_eq!(lex("}"), Ok(vec![Token::RBrace]));
        assert_eq!(lex(","), Ok(vec![Token::Comma]));
        assert_eq!(lex(";"), Ok(vec![Token::Semicolon]));
        assert_eq!(lex(":"), Ok(vec![Token::Colon]));
    }

    // Test lexing of a complex expression with delimiters
    #[test]
    fn test_lex_complex_expr() {
        assert_eq!(
            lex("1 + (2 * 3) - {4 / 5}"),
            Ok(vec![
                Token::Integer(1),
                Token::Plus,
                Token::LParen,
                Token::Integer(2),
                Token::Star,
                Token::Integer(3),
                Token::RParen,
                Token::Minus,
                Token::LBrace,
                Token::Integer(4),
                Token::Slash,
                Token::Integer(5),
                Token::RBrace
            ])
        );
    }
    // Test lexing of a complex expression with delimiters and spaces

}
