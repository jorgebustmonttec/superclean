use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_until}, // <-- ADD these here
    character::complete::{char, digit1, multispace0, alphanumeric0, alpha1},
    combinator::{map_res},
    multi::many0,
    sequence::{delimited, preceded},
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


// ======================= Keywords =======================

/// Tokenize identifiers and keywords.
/// If the word is a keyword (like `let`, `if`, `true`), return that token.
/// Otherwise, return it as an `Identifier(String)`.
fn lex_identifier_or_keyword(input: &str) -> IResult<&str, Token> {
    // Match a sequence that starts with a letter and may be followed by letters/numbers
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

/// Tokenize the `=` symbol/// Tokenize the `=` symbol
fn lex_equal(input: &str) -> IResult<&str, Token> {
    char('=').map(|_| Token::Equal).parse(input)
}


// ======================= String Literals =======================

/// Tokenize a string literal like `"hello"`
/// Supports escaped characters like `\"`, `\\`, `\n`
fn lex_string(input: &str) -> IResult<&str, Token> {
    println!("===> TRYING TO PARSE STRING from: {input:?}");

    let mut escaped = false;
    let mut result = String::new();
    let mut chars = input.chars().enumerate();

    // Ensure it starts with a "
    let Some((mut i, '"')) = chars.next() else {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char)));
    };

    // Walk through characters
    while let Some((j, c)) = chars.next() {
        i = j;
        if escaped {
            let unescaped = match c {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                other => {
                    // Invalid escape, we can reject or include literally
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
            // End of string
            let rest = &input[i + 1..];
            println!("===> STRING SUCCESSFULLY PARSED: {result:?}");
            return Ok((rest, Token::StringLiteral(result)));
        } else {
            result.push(c);
        }
    }

    // If we reached here, we never found closing quote
    Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Char)))
}


/// ======================= Comments =======================

/// Tokenize line comments that start with `//`
/// Consumes until the end of the line or input
fn lex_line_comment(input: &str) -> IResult<&str, Token> {
    preceded(
        tag("//"),
        take_while(|c| c != '\n'),
    )
    .map(|text: &str| Token::LineComment(text.to_string()))
    .parse(input)
}

/// Tokenize block comments like `/* ... */`
/// Consumes everything until the closing `*/`
fn lex_block_comment(input: &str) -> IResult<&str, Token> {
    delimited(
        tag("/*"),
        take_until("*/"),
        tag("*/"),
    )
    .map(|text: &str| Token::BlockComment(text.to_string()))
    .parse(input)
}



// ======================= Tokenization =======================

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
    ));

    delimited(
        multispace0,
        alt((token_parser, token_parser2)),
        multispace0,
    )
    .parse(input)
}



// ================= Main Lexing Function =================

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

    // Test lexing of identifiers and keywords
    #[test]
    fn test_lex_identifiers_and_keywords() {
        assert_eq!(lex("let"), Ok(vec![Token::Let]));
        assert_eq!(lex("fun"), Ok(vec![Token::Fun]));
        assert_eq!(lex("return"), Ok(vec![Token::Return]));
        assert_eq!(lex("if"), Ok(vec![Token::If]));
        assert_eq!(lex("else"), Ok(vec![Token::Else]));
        assert_eq!(lex("while"), Ok(vec![Token::While]));
        assert_eq!(lex("print"), Ok(vec![Token::Print]));
        assert_eq!(lex("true"), Ok(vec![Token::True]));
        assert_eq!(lex("false"), Ok(vec![Token::False]));
        assert_eq!(lex("Int"), Ok(vec![Token::IntType]));
        assert_eq!(lex("Bool"), Ok(vec![Token::BoolType]));
        assert_eq!(lex("String"), Ok(vec![Token::StringType]));
        assert_eq!(lex("Unit"), Ok(vec![Token::UnitType]));
        assert_eq!(lex("x"), Ok(vec![Token::Identifier("x".to_string())]));
        assert_eq!(lex("foo123"), Ok(vec![Token::Identifier("foo123".to_string())]));
    }

    // Test lexing variable binding
    #[test]
    fn test_lex_let_binding() {
        assert_eq!(
            lex("let x = 5;"),
            Ok(vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
            ])
        );
    }

    // Test lexing function definition
    #[test]
    fn test_lex_fun_definition() {
        assert_eq!(
            lex("fun add(x: Int, y: Int): Int { return x + y; }"),
            Ok(vec![
                Token::Fun,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Identifier("x".to_string()),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Identifier("y".to_string()),
                Token::Colon,
                Token::IntType,
                Token::RParen,
                Token::Colon,
                Token::IntType,
                Token::LBrace,
                Token::Return,
                Token::Identifier("x".to_string()),
                Token::Plus,
                Token::Identifier("y".to_string()),
                Token::Semicolon,
                Token::RBrace,
            ])
        );
    }

    // Test lexing an if-else statement
    #[test]
    fn test_lex_if_else_expr() {
        assert_eq!(
            lex("if x > 0 { print x; } else { print 0; }"),
            Ok(vec![
                Token::If,
                Token::Identifier("x".to_string()),
                Token::Greater,
                Token::Integer(0),
                Token::LBrace,
                Token::Print,
                Token::Identifier("x".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Print,
                Token::Integer(0),
                Token::Semicolon,
                Token::RBrace,
            ])
        );
    }

    // Test lexing string literals

    // Test lexing a simple string
    #[test]
    fn test_lex_string_simple() {
        assert_eq!(
            lex(r#""hello world""#),
            Ok(vec![Token::StringLiteral("hello world".to_string())])
        );
    }
    
    // Test lexing a string with escaped quotes
    #[test]
    fn test_lex_string_with_quotes() {
        assert_eq!(
            lex(r#""He said \"hi\"""#),
            Ok(vec![Token::StringLiteral("He said \"hi\"".to_string())])
        );
    }
    
    // Test lexing a string with escaped backslashes
    #[test]
    fn test_lex_string_escaped_stuff() {
        assert_eq!(
            lex("\"escaped \\\\ and \\n and \\\"quotes\\\"\""),
            Ok(vec![Token::StringLiteral("escaped \\ and \n and \"quotes\"".to_string())])
        );
    }
    
    // Test lexing a string with invalid escape sequences
    #[test]
    fn test_lex_string_inside_call() {
        assert_eq!(
            lex(r#"print("hi");"#),
            Ok(vec![
                Token::Print,
                Token::LParen,
                Token::StringLiteral("hi".to_string()),
                Token::RParen,
                Token::Semicolon,
            ])
        );
    }
    

    #[test]
    fn test_lex_line_comment() {
        assert_eq!(
            lex("// hello world"),
            Ok(vec![Token::LineComment(" hello world".to_string())])
        );
    }

    #[test]
    fn test_lex_line_comment_with_code() {
        assert_eq!(
            lex("x = 5; // this is a comment"),
            Ok(vec![
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
                Token::LineComment(" this is a comment".to_string()),
            ])
        );
    }

    #[test]
    fn test_lex_block_comment() {
        assert_eq!(
            lex("/* this is a block comment */"),
            Ok(vec![Token::BlockComment(" this is a block comment ".to_string())])
        );
    }

    #[test]
    fn test_lex_block_comment_with_code() {
        assert_eq!(
            lex("x = 5; /* set x to five */ y = 10;"),
            Ok(vec![
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
                Token::BlockComment(" set x to five ".to_string()),
                Token::Identifier("y".to_string()),
                Token::Equal,
                Token::Integer(10),
                Token::Semicolon,
            ])
        );
    }


    //full test
    #[test]
    fn test_lex_full() {
        let input = r#"
            let x = 5;
            fun add(a: Int, b: Int): Int {
                return a + b;
            }
            if x > 0 {
                print("x is positive");
            } else {
                print("x is non-positive");
            }
        "#;

        assert_eq!(
            lex(input),
            Ok(vec![
                Token::Let,
                Token::Identifier("x".to_string()),
                Token::Equal,
                Token::Integer(5),
                Token::Semicolon,
                Token::Fun,
                Token::Identifier("add".to_string()),
                Token::LParen,
                Token::Identifier("a".to_string()),
                Token::Colon,
                Token::IntType,
                Token::Comma,
                Token::Identifier("b".to_string()),
                Token::Colon,
                Token::IntType,
                Token::RParen,
                Token::Colon,
                Token::IntType,
                Token::LBrace,
                Token::Return,
                Token::Identifier("a".to_string()),
                Token::Plus,
                Token::Identifier("b".to_string()),
                Token::Semicolon,
                Token::RBrace,
                Token::If,
                Token::Identifier("x".to_string()),
                Token::Greater,
                Token::Integer(0),
                Token::LBrace,
                Token::Print,
                Token::LParen,
                Token::StringLiteral("x is positive".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Print,
                Token::LParen,
                Token::StringLiteral("x is non-positive".to_string()),
                Token::RParen,
                Token::Semicolon,
                Token::RBrace
            ])
        );
    }


}
