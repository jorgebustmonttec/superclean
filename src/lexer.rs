pub mod comments;
pub mod core;
pub mod delimiters;
pub mod keywords;
pub mod literals;
pub mod operators;

pub use core::{find_line_and_column, lex};

// ======================= Tests =======================

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
        assert_eq!(lex("="), Ok(vec![Token::Equal]));
        assert_eq!(lex("%"), Ok(vec![Token::Percent]));
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
        assert_eq!(
            lex("foo123"),
            Ok(vec![Token::Identifier("foo123".to_string())])
        );
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
            Ok(vec![Token::StringLiteral(
                "escaped \\ and \n and \"quotes\"".to_string()
            )])
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
            Ok(vec![Token::BlockComment(
                " this is a block comment ".to_string()
            )])
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

    // Error handling tests

    #[test]
    fn test_lex_unrecognized_token() {
        let err = lex("@").unwrap_err();
        assert_eq!(err.line, 1);
        assert_eq!(err.column, 1);
        assert!(err.message.contains("Unrecognized"));
    }

    #[test]
    fn test_lex_error_position_multiline() {
        let code = r#"
let x = 5;
let y = @;
        "#;

        let err = lex(code).unwrap_err();
        assert_eq!(err.line, 3);
        //assert!(err.column > 0); // You can refine this later if needed
        assert_eq!(err.column, 9);
        assert!(err.message.contains("Unrecognized"));
    }

    // Test lexing of tuple
    #[test]
    fn test_lex_tuple() {
        assert_eq!(
            lex("(1, 2)"),
            Ok(vec![
                Token::LParen,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::RParen
            ])
        );
    }

    // Test lexing of List
    #[test]
    fn test_lex_list() {
        assert_eq!(
            lex("[1, 2, 3]"),
            Ok(vec![
                Token::LBracket,
                Token::Integer(1),
                Token::Comma,
                Token::Integer(2),
                Token::Comma,
                Token::Integer(3),
                Token::RBracket
            ])
        );
    }

    #[test]
    fn test_lex_floats() {
        assert_eq!(lex("3.14"), Ok(vec![Token::Float(3.14)]));
        assert_eq!(lex("0.001"), Ok(vec![Token::Float(0.001)]));
        assert_eq!(lex("10.0"), Ok(vec![Token::Float(10.0)]));
        assert_eq!(
            lex("1.0 + 2"),
            Ok(vec![Token::Float(1.0), Token::Plus, Token::Integer(2)])
        );
    }

    #[test]
    fn test_lex_float_then_id() {
        assert_eq!(
            lex("3.14abc"),
            Ok(vec![
                Token::Float(3.14),
                Token::Identifier("abc".to_string())
            ])
        );
    }

    // Test lexing logical operators
    #[test]
    fn test_lex_logical_operators() {
        assert_eq!(lex("&&"), Ok(vec![Token::And]));
        assert_eq!(lex("||"), Ok(vec![Token::Or]));
        assert_eq!(lex("!"), Ok(vec![Token::Not]));
    }
    // Test lexing of a complex expression with logical operators
    #[test]
    fn test_lex_complex_expr_logical() {
        assert_eq!(
            lex("x && y || z"),
            Ok(vec![
                Token::Identifier("x".to_string()),
                Token::And,
                Token::Identifier("y".to_string()),
                Token::Or,
                Token::Identifier("z".to_string())
            ])
        );
    }
}
