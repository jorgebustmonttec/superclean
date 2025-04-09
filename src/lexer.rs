pub mod comments;
pub mod core;
pub mod delimiters;
pub mod keywords;
pub mod literals;
pub mod operators;
pub mod whitespace;

pub use core::{find_line_and_column, lex};

// ======================= Tests =======================

#[cfg(test)]
mod tests {
    use crate::lexer::lex;
    use crate::token::Token;

    mod whitespace_tests {
        use super::*;

        #[test]
        fn test_whitespace_and_newlines() {
            assert_eq!(lex(" "), Ok(vec![Token::Whitespace(" ".to_string())]));
            assert_eq!(lex("\t"), Ok(vec![Token::Whitespace("\t".to_string())]));
            assert_eq!(lex("\n"), Ok(vec![Token::Newline]));
            assert_eq!(
                lex(" \t\n"),
                Ok(vec![Token::Whitespace(" \t".to_string()), Token::Newline,])
            );
        }
    }

    mod keyword_and_ident_tests {
        use super::*;

        #[test]
        fn test_keywords_and_identifiers() {
            assert_eq!(
                lex("let x = 42;"),
                Ok(vec![
                    Token::Let,
                    Token::Whitespace(" ".to_string()),
                    Token::Identifier("x".to_string()),
                    Token::Whitespace(" ".to_string()),
                    Token::Equal,
                    Token::Whitespace(" ".to_string()),
                    Token::Integer(42),
                    Token::Semicolon,
                ])
            );
        }
    }

    mod comment_tests {
        use super::*;

        #[test]
        fn test_comments() {
            assert_eq!(
                lex("// comment"),
                Ok(vec![Token::LineComment(" comment".to_string())])
            );
            assert_eq!(
                lex("/* block */"),
                Ok(vec![Token::BlockComment(" block ".to_string())])
            );
        }
    }

    mod literal_tests {
        use super::*;

        #[test]
        fn test_literals() {
            assert_eq!(lex("42"), Ok(vec![Token::Integer(42)]));
            assert_eq!(lex("3.14"), Ok(vec![Token::Float(3.14)]));
            assert_eq!(
                lex("\"hello\""),
                Ok(vec![Token::StringLiteral("hello".to_string())])
            );
        }
    }

    mod operator_and_delim_tests {
        use super::*;

        #[test]
        fn test_operators_and_delimiters() {
            assert_eq!(
                lex("+ - * / % = == != < <= > >= && || ! : ; , ( ) { } [ ]"),
                Ok(vec![
                    Token::Plus,
                    Token::Whitespace(" ".to_string()),
                    Token::Minus,
                    Token::Whitespace(" ".to_string()),
                    Token::Star,
                    Token::Whitespace(" ".to_string()),
                    Token::Slash,
                    Token::Whitespace(" ".to_string()),
                    Token::Percent,
                    Token::Whitespace(" ".to_string()),
                    Token::Equal,
                    Token::Whitespace(" ".to_string()),
                    Token::EqualEqual,
                    Token::Whitespace(" ".to_string()),
                    Token::NotEqual,
                    Token::Whitespace(" ".to_string()),
                    Token::Less,
                    Token::Whitespace(" ".to_string()),
                    Token::LessEqual,
                    Token::Whitespace(" ".to_string()),
                    Token::Greater,
                    Token::Whitespace(" ".to_string()),
                    Token::GreaterEqual,
                    Token::Whitespace(" ".to_string()),
                    Token::And,
                    Token::Whitespace(" ".to_string()),
                    Token::Or,
                    Token::Whitespace(" ".to_string()),
                    Token::Not,
                    Token::Whitespace(" ".to_string()),
                    Token::Colon,
                    Token::Whitespace(" ".to_string()),
                    Token::Semicolon,
                    Token::Whitespace(" ".to_string()),
                    Token::Comma,
                    Token::Whitespace(" ".to_string()),
                    Token::LParen,
                    Token::Whitespace(" ".to_string()),
                    Token::RParen,
                    Token::Whitespace(" ".to_string()),
                    Token::LBrace,
                    Token::Whitespace(" ".to_string()),
                    Token::RBrace,
                    Token::Whitespace(" ".to_string()),
                    Token::LBracket,
                    Token::Whitespace(" ".to_string()),
                    Token::RBracket,
                ])
            );
        }
    }
}
