#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    Fun,
    Return,
    If,
    Else,
    While,
    Print,
    True,
    False,
    IntType,
    BoolType,
    StringType,
    UnitType,

    // Identifiers and literals
    Identifier(String),
    Integer(i64),
    StringLiteral(String),
    LineComment(String),
    BlockComment(String),
    Float(f64),

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    EqualEqual,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Colon,
    Percent,

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    LBracket,
    RBracket,
    Newline,

    // End of input
    EOF,
}
