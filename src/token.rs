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

    // Delimiters
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,

    // End of input
    EOF,
}
