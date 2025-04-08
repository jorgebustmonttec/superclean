#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Keywords
    Let,
    Var,
    Const,
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
    FloatType,

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
    And,
    Or,
    Not,

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
