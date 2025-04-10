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
    Break,
    For,
    In,

    // Types
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

    // Miscellaneous
    Whitespace(String), // for spaces, tabs, etc.
    Newline,            // for newlines like \n

    // End of input
    EOF,
}

impl Token {
    /// Returns the visual length (in characters) of the token as written in code.
    pub fn visual_length(&self) -> usize {
        match self {
            Token::Identifier(s) => s.len(),
            Token::StringLiteral(s) => s.len() + 2, // includes quotes
            Token::LineComment(s) => 2 + s.len(),   // //comment
            Token::BlockComment(s) => 4 + s.len(),  // /*comment*/
            Token::Integer(n) => n.to_string().len(),
            Token::Float(f) => f.to_string().len(),
            Token::Whitespace(s) => s.len(),
            Token::Newline => 1,

            Token::Let => 3,
            Token::Var => 3,
            Token::Const => 5,
            Token::Fun => 3,
            Token::Return => 6,
            Token::If => 2,
            Token::Else => 4,
            Token::While => 5,
            Token::Print => 5,
            Token::True => 4,
            Token::False => 5,
            Token::Break => 5,
            Token::For => 3,
            Token::In => 2,

            Token::IntType => 3,
            Token::BoolType => 4,
            Token::StringType => 6,
            Token::UnitType => 4,
            Token::FloatType => 5,

            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Equal
            | Token::Less
            | Token::Greater
            | Token::Colon
            | Token::Percent
            | Token::Comma
            | Token::Semicolon
            | Token::LParen
            | Token::RParen
            | Token::LBrace
            | Token::RBrace
            | Token::LBracket
            | Token::RBracket
            | Token::Not => 1,

            Token::EqualEqual
            | Token::NotEqual
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::And
            | Token::Or => 2,

            Token::EOF => 0,
        }
    }
}
