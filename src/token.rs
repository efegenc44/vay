use std::fmt::Display;

use crate::interner::InternIdx;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Identifier(InternIdx),

    Semicolon,
    Colon,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,

    ProcKeyword,
    VariantKeyword,
}

impl Token {
    pub const fn dummy_identifier() -> Token {
        Token::Identifier(InternIdx::dummy_idx())
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(intern_idx) => write!(f, "IDENTIFIER: {}", intern_idx.idx()),

            Token::Semicolon => write!(f, ";"),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::LeftParenthesis => write!(f, "("),
            Token::RightParenthesis => write!(f, ")"),
            Token::LeftCurly => write!(f, "{{"),
            Token::RightCurly => write!(f, "}}"),

            Token::ProcKeyword => write!(f, "proc"),
            Token::VariantKeyword => write!(f, "variant"),
        }
    }
}
