use std::fmt::Display;

use crate::interner::InternIdx;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Token {
    Identifier(InternIdx),
    Keyword(Keyword),
    Punctuation(Punctuation),
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
            Token::Keyword(keyword) => write!(f, "KEYWORD: {keyword}"),
            Token::Punctuation(punctuation) => write!(f, "PUNCTUATION: {punctuation}"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Proc,
    Variant,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Proc => write!(f, "proc"),
            Keyword::Variant => write!(f, "variant"),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Punctuation {
    Semicolon,
    Colon,
    Comma,
    LeftParenthesis,
    RightParenthesis,
    LeftCurly,
    RightCurly,
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Punctuation::Semicolon => write!(f, ";"),
            Punctuation::Colon => write!(f, ":"),
            Punctuation::Comma => write!(f, ","),
            Punctuation::LeftParenthesis => write!(f, "("),
            Punctuation::RightParenthesis => write!(f, ")"),
            Punctuation::LeftCurly => write!(f, "{{"),
            Punctuation::RightCurly => write!(f, "}}"),
        }
    }
}
