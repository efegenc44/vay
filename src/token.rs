use std::fmt::Display;

pub enum Token {
    Identifier(String),
    Keyword(Keyword),
    Punctuation(Punctuation),
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Identifier(lexeme) => write!(f, "IDENTIFIER: {lexeme}"),
            Token::Keyword(keyword) => write!(f, "KEYWORD: {keyword}"),
            Token::Punctuation(punctuation) => write!(f, "PUNCTUATION: {punctuation}"),
        }
    }
}

pub enum Keyword {
    Proc,
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::Proc => write!(f, "proc"),
        }
    }
}

pub enum Punctuation {
    Semicolon,
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Punctuation::Semicolon => write!(f, ";"),
        }
    }
}
