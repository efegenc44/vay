use core::{
    iter::{Iterator, Peekable},
    str::Chars,
};
use std::fmt::Display;

use crate::{
    interner::Interner,
    token::{Keyword, Punctuation, Token},
};

pub struct Lexer<'source> {
    chars: Peekable<Chars<'source>>,
    index: usize,

    row: usize,
    column: usize,

    interner: Interner,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str, interner: Interner) -> Self {
        Self {
            chars: source.chars().peekable(),
            index: 0,

            row: 1,
            column: 1,

            interner,
        }
    }

    fn peek_ch(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next();

        if let Some('\n') = ch {
            self.row += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }

        self.index += 1;
        ch
    }

    fn identifier_or_keyword(&mut self) -> Token {
        let mut lexeme = String::new();
        while let Some(ch) = self.peek_ch() {
            if ch.is_alphanumeric() {
                lexeme.push(self.advance().unwrap());
            } else {
                break;
            }
        }

        let keyword = match lexeme.as_str() {
            "proc" => Keyword::Proc,
            _ => return Token::Identifier(self.interner.intern(lexeme)),
        };

        Token::Keyword(keyword)
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexResult<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.peek_ch()?.is_whitespace() {
            self.advance();
        }

        let ch = self.peek_ch()?;

        if ch.is_alphabetic() {
            return Some(Ok(self.identifier_or_keyword()));
        }

        let punct = match ch {
            ';' => Punctuation::Semicolon,
            _ => return Some(Err(LexError::UnknownStartOfAToken(*ch))),
        };
        self.advance();

        Some(Ok(Token::Punctuation(punct)))
    }
}

pub enum LexError {
    UnknownStartOfAToken(char),
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexError::UnknownStartOfAToken(ch) => {
                write!(f, "Encountered an unknown start of a token: `{ch}`.")
            }
        }
    }
}

type LexResult<T> = Result<T, LexError>;
