use core::{
    iter::{Iterator, Peekable},
    str::Chars,
};
use std::fmt::Display;

use crate::{
    interner::Interner,
    location::{Located, Position, SourceLocation},
    token::{Keyword, Punctuation, Token},
};

const PUNCTUATION_CHARS: &[char] = &[';', ':', ',', '(', ')', '{', '}'];

macro_rules! locate {
    ($self:expr, $block:block) => {{
        let start = $self.position;
        $block
        let end = $self.position;
        SourceLocation::new(start, end)
    }};
}

pub struct Lexer<'source> {
    chars: Peekable<Chars<'source>>,
    index: usize,
    position: Position,
    interner: Interner,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str, interner: Interner) -> Self {
        Self {
            chars: source.chars().peekable(),
            index: 0,
            position: Position::new(1, 1),
            interner,
        }
    }

    fn peek_ch(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.chars.next();

        if let Some('\n') = ch {
            self.position.newline();
        } else {
            self.position.advance();
        }

        self.index += 1;
        ch
    }

    fn identifier_or_keyword(&mut self) -> Located<Token> {
        let mut lexeme = String::new();
        let location = locate!(self, {
            while let Some(ch) = self.peek_ch() {
                if ch.is_alphanumeric() {
                    lexeme.push(self.advance().unwrap());
                } else {
                    break;
                }
            }
        });

        let keyword = match lexeme.as_str() {
            "proc" => Keyword::Proc,
            "variant" => Keyword::Variant,
            "" => unreachable!(),
            _ => {
                let token = Token::Identifier(self.interner.intern(lexeme));
                return Located::new(token, location);
            }
        };

        Located::new(Token::Keyword(keyword), location)
    }

    fn punctuation(&mut self) -> Located<Token> {
        let punctuation;
        let location = locate!(self, {
            punctuation = match self.peek_ch().unwrap() {
                ';' => Punctuation::Semicolon,
                ':' => Punctuation::Colon,
                ',' => Punctuation::Comma,
                '(' => Punctuation::LeftParenthesis,
                ')' => Punctuation::RightParenthesis,
                '{' => Punctuation::LeftCurly,
                '}' => Punctuation::RightCurly,
                _ => unreachable!(),
            };
            self.advance();
        });

        Located::new(Token::Punctuation(punctuation), location)
    }
}

impl Iterator for Lexer<'_> {
    type Item = LexResult<Located<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.peek_ch()?.is_whitespace() {
            self.advance();
        }

        let ch = self.peek_ch()?;

        let result = if ch.is_alphabetic() {
            Ok(self.identifier_or_keyword())
        } else if PUNCTUATION_CHARS.contains(ch) {
            Ok(self.punctuation())
        } else {
            Err(LexError::UnknownStartOfAToken(*ch))
        };

        Some(result)
    }
}

#[derive(Clone)]
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
