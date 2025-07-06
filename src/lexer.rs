use core::{
    iter::{Iterator, Peekable},
    str::Chars,
};
use std::fmt::Display;

use crate::{
    error::Error, interner::Interner, location::{Located, Position, SourceLocation}, token::Token
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

pub struct Lexer<'source, 'interner> {
    chars: Peekable<Chars<'source>>,
    index: usize,
    position: Position,
    interner: &'interner mut Interner,
}

impl<'source, 'interner> Lexer<'source, 'interner> {
    pub fn new(source: &'source str, interner: &'interner mut Interner) -> Self {
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

    fn optional(&mut self, optional: char) -> bool {
        match self.peek_ch() {
            Some(ch) if *ch == optional => {
                self.advance();
                true
            },
            Some(_) => false,
            None => false,
        }
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

        let token = match lexeme.as_str() {
            "module" => Token::ModuleKeyword,
            "import" => Token::ImportKeyword,
            "proc" => Token::ProcKeyword,
            "variant" => Token::VariantKeyword,
            "" => unreachable!(),
            _ => Token::Identifier(self.interner.intern(lexeme)),
        };

        Located::new(token, location)
    }

    fn punctuation(&mut self) -> Located<Token> {
        let token;
        let location = locate!(self, {
            token = match self.advance().unwrap() {
                ';' => Token::Semicolon,
                ':' => if self.optional(':') { Token::DoubleColon } else { Token::Colon },
                ',' => Token::Comma,
                '(' => Token::LeftParenthesis,
                ')' => Token::RightParenthesis,
                '{' => Token::LeftCurly,
                '}' => Token::RightCurly,
                _ => unreachable!(),
            };
        });

        Located::new(token, location)
    }
}

impl Iterator for Lexer<'_, '_> {
    type Item = LexResult<Located<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.peek_ch()?.is_whitespace() {
            self.advance();
        }

        let ch = *self.peek_ch()?;

        let result = if ch.is_alphabetic() {
            Ok(self.identifier_or_keyword())
        } else if PUNCTUATION_CHARS.contains(&ch) {
            Ok(self.punctuation())
        } else {
            let start = self.position;
            self.advance();
            let end = self.position;
            Err(Located::new(
                LexError::UnknownStartOfAToken(ch),
                SourceLocation::new(start, end)
            ))
        };

        Some(result)
    }
}

#[derive(Clone)]
pub enum LexError {
    UnknownStartOfAToken(char),
}

impl Error for Located<LexError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self) -> String {
        self.data().to_string()
    }
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

type LexResult<T> = Result<T, Located<LexError>>;
