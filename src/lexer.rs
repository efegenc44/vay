use core::{
    iter::{Iterator, Peekable},
    str::Chars,
};

use crate::{
    interner::Interner,
    location::{Located, Position, SourceLocation},
    reportable::{Reportable, ReportableResult},
    token::Token,
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
            }
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
                ':' => {
                    if self.optional(':') {
                        Token::DoubleColon
                    } else {
                        Token::Colon
                    }
                }
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

    fn unknown_start_of_a_token<T>(ch: char, location: SourceLocation) -> ReportableResult<T> {
        let lex_error = LexError::UnknownStartOfAToken(ch);

        Err(Box::new(Located::new(lex_error, location)))
    }
}

impl Iterator for Lexer<'_, '_> {
    type Item = ReportableResult<Located<Token>>;

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
            let location = locate!(self, {
                self.advance();
            });

            Self::unknown_start_of_a_token(ch, location)
        };

        Some(result)
    }
}

#[derive(Clone)]
pub enum LexError {
    UnknownStartOfAToken(char),
}

impl Reportable for Located<LexError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self, _interner: &Interner) -> String {
        match self.data() {
            LexError::UnknownStartOfAToken(ch) => {
                format!("Encountered an unknown start of a token: `{ch}`.")
            }
        }
    }
}
