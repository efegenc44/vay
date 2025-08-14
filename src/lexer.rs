use core::{
    iter::{Iterator, Peekable},
    str::Chars,
};

use crate::{
    location::{Located, Position, SourceLocation},
    reportable::{Reportable, ReportableResult},
    interner::interner_mut,
    token::Token
};

const PUNCTUATION_CHARS: &[char] = &[
    ';', ':', ',', '(', ')',
    '{', '}', '.', '=', '-',
    '+', '*', '/', '<', '>',
    '|', '&', '[', ']',
];

macro_rules! locate {
    ($self:expr, $block:block) => {{
        let start = $self.position;
        $block
        let end = $self.position;
        SourceLocation::new(start, end)
    }};
}

pub struct Lexer<'source_content> {
    chars: Peekable<Chars<'source_content>>,
    index: usize,
    position: Position,
    source: String,
}

impl<'source> Lexer<'source> {
    pub fn new(source: String, source_content: &'source str) -> Self {
        Self {
            chars: source_content.chars().peekable(),
            index: 0,
            position: Position::new(1, 1),
            source,
        }
    }

    pub fn source(&self) -> &str {
        &self.source
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
            "fun" => Token::FunKeyword,
            "variant" => Token::VariantKeyword,
            "match" => Token::MatchKeyword,
            "interface" => Token::InterfaceKeyword,
            "let" => Token::LetKeyword,
            "in" => Token::InKeyword,
            "as" => Token::AsKeyword,
            "return" => Token::ReturnKeyword,
            "struct" => Token::StructKeyword,
            "builtin" => Token::BuiltInKeyword,
            "external" => Token::ExternalKeyword,
            "" => unreachable!(),
            _ => Token::Identifier(interner_mut().intern(lexeme)),
        };

        Located::new(token, location)
    }

    fn string(&mut self) -> ReportableResult<Located<Token>> {
        let mut string = String::new();
        let location = locate!(self, {
            self.advance().unwrap(); // NOTE: Advance the first `"`
            while let Some(ch) = self.peek_ch() {
                if *ch != '"' {
                    if *ch == '\\' {
                        string.push(self.string_escape()?);
                    } else {
                        string.push(self.advance().unwrap());
                    }
                } else {
                    break;
                }
            }
        });

        if self.peek_ch().is_none() {
            return self.error(LexError::UnterminatedStringLiteral, location);
        }
        self.advance().unwrap();

        // NOTE: Add the position of last `"`
        let mut end = location.end();
        end.advance();
        let location = SourceLocation::new(location.start(), end);

        let string = Token::String(interner_mut().intern(string));
        Ok(Located::new(string, location))
    }

    fn string_escape(&mut self) -> ReportableResult<char> {
        let escape;
        let location = locate!(self, {
            self.advance().unwrap(); // NOTE: Advance `\`

            escape = if let Some(ch) = self.advance() {
                match ch {
                    '\\' => Ok('\\'),
                    'n'  => Ok('\n'),
                    'r'  => Ok('\r'),
                    't'  => Ok('\t'),
                    '0'  => Ok('\0'),
                    '"'  => Ok('\"'),
                    unknown => Err(Some(unknown))
                }
            } else {
                Err(None)
            };
        });

        match escape {
            Ok(ch) => Ok(ch),
            Err(Some(unknown)) => self.error(LexError::UnknownEscapeSequence(unknown), location),
            Err(None) => self.error(LexError::UnterminatedEscapeSequence, location),
        }
    }

    fn number(&mut self) -> Located<Token> {
        let mut lexeme = String::new();
        let token;
        let location = locate!(self, {
            while let Some(ch) = self.peek_ch() {
                if ch.is_ascii_digit() {
                    lexeme.push(self.advance().unwrap());
                } else {
                    break;
                }
            }

            if self.optional('.') {
                lexeme.push('.');
                while let Some(ch) = self.peek_ch() {
                    if ch.is_ascii_digit() {
                        lexeme.push(self.advance().unwrap());
                    } else {
                        break;
                    }
                }

                let number = lexeme.parse::<f32>().unwrap();
                token = Token::F32(number);
            } else {
                let number = lexeme.parse::<u64>().unwrap();
                token = Token::U64(number);
            }
        });

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
                '[' => Token::LeftSquare,
                ']' => Token::RightSquare,
                '{' => Token::LeftCurly,
                '}' => Token::RightCurly,
                '.' => {
                    if self.optional('.') {
                        Token::DoubleDot
                    } else {
                        Token::Dot
                    }
                },
                '=' => {
                    if self.optional('=') {
                        Token::DoubleEquals
                    } else {
                        Token::Equals
                    }
                },
                '-' => Token::Minus,
                '+' => Token::Plus,
                '*' => Token::Asterisk,
                '/' => {
                    if self.optional('=') {
                        Token::SlashEquals
                    } else {
                        Token::Slash
                    }
                },
                '<' => {
                    if self.optional('=') {
                        Token::LessEquals
                    } else {
                        Token::Less
                    }
                },
                '>' => {
                    if self.optional('=') {
                        Token::GreaterEquals
                    } else {
                        Token::Greater
                    }
                },
                '|' => Token::Bar,
                '&' => Token::Ampersand,
                _ => unreachable!(),
            };
        });

        Located::new(token, location)
    }

    fn error<T>(&self, error: LexError, location: SourceLocation) -> ReportableResult<T> {
        let reportable = (Located::new(error, location), self.source.clone());
        Err(Box::new(reportable))
    }
}

impl Iterator for Lexer<'_> {
    type Item = ReportableResult<Located<Token>>;

    fn next(&mut self) -> Option<Self::Item> {
        while self.peek_ch()?.is_whitespace() {
            self.advance();
        }

        let ch = *self.peek_ch()?;

        let result = if ch.is_alphabetic() {
            Ok(self.identifier_or_keyword())
        } else if ch == '"' {
            self.string()
        } else if ch.is_ascii_digit() {
            Ok(self.number())
        } else if PUNCTUATION_CHARS.contains(&ch) {
            Ok(self.punctuation())
        } else {
            let location = locate!(self, {
                self.advance();
            });

            self.error(LexError::UnknownStartOfAToken(ch), location)
        };

        Some(result)
    }
}

#[derive(Clone)]
pub enum LexError {
    UnknownStartOfAToken(char),
    UnterminatedStringLiteral,
    UnterminatedEscapeSequence,
    UnknownEscapeSequence(char),
}

impl Reportable for (Located<LexError>, String) {
    fn location(&self) -> SourceLocation {
        self.0.location()
    }

    fn source(&self) -> &str {
        &self.1
    }

    fn description(&self) -> String {
        match self.0.data() {
            LexError::UnknownStartOfAToken(ch) => {
                format!("Encountered an unknown start of a token: `{ch}`.")
            }
            LexError::UnterminatedStringLiteral => {
                "String literal is not terminated.".into()
            }
            LexError::UnterminatedEscapeSequence => {
                "Espace sequence is not terminated.".into()
            }
            LexError::UnknownEscapeSequence(ch) => {
                format!("Encountered an unknown escape sequence: `{ch}`.")
            }
        }
    }
}
