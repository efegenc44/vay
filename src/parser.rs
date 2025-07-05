use std::{fmt::Display, iter::Peekable};

use crate::{
    declaration::{Declaration, TypedIdentifier, VariantCase},
    expression::{Expression, TypeExpression},
    interner::InternIdx,
    lexer::{LexError, Lexer},
    location::{self, Located},
    statement::Statement,
    token::{self, Keyword, Punctuation, Token},
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[Token::dummy_identifier()];

const STATEMENT_KEYWORDS: &[Token] = &[Token::dummy_identifier(), Token::Keyword(Keyword::Proc)];

const DECLARATION_KEYWORDS: &[Token] = &[
    Token::Keyword(Keyword::Proc),
    Token::Keyword(Keyword::Variant),
];

pub struct Parser<'source> {
    tokens: Peekable<Lexer<'source>>,
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn peek(&mut self) -> ParseResult<Option<Located<Token>>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(*token)),
            Some(Err(lex_error)) => Err(ParseError::LexError(lex_error.clone())),
            None => Ok(None),
        }
    }

    fn advance(&mut self) -> ParseResult<Option<Located<Token>>> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(lex_error)) => Err(ParseError::LexError(lex_error)),
            None => Ok(None),
        }
    }

    fn expect_one_of(&mut self, expected_ones: &[Token]) -> ParseResult<Located<Token>> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: expected_ones.to_vec(),
            });
        };

        let result = expected_ones
            .iter()
            .find(|expected| expected == &token.data());
        match result {
            Some(_) => Ok(token),
            None => Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: expected_ones.to_vec(),
            }),
        }
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Located<Token>> {
        self.expect_one_of(&[expected])
    }

    fn expect_identifier(&mut self) -> ParseResult<Located<InternIdx>> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: vec![Token::dummy_identifier()],
            });
        };

        if let Token::Identifier(intern_idx) = token.data() {
            Ok(Located::new(*intern_idx, token.location()))
        } else {
            Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: vec![Token::dummy_identifier()],
            })
        }
    }

    fn expression(&mut self) -> ParseResult<Located<Expression>> {
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Located<Expression>> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: PRIMARY_TOKEN_STARTS.to_vec(),
            });
        };

        match token.data() {
            Token::Identifier(intern_idx) => {
                let expression = Expression::Identifier(*intern_idx);
                Ok(Located::new(expression, token.location()))
            }
            _ => Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }),
        }
    }

    fn statement(&mut self) -> ParseResult<Located<Statement>> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: STATEMENT_KEYWORDS.to_vec(),
            });
        };

        match token.data() {
            Token::Keyword(Keyword::Proc) => {
                todo!()
            }
            Token::Keyword(Keyword::Variant) => {
                todo!()
            }
            _ => Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: STATEMENT_KEYWORDS.to_vec(),
            }),
        }
    }

    fn declaration(&mut self) -> ParseResult<Declaration> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: DECLARATION_KEYWORDS.to_vec(),
            });
        };

        match token.data() {
            Token::Keyword(Keyword::Proc) => self.procedure(),
            Token::Keyword(Keyword::Variant) => self.variant(),
            _ => Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: DECLARATION_KEYWORDS.to_vec(),
            }),
        }
    }

    pub fn module(&mut self) -> ParseResult<Vec<Declaration>> {
        let mut declarations = vec![];
        while let Some(_) = self.peek()? {
            declarations.push(self.declaration()?);
        }
        Ok(declarations)
    }

    fn procedure(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Punctuation(Punctuation::LeftParenthesis))?;
        let mut arguments = vec![];
        let mut first = true;
        loop {
            match self.peek()? {
                Some(token) => {
                    if token.data() == &Token::Punctuation(Punctuation::RightParenthesis) {
                        self.advance()?;
                        break;
                    } else {
                        if first {
                            first = false;
                        } else {
                            self.expect(Token::Punctuation(Punctuation::Comma))?;
                        }
                        arguments.push(self.typed_identifier()?);
                    }
                }
                None => {
                    return Err(ParseError::UnexpectedEOF {
                        expecteds: vec![Token::Punctuation(Punctuation::RightParenthesis)],
                    })
                }
            }
        }

        self.expect(Token::Punctuation(Punctuation::LeftCurly))?;
        let mut body = vec![];
        loop {
            match self.peek()? {
                Some(token) => {
                    if token.data() == &Token::Punctuation(Punctuation::RightCurly) {
                        self.advance()?;
                        break;
                    } else {
                        body.push(self.statement()?);
                        self.expect(Token::Punctuation(Punctuation::Semicolon))?;
                    }
                }
                None => {
                    return Err(ParseError::UnexpectedEOF {
                        expecteds: vec![Token::Punctuation(Punctuation::RightCurly)],
                    })
                }
            }
        }

        Ok(Declaration::Procedure {
            name,
            arguments,
            body,
        })
    }

    fn variant(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Punctuation(Punctuation::LeftCurly))?;
        let mut cases = vec![];
        loop {
            match self.peek()? {
                Some(token) => {
                    if token.data() == &Token::Punctuation(Punctuation::RightCurly) {
                        self.advance()?;
                        break;
                    } else {
                        cases.push(self.variant_case()?);
                    }
                }
                None => {
                    return Err(ParseError::UnexpectedEOF {
                        expecteds: vec![Token::Punctuation(Punctuation::RightCurly)],
                    })
                }
            }
        }

        Ok(Declaration::Variant { name, cases })
    }

    fn type_expression(&mut self) -> ParseResult<Located<TypeExpression>> {
        self.type_expression_primary()
    }

    fn type_expression_primary(&mut self) -> ParseResult<Located<TypeExpression>> {
        let Some(token) = self.advance()? else {
            return Err(ParseError::UnexpectedEOF {
                expecteds: PRIMARY_TOKEN_STARTS.to_vec(),
            });
        };

        match token.data() {
            Token::Identifier(intern_idx) => {
                let type_expression = TypeExpression::Identifier(*intern_idx);
                Ok(Located::new(type_expression, token.location()))
            }
            _ => Err(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }),
        }
    }

    fn typed_identifier(&mut self) -> ParseResult<Located<TypedIdentifier>> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::Punctuation(Punctuation::Colon))?;
        let type_expression = self.type_expression()?;

        let location = identifier.location().extend(&type_expression.location());
        let typed_identifier = TypedIdentifier::new(identifier, type_expression);
        Ok(Located::new(typed_identifier, location))
    }

    fn variant_case(&mut self) -> ParseResult<Located<VariantCase>> {
        let identifier = self.expect_identifier()?;
        let token = self.expect_one_of(&[
            Token::Punctuation(Punctuation::Semicolon),
            Token::Punctuation(Punctuation::LeftCurly),
        ])?;

        let (arguments, location) = match token.data() {
            Token::Punctuation(Punctuation::Semicolon) => (None, token.location()),
            Token::Punctuation(Punctuation::LeftCurly) => {
                let mut arguments = vec![];
                let location = loop {
                    match self.peek()? {
                        Some(token) => {
                            if token.data() == &Token::Punctuation(Punctuation::RightCurly) {
                                self.advance()?;
                                self.expect(Token::Punctuation(Punctuation::Semicolon))?;
                                break token.location();
                            } else {
                                arguments.push(self.typed_identifier()?);
                                self.expect(Token::Punctuation(Punctuation::Semicolon))?;
                            }
                        }
                        None => {
                            return Err(ParseError::UnexpectedEOF {
                                expecteds: vec![Token::Punctuation(Punctuation::RightCurly)],
                            })
                        }
                    }
                };
                (Some(arguments), location)
            }
            _ => unreachable!(),
        };

        let location = identifier.location().extend(&location);
        let variant_case = VariantCase::new(identifier, arguments);
        Ok(Located::new(variant_case, location))
    }
}

pub enum ParseError {
    UnexpectedToken {
        unexpected: Located<Token>,
        expected_ones: Vec<Token>,
    },
    UnexpectedEOF {
        expecteds: Vec<Token>,
    },
    LexError(LexError),
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken {
                unexpected,
                expected_ones: expecteds,
            } => {
                write!(f, "Encountered `{unexpected}` but ")?;
                match &expecteds[..] {
                    [] => unreachable!(),
                    [expected] => write!(f, "expected `{}`", expected),
                    _ => {
                        write!(f, "expected one of ")?;
                        for expected in expecteds {
                            write!(f, "`{}` ", expected)?;
                        }
                        Ok(())
                    }
                }
            }
            ParseError::UnexpectedEOF { expecteds } => {
                write!(f, "Encountered end of file but ")?;
                match &expecteds[..] {
                    [] => unreachable!(),
                    [expected] => write!(f, "expected `{}`", expected),
                    _ => {
                        write!(f, "expected one of ")?;
                        for expected in expecteds {
                            write!(f, "`{}`", expected)?;
                        }
                        Ok(())
                    }
                }
            }
            ParseError::LexError(lex_error) => lex_error.fmt(f),
        }
    }
}

type ParseResult<T> = Result<T, ParseError>;
