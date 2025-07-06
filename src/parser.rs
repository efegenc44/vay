use std::{fmt::Display, iter::Peekable, vec};

use crate::{
    bound::Bound, declaration::{Declaration, Method, TypedIdentifier, VariantCase}, error::Error, expression::{Expression, TypeExpression}, interner::InternIdx, lexer::{LexError, Lexer}, location::{Located, SourceLocation}, statement::Statement, token::Token
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[Token::dummy_identifier()];

const STATEMENT_KEYWORDS: &[Token] = &[Token::dummy_identifier()];

const DECLARATION_KEYWORDS: &[Token] = &[
    Token::ModuleKeyword,
    Token::ProcKeyword,
    Token::VariantKeyword,
    Token::ImportKeyword,
];

pub struct Parser<'source, 'interner> {
    tokens: Peekable<Lexer<'source, 'interner>>,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn new(lexer: Lexer<'source, 'interner>) -> Self {
        Self {
            tokens: lexer.peekable(),
        }
    }

    fn peek(&mut self) -> ParseResult<Option<Located<Token>>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(*token)),
            Some(Err(lex_error)) => Err(Located::new(
                ParseError::LexError(lex_error.data().clone()),
                lex_error.location()
            )),
            None => Ok(None),
        }
    }

    fn advance(&mut self) -> ParseResult<Option<Located<Token>>> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(lex_error)) => Err(Located::new(
                ParseError::LexError(lex_error.data().clone()),
                lex_error.location()
            )),
            None => Ok(None),
        }
    }

    fn expect_one_of(&mut self, expected_ones: &[Token]) -> ParseResult<Located<Token>> {
        let Some(token) = self.advance()? else {
            // TODO: absence errors
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: expected_ones.to_vec(),
            }, SourceLocation::dummy()));
        };

        let result = expected_ones
            .iter()
            .find(|expected| expected == &token.data());
        match result {
            Some(_) => Ok(token),
            None => Err(Located::new(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: expected_ones.to_vec(),
            }, token.location())),
        }
    }

    fn expect(&mut self, expected: Token) -> ParseResult<Located<Token>> {
        self.expect_one_of(&[expected])
    }

    fn expect_identifier(&mut self) -> ParseResult<Located<InternIdx>> {
        // TODO: absence errors
        let Some(token) = self.advance()? else {
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: vec![Token::dummy_identifier()],
            }, SourceLocation::dummy()));
        };

        if let Token::Identifier(intern_idx) = token.data() {
            Ok(Located::new(*intern_idx, token.location()))
        } else {
            Err(Located::new(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: vec![Token::dummy_identifier()],
            }, token.location()))
        }
    }

    fn terminator(&mut self, optinal: Token) -> ParseResult<Option<Located<Token>>> {
        let option = match self.peek()? {
            Some(token) if token.data() == &optinal => Some(self.advance()?.unwrap()),
            Some(_) => None,
            None => {
                // TODO: absence errors
                return Err(Located::new(ParseError::UnexpectedEOF {
                    expected_ones: vec![optinal],
                }, SourceLocation::dummy()))
            }
        };

        Ok(option)
    }

    fn expression(&mut self) -> ParseResult<Located<Expression>> {
        self.primary()
    }

    fn primary(&mut self) -> ParseResult<Located<Expression>> {
        let Some(token) = self.advance()? else {
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }, SourceLocation::dummy()));
        };

        match token.data() {
            Token::Identifier(intern_idx) => {
                let mut path = vec![*intern_idx];
                let mut end = token.location();
                while let Some(Token::DoubleColon) = self.peek()?.map(|token| *token.data()) {
                    self.advance()?;
                    let identifier = self.expect_identifier()?;
                    path.push(*identifier.data());
                    end = identifier.location();
                }

                let expression = Expression::Path(path, Bound::Undetermined);
                Ok(Located::new(expression, token.location().extend(&end)))
            }
            _ => Err(Located::new(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }, token.location())),
        }
    }

    fn statement(&mut self) -> ParseResult<Located<Statement>> {
        let Some(token) = self.peek()? else {
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: STATEMENT_KEYWORDS.to_vec(),
            }, SourceLocation::dummy()));
        };

        match token.data() {
            _ => {
                let expression = self.expression().map_err(|_| Located::new(ParseError::UnexpectedToken {
                    unexpected: token,
                    expected_ones: STATEMENT_KEYWORDS.to_vec(),
                }, token.location()))?;

                let location = expression.location();
                let statement = Statement::Expression(expression);
                Ok(Located::new(statement, location))
            }
        }
    }

    fn declaration(&mut self) -> ParseResult<Declaration> {
        let Some(token) = self.advance()? else {
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: DECLARATION_KEYWORDS.to_vec(),
            }, SourceLocation::dummy()));
        };

        match token.data() {
            Token::ModuleKeyword => self.module(),
            Token::ImportKeyword => self.import(),
            Token::ProcKeyword => self.procedure(),
            Token::VariantKeyword => self.variant(),
            _ => Err(Located::new(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: DECLARATION_KEYWORDS.to_vec(),
            }, token.location())),
        }
    }

    pub fn program(&mut self) -> ParseResult<Vec<Declaration>> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(declarations)
    }

    fn module(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Module { name })
    }

    fn import(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Import { name })
    }

    fn procedure(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;
        let mut arguments = vec![];
        let mut first = true;
        loop {
            match self.terminator(Token::RightParenthesis)? {
                Some(_) => break,
                None => {
                    if first {
                        first = false;
                    } else {
                        self.expect(Token::Comma)?;
                    }
                    arguments.push(self.typed_identifier()?);
                }
            }
        }

        self.expect(Token::LeftCurly)?;
        let mut body = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    body.push(self.statement()?);
                    self.expect(Token::Semicolon)?;
                }
            }
        }

        Ok(Declaration::Procedure {
            name,
            arguments,
            body,
        })
    }

    fn method(&mut self) -> ParseResult<Method> {
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;
        let mut arguments = vec![];
        let mut first = true;
        loop {
            match self.terminator(Token::RightParenthesis)? {
                Some(_) => break,
                None => {
                    if first {
                        first = false;
                    } else {
                        self.expect(Token::Comma)?;
                    }
                    arguments.push(self.typed_identifier()?);
                }
            }
        }

        self.expect(Token::LeftCurly)?;
        let mut body = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    body.push(self.statement()?);
                    self.expect(Token::Semicolon)?;
                }
            }
        }

        Ok(Method::new(name, arguments, body))
    }

    fn variant(&mut self) -> ParseResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::LeftCurly)?;
        let mut cases = vec![];
        let mut methods = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    if let Some(Token::ProcKeyword) = self.peek()?.map(|token| *token.data()) {
                        self.advance()?;
                        methods.push(self.method()?);
                    } else {
                        cases.push(self.variant_case()?)
                    }
                },
            }
        }

        Ok(Declaration::Variant { name, cases, methods })
    }

    fn type_expression(&mut self) -> ParseResult<Located<TypeExpression>> {
        self.type_expression_primary()
    }

    fn type_expression_primary(&mut self) -> ParseResult<Located<TypeExpression>> {
        let Some(token) = self.advance()? else {
            return Err(Located::new(ParseError::UnexpectedEOF {
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }, SourceLocation::dummy()));
        };

        match token.data() {
            Token::Identifier(intern_idx) => {
                let mut path = vec![*intern_idx];
                let mut end = token.location();
                while let Some(Token::DoubleColon) = self.peek()?.map(|token| *token.data()) {
                    self.advance()?;
                    let idenifier = self.expect_identifier()?;
                    path.push(*idenifier.data());
                    end = idenifier.location()
                }

                let type_expression = TypeExpression::Path(path, Bound::Undetermined);
                Ok(Located::new(type_expression, token.location().extend(&end)))
            }
            _ => Err(Located::new(ParseError::UnexpectedToken {
                unexpected: token,
                expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
            }, token.location())),
        }
    }

    fn typed_identifier(&mut self) -> ParseResult<Located<TypedIdentifier>> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::Colon)?;
        let type_expression = self.type_expression()?;

        let location = identifier.location().extend(&type_expression.location());
        let typed_identifier = TypedIdentifier::new(identifier, type_expression);
        Ok(Located::new(typed_identifier, location))
    }

    fn variant_case(&mut self) -> ParseResult<Located<VariantCase>> {
        let identifier = self.expect_identifier()?;
        let token = self.expect_one_of(&[Token::Semicolon, Token::LeftCurly])?;

        let (arguments, location) = match token.data() {
            Token::Semicolon => (None, token.location()),
            Token::LeftCurly => {
                let mut arguments = vec![];
                let location = loop {
                    match self.terminator(Token::RightCurly)? {
                        Some(token) => {
                            self.expect(Token::Semicolon)?;
                            break token.location();
                        }
                        None => {
                            arguments.push(self.typed_identifier()?);
                            self.expect(Token::Semicolon)?;
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
        expected_ones: Vec<Token>,
    },
    LexError(LexError),
}

impl Error for Located<ParseError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self) -> String {
        self.data().to_string()
    }
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
            ParseError::UnexpectedEOF {
                expected_ones: expecteds,
            } => {
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

type ParseResult<T> = Result<T, Located<ParseError>>;
