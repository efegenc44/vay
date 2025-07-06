use std::iter::Peekable;

use crate::{
    bound::Bound,
    declaration::{Declaration, Method, TypedIdentifier, VariantCase},
    expression::{Expression, TypeExpression},
    interner::{InternIdx, Interner},
    lexer::Lexer,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::Statement,
    token::Token,
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

    fn peek(&mut self) -> ReportableResult<Option<Located<Token>>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(*token)),
            // TODO: fix here, hacky
            Some(Err(_)) => self.advance(),
            None => Ok(None),
        }
    }

    fn advance(&mut self) -> ReportableResult<Option<Located<Token>>> {
        match self.tokens.next() {
            Some(Ok(token)) => Ok(Some(token)),
            Some(Err(lex_error)) => Err(lex_error),
            None => Ok(None),
        }
    }

    fn expect_one_of(&mut self, expected_ones: &[Token]) -> ReportableResult<Located<Token>> {
        let Some(token) = self.advance()? else {
            return Self::unexpected_eof(expected_ones.to_vec());
        };

        let result = expected_ones
            .iter()
            .find(|expected| expected == &token.data());
        match result {
            Some(_) => Ok(token),
            None => Self::unexpected_token(token, expected_ones.to_vec()),
        }
    }

    fn expect(&mut self, expected: Token) -> ReportableResult<Located<Token>> {
        self.expect_one_of(&[expected])
    }

    fn expect_identifier(&mut self) -> ReportableResult<Located<InternIdx>> {
        let Some(token) = self.advance()? else {
            return Self::unexpected_eof(vec![Token::dummy_identifier()]);
        };

        if let Token::Identifier(intern_idx) = token.data() {
            Ok(Located::new(*intern_idx, token.location()))
        } else {
            Self::unexpected_token(token, vec![Token::dummy_identifier()])
        }
    }

    fn terminator(&mut self, terminator: Token) -> ReportableResult<Option<Located<Token>>> {
        match self.peek()? {
            Some(token) if token.data() == &terminator => Ok(Some(self.advance()?.unwrap())),
            Some(_) => Ok(None),
            None => Self::unexpected_eof(vec![terminator]),
        }
    }

    fn expression(&mut self) -> ReportableResult<Located<Expression>> {
        self.primary()
    }

    fn primary(&mut self) -> ReportableResult<Located<Expression>> {
        let Some(token) = self.advance()? else {
            return Self::unexpected_eof(PRIMARY_TOKEN_STARTS.to_vec());
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
            _ => Self::unexpected_token(token, PRIMARY_TOKEN_STARTS.to_vec()),
        }
    }

    fn statement(&mut self) -> ReportableResult<Located<Statement>> {
        let Some(token) = self.peek()? else {
            return Self::unexpected_eof(STATEMENT_KEYWORDS.to_vec());
        };

        match token.data() {
            _ => {
                let Ok(expression) = self.expression() else {
                    return Self::unexpected_token(token, STATEMENT_KEYWORDS.to_vec());
                };

                let location = expression.location();
                let statement = Statement::Expression(expression);
                Ok(Located::new(statement, location))
            }
        }
    }

    fn declaration(&mut self) -> ReportableResult<Declaration> {
        let Some(token) = self.advance()? else {
            return Self::unexpected_eof(DECLARATION_KEYWORDS.to_vec());
        };

        match token.data() {
            Token::ModuleKeyword => self.module(),
            Token::ImportKeyword => self.import(),
            Token::ProcKeyword => self.procedure(),
            Token::VariantKeyword => self.variant(),
            _ => Self::unexpected_token(token, DECLARATION_KEYWORDS.to_vec()),
        }
    }

    pub fn program(&mut self) -> ReportableResult<Vec<Declaration>> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(declarations)
    }

    fn module(&mut self) -> ReportableResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Module { name })
    }

    fn import(&mut self) -> ReportableResult<Declaration> {
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Import { name })
    }

    fn procedure(&mut self) -> ReportableResult<Declaration> {
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

    fn method(&mut self) -> ReportableResult<Method> {
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

    fn variant(&mut self) -> ReportableResult<Declaration> {
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
                }
            }
        }

        Ok(Declaration::Variant {
            name,
            cases,
            methods,
        })
    }

    fn type_expression(&mut self) -> ReportableResult<Located<TypeExpression>> {
        self.type_expression_primary()
    }

    fn type_expression_primary(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let Some(token) = self.advance()? else {
            return Self::unexpected_eof(PRIMARY_TOKEN_STARTS.to_vec());
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
            _ => Self::unexpected_token(token, PRIMARY_TOKEN_STARTS.to_vec()),
        }
    }

    fn typed_identifier(&mut self) -> ReportableResult<Located<TypedIdentifier>> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::Colon)?;
        let type_expression = self.type_expression()?;

        let location = identifier.location().extend(&type_expression.location());
        let typed_identifier = TypedIdentifier::new(identifier, type_expression);
        Ok(Located::new(typed_identifier, location))
    }

    fn variant_case(&mut self) -> ReportableResult<Located<VariantCase>> {
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

    fn unexpected_token<T>(
        token: Located<Token>,
        expected_ones: Vec<Token>,
    ) -> ReportableResult<T> {
        let parse_error = ParseError::UnexpectedToken {
            unexpected: *token.data(),
            expected_ones,
        };

        Err(Box::new(Located::new(parse_error, token.location())))
    }

    fn unexpected_eof<T>(expected_ones: Vec<Token>) -> ReportableResult<T> {
        let parse_error = ParseError::UnexpectedEOF { expected_ones };

        Err(Box::new(Located::new(parse_error, SourceLocation::dummy())))
    }
}

pub enum ParseError {
    UnexpectedToken {
        unexpected: Token,
        expected_ones: Vec<Token>,
    },
    UnexpectedEOF {
        expected_ones: Vec<Token>,
    },
}

impl Reportable for Located<ParseError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self, _intener: &Interner) -> String {
        match self.data() {
            ParseError::UnexpectedToken {
                unexpected,
                expected_ones,
            } => {
                let mut description = format!("Encountered {} but ", unexpected.kind_name());
                match &expected_ones[..] {
                    [] => unreachable!(),
                    [expected] => {
                        description.push_str(&format!("expected {}", expected.kind_name()))
                    }
                    [init @ .., last] => {
                        description.push_str("expected either ");
                        for expected in init {
                            description.push_str(&format!("{}, ", expected.kind_name()));
                        }
                        description.push_str(&format!("or {}.", last.kind_name()));
                    }
                };
                description
            }
            ParseError::UnexpectedEOF { expected_ones } => {
                let mut description = String::from("Encountered end of file but ");
                match &expected_ones[..] {
                    [] => unreachable!(),
                    [expected] => {
                        description.push_str(&format!("expected {}", expected.kind_name()))
                    }
                    [init @ .., last] => {
                        description.push_str("expected either ");
                        for expected in init {
                            description.push_str(&format!("{}, ", expected.kind_name()));
                        }
                        description.push_str(&format!("or {}.", last.kind_name()));
                    }
                };
                description
            }
        }
    }
}
