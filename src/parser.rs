use std::iter::Peekable;

use crate::{
    bound::Bound,
    declaration::{Declaration, Method, Module, TypedIdentifier, VariantCase},
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

pub struct Parser<'source_content, 'interner> {
    tokens: Peekable<Lexer<'source_content, 'interner>>,
    source: String,
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn new(lexer: Lexer<'source, 'interner>) -> Self {
        let source = lexer.source().to_string();
        Self {
            tokens: lexer.peekable(),
            source,
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
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: expected_ones.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        let result = expected_ones
            .iter()
            .find(|expected| expected == &token.data());

        match result {
            Some(_) => Ok(token),
            None => self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: expected_ones.to_vec(),
                },
                token.location(),
            ),
        }
    }

    fn expect(&mut self, expected: Token) -> ReportableResult<Located<Token>> {
        self.expect_one_of(&[expected])
    }

    fn expect_identifier(&mut self) -> ReportableResult<Located<InternIdx>> {
        let Some(token) = self.advance()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: vec![Token::dummy_identifier()],
                },
                SourceLocation::dummy(),
            );
        };

        if let Token::Identifier(intern_idx) = token.data() {
            Ok(Located::new(*intern_idx, token.location()))
        } else {
            self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: vec![Token::dummy_identifier()],
                },
                token.location(),
            )
        }
    }

    fn terminator(&mut self, terminator: Token) -> ReportableResult<Option<Located<Token>>> {
        match self.peek()? {
            Some(token) if token.data() == &terminator => Ok(Some(self.advance()?.unwrap())),
            Some(_) => Ok(None),
            None => self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: vec![terminator],
                },
                SourceLocation::dummy(),
            ),
        }
    }

    fn expression(&mut self) -> ReportableResult<Located<Expression>> {
        self.primary()
    }

    fn primary(&mut self) -> ReportableResult<Located<Expression>> {
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        match token.data() {
            Token::Identifier(_) => self.path(),
            _ => self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
                },
                token.location(),
            ),
        }
    }

    fn path(&mut self) -> ReportableResult<Located<Expression>> {
        let identifier = self.expect_identifier()?;
        let mut path = vec![*identifier.data()];
        let mut end = identifier.location();
        while let Some(Token::DoubleColon) = self.peek()?.map(|token| *token.data()) {
            self.advance()?;
            let identifier = self.expect_identifier()?;
            path.push(*identifier.data());
            end = identifier.location();
        }

        let expression = Expression::Path(path, Bound::Undetermined);
        Ok(Located::new(expression, identifier.location().extend(&end)))
    }

    fn statement(&mut self) -> ReportableResult<Located<Statement>> {
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: STATEMENT_KEYWORDS.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        match token.data() {
            _ => {
                let Ok(expression) = self.expression() else {
                    return self.error(
                        ParseError::UnexpectedToken {
                            unexpected: *token.data(),
                            expected_ones: STATEMENT_KEYWORDS.to_vec(),
                        },
                        token.location(),
                    );
                };

                let location = expression.location();
                let statement = Statement::Expression(expression);
                Ok(Located::new(statement, location))
            }
        }
    }

    fn declaration(&mut self) -> ReportableResult<Declaration> {
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: DECLARATION_KEYWORDS.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        match token.data() {
            Token::ModuleKeyword => self.modul(),
            Token::ImportKeyword => self.import(),
            Token::ProcKeyword => self.procedure(),
            Token::VariantKeyword => self.variant(),
            _ => self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: DECLARATION_KEYWORDS.to_vec(),
                },
                token.location(),
            ),
        }
    }

    pub fn module(&mut self) -> ReportableResult<Module> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(Module::new(declarations, self.source.clone()))
    }

    // TODO: Rename
    fn modul(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ModuleKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Module { name })
    }

    fn import(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ImportKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Semicolon)?;

        Ok(Declaration::Import { name })
    }

    fn procedure(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ProcKeyword)?;
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
        self.expect(Token::ProcKeyword)?;
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
        self.expect(Token::VariantKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftCurly)?;
        let mut cases = vec![];
        let mut methods = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    if let Some(Token::ProcKeyword) = self.peek()?.map(|token| *token.data()) {
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
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        match token.data() {
            Token::Identifier(_) => self.type_path(),
            _ => self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
                },
                token.location(),
            ),
        }
    }

    fn type_path(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let identifier = self.expect_identifier()?;
        let mut path = vec![*identifier.data()];
        let mut end = identifier.location();
        while let Some(Token::DoubleColon) = self.peek()?.map(|token| *token.data()) {
            self.advance()?;
            let idenifier = self.expect_identifier()?;
            path.push(*idenifier.data());
            end = idenifier.location()
        }

        let type_expression = TypeExpression::Path(path, Bound::Undetermined);
        Ok(Located::new(
            type_expression,
            identifier.location().extend(&end),
        ))
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

    fn error<T>(&self, error: ParseError, location: SourceLocation) -> ReportableResult<T> {
        let reportable = (Located::new(error, location), self.source.clone());
        Err(Box::new(reportable))
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

impl Reportable for (Located<ParseError>, String) {
    fn location(&self) -> SourceLocation {
        self.0.location()
    }

    fn source(&self) -> &str {
        &self.1
    }

    fn description(&self, _intener: &Interner) -> String {
        match self.0.data() {
            ParseError::UnexpectedToken {
                unexpected,
                expected_ones,
            } => {
                let mut description = format!("Encountered {} but ", unexpected.kind_name());
                match &expected_ones[..] {
                    [] => unreachable!(),
                    [expected] => {
                        description.push_str(&format!("expected {}.", expected.kind_name()))
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
                        description.push_str(&format!("expected {}  .", expected.kind_name()))
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
