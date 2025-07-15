use std::iter::Peekable;

use crate::{
    bound::{Bound, Path},
    declaration::{
        Declaration, ImportDeclaration, MethodDeclaration, Module, ModuleDeclaration,
        ProcedureDeclaration, TypedIdentifier, VariantCase, VariantDeclaration,
    },
    expression::{Expression, TypeExpression},
    interner::{InternIdx, Interner},
    lexer::Lexer,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchBranch, Pattern, Statement},
    token::Token,
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[Token::dummy_identifier()];

const STATEMENT_KEYWORDS: &[Token] = &[
    Token::MatchKeyword,
    Token::ReturnKeyword,
    Token::dummy_identifier(),
];

const DECLARATION_KEYWORDS: &[Token] = &[
    Token::ModuleKeyword,
    Token::ProcKeyword,
    Token::VariantKeyword,
    Token::ImportKeyword,
];

const PATTERN_TOKEN_STARTS: &[Token] = &[Token::dummy_identifier()];

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
        self.application()
    }

    fn application(&mut self) -> ReportableResult<Located<Expression>> {
        let mut expression = self.primary()?;
        loop {
            match self.peek()?.map(|token| *token.data()) {
                Some(Token::LeftParenthesis) => {
                    self.advance()?;
                    let mut arguments = vec![];
                    let mut first = true;
                    let end = loop {
                        match self.terminator(Token::RightParenthesis)? {
                            Some(token) => break token.location(),
                            None => {
                                if first {
                                    first = false;
                                } else {
                                    self.expect(Token::Comma)?;
                                }
                                arguments.push(self.expression()?);
                            }
                        }
                    };

                    let location = expression.location().extend(&end);
                    let application = Expression::Application {
                        function: Box::new(expression),
                        arguments,
                    };
                    expression = Located::new(application, location);
                }
                Some(Token::Dot) => {
                    self.advance()?;
                    let name = self.expect_identifier()?;

                    let location = expression.location().extend(&name.location());
                    let projection = Expression::Projection {
                        expression: Box::new(expression),
                        name,
                    };
                    expression = Located::new(projection, location)
                }
                _ => break,
            }
        }

        Ok(expression)
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
            Token::ReturnKeyword => self.retrn(),
            Token::MatchKeyword => self.matc(),
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

    fn matc(&mut self) -> ReportableResult<Located<Statement>> {
        let start = self.expect(Token::MatchKeyword)?.location();
        let expression = self.expression()?;

        self.expect(Token::LeftCurly)?;
        let mut branches = vec![];
        let end = loop {
            match self.terminator(Token::RightCurly)? {
                Some(token) => break token.location(),
                None => {
                    branches.push(self.match_branch()?);
                }
            }
        };

        let location = start.extend(&end);
        Ok(Located::new(
            Statement::Match {
                expression,
                branches,
            },
            location,
        ))
    }

    fn match_branch(&mut self) -> ReportableResult<Located<MatchBranch>> {
        let pattern = self.pattern()?;
        self.expect(Token::Colon)?;
        let statement = self.statement()?;

        let location = pattern.location().extend(&statement.location());
        Ok(Located::new(MatchBranch { pattern, statement }, location))
    }

    fn pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: PATTERN_TOKEN_STARTS.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        match token.data() {
            Token::Identifier(_) => self.varint_case_pattern(),
            _ => self.error(
                ParseError::UnexpectedToken {
                    unexpected: *token.data(),
                    expected_ones: PRIMARY_TOKEN_STARTS.to_vec(),
                },
                token.location(),
            ),
        }
    }

    fn varint_case_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let identifier = self.expect_identifier()?;

        let (fields, location) =
            if let Some(Token::LeftParenthesis) = self.peek()?.map(|token| *token.data()) {
                self.advance()?;
                let mut fields = vec![];
                let mut first = true;
                let end = loop {
                    match self.terminator(Token::RightParenthesis)? {
                        Some(token) => break token.location(),
                        None => {
                            if first {
                                first = false;
                            } else {
                                self.expect(Token::Comma)?;
                            }
                            fields.push(self.expect_identifier()?);
                        }
                    }
                };

                (Some(fields), identifier.location().extend(&end))
            } else {
                (None, identifier.location())
            };

        Ok(Located::new(
            Pattern::VariantCase {
                name: identifier,
                fields,
            },
            location,
        ))
    }

    fn retrn(&mut self) -> ReportableResult<Located<Statement>> {
        let token = self.expect(Token::ReturnKeyword)?;
        let expression = self.expression()?;

        let location = token.location().extend(&expression.location());
        Ok(Located::new(Statement::Return(expression), location))
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

        let declaration = match token.data() {
            Token::ModuleKeyword => Declaration::Module(self.module_declaration()?),
            Token::ImportKeyword => Declaration::Import(self.import_declaration()?),
            Token::ProcKeyword => Declaration::Procedure(self.procedure_declaration()?),
            Token::VariantKeyword => Declaration::Variant(self.variant_declaration()?),
            _ => {
                return self.error(
                    ParseError::UnexpectedToken {
                        unexpected: *token.data(),
                        expected_ones: DECLARATION_KEYWORDS.to_vec(),
                    },
                    token.location(),
                )
            }
        };

        Ok(declaration)
    }

    pub fn module(&mut self) -> ReportableResult<Module> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(Module::new(declarations, self.source.clone()))
    }

    fn module_declaration(&mut self) -> ReportableResult<ModuleDeclaration> {
        self.expect(Token::ModuleKeyword)?;
        let name = self.expect_identifier()?;

        Ok(ModuleDeclaration { name })
    }

    fn import_declaration(&mut self) -> ReportableResult<ImportDeclaration> {
        self.expect(Token::ImportKeyword)?;
        let name = self.expect_identifier()?;

        Ok(ImportDeclaration { name })
    }

    fn procedure_declaration(&mut self) -> ReportableResult<ProcedureDeclaration> {
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

        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;

        self.expect(Token::LeftCurly)?;
        let mut body = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    body.push(self.statement()?);
                }
            }
        }

        Ok(ProcedureDeclaration {
            name,
            arguments,
            return_type,
            body,
            path: Path::empty(),
        })
    }

    fn method_declaration(&mut self) -> ReportableResult<MethodDeclaration> {
        self.expect(Token::ProcKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;

        // TODO: Better error message
        let this = self.expect_identifier()?;

        let mut arguments = vec![];
        loop {
            match self.terminator(Token::RightParenthesis)? {
                Some(_) => break,
                None => {
                    // TODO: Maybe better error message when
                    //   right after the first identifier
                    self.expect(Token::Comma)?;
                    arguments.push(self.typed_identifier()?);
                }
            }
        }

        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;

        self.expect(Token::LeftCurly)?;
        let mut body = vec![];
        loop {
            match self.terminator(Token::RightCurly)? {
                Some(_) => break,
                None => {
                    body.push(self.statement()?);
                }
            }
        }

        Ok(MethodDeclaration {
            name,
            self_reference: this,
            arguments,
            return_type,
            body,
        })
    }

    fn variant_declaration(&mut self) -> ReportableResult<VariantDeclaration> {
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
                        methods.push(self.method_declaration()?);
                    } else {
                        cases.push(self.variant_case()?)
                    }
                }
            }
        }

        Ok(VariantDeclaration {
            name,
            cases,
            methods,
            path: Path::empty(),
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
            Token::ProcKeyword => self.type_procedure(),
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

    fn type_procedure(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let start = self.expect(Token::ProcKeyword)?.location();
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
                    arguments.push(self.type_expression()?);
                }
            }
        }

        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;

        let location = start.extend(&return_type.location());
        Ok(Located::new(
            TypeExpression::Procedure {
                arguments,
                return_type: Box::new(return_type),
            },
            location,
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
        let (arguments, location) =
            if let Some(Token::LeftParenthesis) = self.peek()?.map(|token| *token.data()) {
                self.advance()?;
                let mut arguments = vec![];
                let mut first = true;
                let end = loop {
                    match self.terminator(Token::RightParenthesis)? {
                        Some(token) => break token.location(),
                        None => {
                            if first {
                                first = false;
                            } else {
                                self.expect(Token::Comma)?;
                            }
                            arguments.push(self.type_expression()?);
                        }
                    }
                };
                (Some(arguments), identifier.location().extend(&end))
            } else {
                (None, identifier.location())
            };

        let variant_case = VariantCase::new(identifier, arguments, Path::empty());
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
