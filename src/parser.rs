use std::iter::Peekable;

use crate::{
    bound::{Bound, Path},
    declaration::{
        Declaration, ImportDeclaration, InterfaceDeclaration, MethodDeclaration, MethodSignature, Module, ModuleDeclaration, ProcedureDeclaration, TypeVar, TypedIdentifier, VariantCase, VariantDeclaration
    },
    expression::{
        ApplicationExpression, Expression, PathExpression, PathTypeExpression, ProcedureTypeExpression, ProjectionExpression, TypeApplicationExpression, TypeExpression
    },
    interner::{InternIdx, Interner},
    lexer::Lexer,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchBranch, MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern},
    token::Token,
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[Token::dummy_identifier()];

const PRIMARY_TYPE_TOKEN_STARTS: &[Token] = &[
    Token::ProcKeyword,
    Token::dummy_identifier()
];

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
    Token::InterfaceKeyword,
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

    fn peek_one_of(&mut self, expected_ones: &[Token]) -> ReportableResult<Located<Token>> {
        let Some(token) = self.peek()? else {
            return self.error(
                ParseError::UnexpectedEOF {
                    expected_ones: expected_ones.to_vec(),
                },
                SourceLocation::dummy(),
            );
        };

        let result = expected_ones
            .iter()
            .find(|expected| {
                if matches!(expected, Token::Identifier(_)) {
                    matches!(token.data(), Token::Identifier(_))
                } else {
                    expected == &token.data()
                }
            });

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

    fn peek_is(&mut self, expected: Token) -> ReportableResult<bool> {
        Ok(self.peek_one_of(&[expected]).is_ok())
    }

    fn expect_one_of(&mut self, expected_ones: &[Token]) -> ReportableResult<Located<Token>> {
        let token = self.peek_one_of(expected_ones)?;
        self.advance()?;
        Ok(token)
    }

    fn expect(&mut self, expected: Token) -> ReportableResult<Located<Token>> {
        self.expect_one_of(&[expected])
    }

    fn expect_identifier(&mut self) -> ReportableResult<Located<InternIdx>> {
        let token = self.expect(Token::dummy_identifier())?;
        let Token::Identifier(intern_idx) = token.data() else {
            unreachable!();
        };

        Ok(Located::new(*intern_idx, token.location()))
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

    fn until<T, F>(&mut self, terminator: Token, f: F, seperator: Option<Token>) -> ReportableResult<(Vec<T>, SourceLocation)>
    where
        F: Fn(&mut Self) -> ReportableResult<T>
    {
        let mut values = vec![];
        let mut first = true;
        let end = loop {
            match self.terminator(terminator)? {
                Some(token) => break token.location(),
                None => {
                    if let Some(seperator) = seperator {
                        if first {
                            first = false;
                        } else {
                            self.expect(seperator)?;
                        }
                    }
                    values.push(f(self)?);
                }
            }
        };

        Ok((values, end))
    }

    fn expression(&mut self) -> ReportableResult<Located<Expression>> {
        self.application()
    }

    fn application(&mut self) -> ReportableResult<Located<Expression>> {
        let mut expression = self.primary()?;
        loop {
            if self.peek_is(Token::LeftParenthesis)? {
                self.advance()?;
                let (arguments, end) = self.until(
                    Token::RightParenthesis,
                    Self::expression,
                    Some(Token::Comma)
                )?;

                let location = expression.location().extend(&end);
                let application = ApplicationExpression {
                    function: Box::new(expression),
                    arguments,
                };
                expression = Located::new(Expression::Application(application), location);
            } else if self.peek_is(Token::Dot)? {
                self.advance()?;
                let name = self.expect_identifier()?;

                let location = expression.location().extend(&name.location());
                let projection = ProjectionExpression {
                    expression: Box::new(expression),
                    name,
                };
                expression = Located::new(Expression::Projection(projection), location);
            } else {
                break;
            }
        }

        Ok(expression)
    }

    fn primary(&mut self) -> ReportableResult<Located<Expression>> {
        let token = self.peek_one_of(PRIMARY_TOKEN_STARTS)?;
        match token.data() {
            Token::Identifier(_) => self.path(),
            _ => unreachable!()
        }
    }

    fn path(&mut self) -> ReportableResult<Located<Expression>> {
        let identifier = self.expect_identifier()?;
        let mut parts = vec![*identifier.data()];
        let mut end = identifier.location();
        while self.peek_is(Token::DoubleColon)? {
            self.advance()?;
            let identifier = self.expect_identifier()?;
            parts.push(*identifier.data());
            end = identifier.location();
        }

        let path = PathExpression { parts, bound: Bound::Undetermined };
        let expression = Expression::Path(path);
        Ok(Located::new(expression, identifier.location().extend(&end)))
    }

    fn statement(&mut self) -> ReportableResult<Located<Statement>> {
        let token = self.peek_one_of(STATEMENT_KEYWORDS)?;
        match token.data() {
            Token::ReturnKeyword => self.return_statement(),
            Token::MatchKeyword => self.matc(),
            _ => {
                let expression = self.expression()?;
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
        let (branches, end) = self.until(Token::RightCurly, Self::match_branch, None)?;
        let location = start.extend(&end);
        let matc = MatchStatement {
            expression,
            branches
        };

        Ok(Located::new(Statement::Match(matc), location))
    }

    fn match_branch(&mut self) -> ReportableResult<Located<MatchBranch>> {
        let pattern = self.pattern()?;
        self.expect(Token::Colon)?;
        let statement = self.statement()?;

        let location = pattern.location().extend(&statement.location());
        Ok(Located::new(MatchBranch::new(pattern, statement), location))
    }

    fn pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let token = self.peek_one_of(PATTERN_TOKEN_STARTS)?;
        match token.data() {
            Token::Identifier(_) => self.varint_case_pattern(),
            _ => unreachable!()
        }
    }

    fn varint_case_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let name = self.expect_identifier()?;
        let (fields, location) = if self.peek_is(Token::LeftParenthesis)? {
            self.advance()?;
            let (fields, end) = self.until(
                Token::RightParenthesis,
                Self::expect_identifier,
                Some(Token::Comma)
            )?;
            (Some(fields), name.location().extend(&end))
        } else {
            (None, name.location())
        };

        let variant_case = VariantCasePattern { name, fields };
        Ok(Located::new(Pattern::VariantCase(variant_case), location))
    }

    fn return_statement(&mut self) -> ReportableResult<Located<Statement>> {
        let token = self.expect(Token::ReturnKeyword)?;
        let expression = self.expression()?;

        let location = token.location().extend(&expression.location());
        let retrn = ReturnStatement { expression };
        Ok(Located::new(Statement::Return(retrn), location))
    }

    fn declaration(&mut self) -> ReportableResult<Declaration> {
        let token = self.peek_one_of(DECLARATION_KEYWORDS)?;
        match token.data() {
            Token::ModuleKeyword => self.modul(),
            Token::ImportKeyword => self.import(),
            Token::ProcKeyword => self.procedure(),
            Token::VariantKeyword => self.variant(),
            Token::InterfaceKeyword => self.interface(),
            _ => unreachable!()
        }
    }

    pub fn module(&mut self) -> ReportableResult<Module> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(Module::new(declarations, self.source.clone()))
    }

    fn type_var(&mut self) -> ReportableResult<Located<TypeVar>> {
        let name = self.expect_identifier()?;
        let (interfaces, end) = if self.peek_is(Token::LeftParenthesis)? {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                |parser| {
                    Ok((parser.expect_identifier()?, Path::empty()))
                },
                Some(Token::Comma)
            )?
        } else {
            (vec![], name.location())
        };

        let type_var = TypeVar { name, interfaces };
        Ok(Located::new(type_var, name.location().extend(&end)))
    }

    fn modul(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ModuleKeyword)?;
        let name = self.expect_identifier()?;

        let module = ModuleDeclaration { name };
        Ok(Declaration::Module(module))
    }

    fn import(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ImportKeyword)?;
        let name = self.expect_identifier()?;

        let import = ImportDeclaration { name };
        Ok(Declaration::Import(import))
    }

    fn procedure(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ProcKeyword)?;
        let name = self.expect_identifier()?;

        let type_vars = if self.peek_is(Token::Colon)? {
            self.advance()?;
            self.expect(Token::LeftParenthesis)?;
            self.until(
                Token::RightParenthesis,
                Self::type_var,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };

        self.expect(Token::LeftParenthesis)?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            Self::typed_identifier,
            Some(Token::Comma)
        )?;
        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;
        self.expect(Token::LeftCurly)?;
        let (body, _) = self.until(Token::RightCurly, Self::statement, None)?;

        let procedure = ProcedureDeclaration {
            name,
            type_vars,
            arguments,
            return_type,
            body,
            path: Path::empty(),
        };

        Ok(Declaration::Procedure(procedure))
    }

    fn method_declaration(&mut self) -> ReportableResult<MethodDeclaration> {
        self.expect(Token::ProcKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;

        // TODO: Better error message
        let instance = self.expect_identifier()?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            |parser| {
                parser.expect(Token::Comma)?;
                parser.typed_identifier()
            },
            None
        )?;

        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;
        self.expect(Token::LeftCurly)?;
        let (body, _) = self.until(Token::RightCurly, Self::statement, None)?;

        Ok(MethodDeclaration {
            name,
            instance,
            arguments,
            return_type,
            body,
        })
    }

    fn variant(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::VariantKeyword)?;
        let name = self.expect_identifier()?;
        let type_vars = if self.peek_is(Token::LeftParenthesis)? {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                Self::type_var,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };
        self.expect(Token::LeftCurly)?;
        let mut cases = vec![];
        let mut methods = vec![];
        while self.terminator(Token::RightCurly)?.is_none() {
            if self.peek_is(Token::ProcKeyword)? {
                methods.push(self.method_declaration()?);
            } else {
                cases.push(self.variant_case()?)
            }
        }

        let variant = VariantDeclaration {
            name,
            type_vars,
            cases,
            methods,
            path: Path::empty(),
        };

        Ok(Declaration::Variant(variant))
    }

    fn method_signature(&mut self) -> ReportableResult<MethodSignature> {
        self.expect(Token::ProcKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            Self::typed_identifier,
            Some(Token::Comma)
        )?;
        self.expect(Token::Colon)?;
        let return_type = self.type_expression()?;

        Ok(MethodSignature { name, arguments, return_type })
    }

    fn interface(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::InterfaceKeyword)?;
        let name = self.expect_identifier()?;
        let type_name = self.expect_identifier()?;
        self.expect(Token::LeftCurly)?;
        let (methods, _) = self.until(Token::RightCurly, Self::method_signature, None)?;


        let interface = InterfaceDeclaration { name, type_name, methods, path: Path::empty() };
        Ok(Declaration::Interface(interface))
    }

    fn type_expression(&mut self) -> ReportableResult<Located<TypeExpression>> {
        self.type_application()
    }

    fn type_application(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let mut expression = self.type_expression_primary()?;
        loop {
            if self.peek_is(Token::LeftParenthesis)? {
                self.advance()?;
                let (arguments, end) = self.until(
                    Token::RightParenthesis,
                    Self::type_expression,
                    Some(Token::Comma)
                )?;

                let location = expression.location().extend(&end);
                let application = TypeApplicationExpression {
                    function: Box::new(expression),
                    arguments,
                };
                expression = Located::new(TypeExpression::Application(application), location);
            } else {
                break;
            }
        }

        Ok(expression)
    }

    fn type_expression_primary(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let token = self.peek_one_of(PRIMARY_TYPE_TOKEN_STARTS)?;
        match token.data() {
            Token::Identifier(_) => self.type_path(),
            Token::ProcKeyword => self.type_procedure(),
            _ => unreachable!()
        }
    }

    fn type_path(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let identifier = self.expect_identifier()?;
        let mut parts = vec![*identifier.data()];
        let mut end = identifier.location();
        while self.peek_is(Token::DoubleColon)? {
            self.advance()?;
            let idenifier = self.expect_identifier()?;
            parts.push(*idenifier.data());
            end = idenifier.location()
        }

        let path = PathTypeExpression { parts, bound: Bound::Undetermined };
        let type_expression = TypeExpression::Path(path);
        Ok(Located::new(
            type_expression,
            identifier.location().extend(&end),
        ))
    }

    fn type_procedure(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let start = self.expect(Token::ProcKeyword)?.location();
        self.expect(Token::LeftParenthesis)?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            Self::type_expression,
            Some(Token::Comma)
        )?;
        self.expect(Token::Colon)?;
        let return_type = Box::new(self.type_expression()?);

        let location = start.extend(&return_type.location());
        let procedure_type = ProcedureTypeExpression {
            arguments,
            return_type,
        };
        Ok(Located::new(TypeExpression::Procedure(procedure_type), location))
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
        let (arguments, location) = if self.peek_is(Token::LeftParenthesis)? {
            self.advance()?;
            let (arguments, end) = self.until(
                Token::RightParenthesis,
                Self::type_expression,
                Some(Token::Comma)
            )?;

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
