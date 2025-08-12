use std::{collections::HashMap, iter::Peekable};

use crate::{
    bound::{Bound, Path},
    declaration::{
        BuiltInDeclaration, Constraint, Declaration, ExternalDeclaration, FunctionDeclaration, ImportDeclaration, ImportName, InterfaceDeclaration, InterfaceMethodSignature, MethodDeclaration, MethodSignature, Module, ModuleDeclaration, StructDeclaration, TypeVar, TypedIdentifier, VariantCase, VariantDeclaration
    },
    expression::{
        ApplicationExpression, AssignmentExpression, Expression, FunctionTypeExpression, LambdaExpression, LetExpression, MatchBranch, MatchExpression, PathExpression, PathTypeExpression, Pattern, ProjectionExpression, ReturnExpression, SequenceExpression, TypeApplicationExpression, TypeExpression, VariantCasePattern
    },
    interner::{InternIdx, Interner},
    lexer::Lexer,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    token::Token,
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[
    Token::dummy_identifier(),
    Token::U64(0),
    Token::F32(0.),
    Token::String(InternIdx::dummy_idx()),
    Token::LetKeyword,
    Token::LeftParenthesis,
    Token::FunKeyword,
    Token::MatchKeyword,
    Token::ReturnKeyword,
];

const PRIMARY_TYPE_TOKEN_STARTS: &[Token] = &[
    Token::dummy_identifier(),
    Token::FunKeyword,
    Token::LeftParenthesis,
];


const DECLARATION_KEYWORDS: &[Token] = &[
    Token::ModuleKeyword,
    Token::FunKeyword,
    Token::VariantKeyword,
    Token::ImportKeyword,
    Token::InterfaceKeyword,
    Token::StructKeyword,
    Token::BuiltInKeyword,
    Token::ExternalKeyword,
];

const PATTERN_TOKEN_STARTS: &[Token] = &[
    Token::dummy_identifier(),
    Token::U64(0),
    Token::F32(0.),
    Token::String(InternIdx::dummy_idx()),
    Token::Dot,
    Token::LeftParenthesis,
];

#[derive(Eq, PartialEq, Hash, Clone, Copy)]
enum Operator {
    Multiplication,
    Addition,
    Subtraction,
    Equality,
    NotEquality,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or
}

const OPERATOR_PATHS: &[(Operator, &str)] = &[
    (Operator::Multiplication, "Core::Multiplicable::multiply"),
    (Operator::Addition, "Core::Addable::add"),
    (Operator::Subtraction, "Core::Subtractable::subtract"),
    (Operator::Equality, "Core::Equatable::equals"),
    (Operator::NotEquality, "Core::notEquals"),
    (Operator::LessThan, "Core::lessThan"),
    (Operator::LessThanOrEqual, "Core::lessThanOrEqual"),
    (Operator::GreaterThan, "Core::greaterThan"),
    (Operator::GreaterThanOrEqual, "Core::greaterThanOrEqual"),
    (Operator::And,  "Core::Logical::and"),
    (Operator::Or,  "Core::Logical::or"),
];

const fn operators(token: &Token) -> Option<(Operator, Associativity, usize)> {
    match token {
        Token::Asterisk => Some((Operator::Multiplication, Associativity::Left, 4)),

        Token::Plus => Some((Operator::Addition, Associativity::Left, 3)),
        Token::Minus => Some((Operator::Subtraction, Associativity::Left, 3)),

        Token::DoubleEquals => Some((Operator::Equality, Associativity::None, 2)),
        Token::SlashEquals => Some((Operator::NotEquality, Associativity::None, 2)),
        Token::Less => Some((Operator::LessThan, Associativity::None, 2)),
        Token::LessEquals => Some((Operator::LessThanOrEqual, Associativity::None, 2)),
        Token::Greater => Some((Operator::GreaterThan, Associativity::None, 2)),
        Token::GreaterEquals => Some((Operator::GreaterThanOrEqual, Associativity::None, 2)),

        Token::Ampersand => Some((Operator::And, Associativity::Right, 1)),
        Token::Bar => Some((Operator::Or, Associativity::Right, 0)),
        _ => None
    }
}

#[derive(PartialEq)]
enum Associativity {
    Right,
    Left,
    None
}

pub struct Parser<'source_content, 'interner> {
    tokens: Peekable<Lexer<'source_content, 'interner>>,
    source: String,
    operator_parts: HashMap<Operator, Vec<InternIdx>>
}

impl<'source, 'interner> Parser<'source, 'interner> {
    pub fn new(mut lexer: Lexer<'source, 'interner>) -> Self {
        let operator_parts = Self::prepare_operator_paths(lexer.interner());
        let source = lexer.source().to_string();
        Self {
            tokens: lexer.peekable(),
            source,
            operator_parts
        }
    }

    fn prepare_operator_paths(interner: &mut Interner) -> HashMap<Operator, Vec<InternIdx>> {
        let mut table = HashMap::new();
        for (operator, path) in OPERATOR_PATHS {
            let parts = path
                .split("::")
                .map(|part| interner.intern(part.into()))
                .collect();

            table.insert(*operator, parts);
        }
        table
    }

    fn peek(&mut self) -> ReportableResult<Option<Located<Token>>> {
        match self.tokens.peek() {
            Some(Ok(token)) => Ok(Some(*token)),
            // TODO: fix here, hacky
            // NOTE: This causes wrong error reporting
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
                } else if matches!(expected, Token::U64(_)) {
                    matches!(token.data(), Token::U64(_))
                } else if matches!(expected, Token::F32(_)) {
                    matches!(token.data(), Token::F32(_))
                } else if matches!(expected, Token::String(_)) {
                    matches!(token.data(), Token::String(_))
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

    fn peek_is(&mut self, expected: Token) -> bool {
        self.peek_one_of(&[expected]).is_ok()
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

    pub fn session_expression(&mut self) -> ReportableResult<Located<Expression>> {
        let expression = self.assignment()?;
        if self.peek()?.is_some() {
            panic!("Couldnt consume all tokens")
        }

        Ok(expression)
    }

    fn expression(&mut self) -> ReportableResult<Located<Expression>> {
        self.assignment()
    }

    fn assignment(&mut self) -> ReportableResult<Located<Expression>> {
        let expression = self.binary(0)?;
        if self.peek_is(Token::Equals) {
            self.advance()?;

            let assign_expression = self.expression()?;
            let location = expression.location().extend(&assign_expression.location());
            let assignment = AssignmentExpression {
                assignable: Box::new(expression),
                expression: Box::new(assign_expression),
            };

            Ok(Located::new(Expression::Assignment(assignment), location))
        } else {
            Ok(expression)
        }
    }

    fn binary(&mut self, min_precedence: usize) -> ReportableResult<Located<Expression>> {
        let mut expression = self.application()?;
        while let Some(token) = self.peek()? {
            let Some((operator, associativity, precedence)) = operators(token.data()) else {
                break;
            };

            if precedence < min_precedence {
                break;
            }

            let operator_location = self.advance()?.unwrap().location();
            let operator_path = PathExpression {
                parts: self.operator_parts[&operator].clone(),
                bound: Bound::Undetermined
            };
            let operator = Box::new(Located::new(Expression::Path(operator_path), operator_location));

            let next_precedence = precedence + (associativity != Associativity::Right) as usize;
            let rhs = self.binary(next_precedence)?;
            let location = expression.location().extend(&rhs.location());

            let application = ApplicationExpression {
                function: operator,
                arguments: vec![expression, rhs],
            };
            let binary = Expression::Application(application);
            expression = Located::new(binary, location);

            if associativity == Associativity::None {
                break;
            }
        }

        Ok(expression)
    }

    fn application(&mut self) -> ReportableResult<Located<Expression>> {
        let mut expression = self.primary()?;
        loop {
            if self.peek_is(Token::LeftParenthesis) {
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
            } else if self.peek_is(Token::Dot) {
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
            Token::U64(u64) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Expression::U64(*u64), token.location()))
            },
            Token::F32(f32) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Expression::F32(*f32), token.location()))
            },
            Token::String(string_idx) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Expression::String(*string_idx), token.location()))
            },
            Token::Identifier(_) => self.path(),
            Token::LetKeyword => self.lett(),
            Token::LeftParenthesis => self.sequence(),
            Token::FunKeyword => self.lambda(),
            Token::MatchKeyword => self.matc(),
            Token::ReturnKeyword => self.retrn(),
            _ => unreachable!()
        }
    }

    fn path_parts(&mut self) -> ReportableResult<Located<Vec<InternIdx>>> {
        let identifier = self.expect_identifier()?;
        let mut parts = vec![*identifier.data()];
        let mut end = identifier.location();
        while self.peek_is(Token::DoubleColon) {
            self.advance()?;
            let identifier = self.expect_identifier()?;
            parts.push(*identifier.data());
            end = identifier.location();
        }

        Ok(Located::new(parts, identifier.location().extend(&end)))
    }

    fn path(&mut self) -> ReportableResult<Located<Expression>> {
        let parts = self.path_parts()?;

        let path = PathExpression { parts: parts.data().to_owned(), bound: Bound::Undetermined };
        let expression = Expression::Path(path);
        Ok(Located::new(expression, parts.location()))
    }

    fn lett(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::LetKeyword)?.location();
        let name = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let value_expression = Box::new(self.expression()?);
        self.expect(Token::InKeyword)?;
        let body_expression = Box::new(self.expression()?);
        let end = body_expression.location();

        let lett = LetExpression { name, value_expression, body_expression };
        let expression = Expression::Let(lett);
        Ok(Located::new(expression, start.extend(&end)))
    }

    fn sequence(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::LeftParenthesis)?.location();
        let (expressions, end) = self.until(
            Token::RightParenthesis,
            Self::expression,
            None
        )?;

        let sequence = SequenceExpression { expressions };
        let expression = Expression::Sequence(sequence);
        Ok(Located::new(expression, start.extend(&end)))
    }

    fn lambda(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::FunKeyword)?.location();
        self.expect(Token::LeftParenthesis)?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            Self::expect_identifier,
            Some(Token::Comma)
        )?;
        let body = Box::new(self.expression()?);
        let end = body.location();

        let lambda = LambdaExpression  { arguments, body };
        let expression = Expression::Lambda(lambda);
        Ok(Located::new(expression, start.extend(&end)))
    }

    fn matc(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::MatchKeyword)?.location();
        let expressions = self.until(
            Token::LeftCurly,
            Self::expression,
            Some(Token::Comma)
        )?.0;

        if expressions.is_empty() {
            todo!("Expression is expected!");
        }

        let (branches, end) = self.until(Token::RightCurly, Self::match_branch, None)?;
        let location = start.extend(&end);
        let matc = MatchExpression {
            expressions,
            branches
        };

        Ok(Located::new(Expression::Match(matc), location))
    }

    fn retrn(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::ReturnKeyword)?.location();
        let expression = Box::new(self.expression()?);
        let end = expression.location();

        let retrn = ReturnExpression { expression };
        Ok(Located::new(Expression::Return(retrn), start.extend(&end)))
    }

    fn match_branch(&mut self) -> ReportableResult<Located<MatchBranch>> {
        let start = self.expect(Token::LetKeyword)?.location();
        let patterns = self.until(
            Token::Colon,
            Self::pattern,
            Some(Token::Comma)
        )?.0;

        if patterns.is_empty() {
            todo!("Pattern is expected!")
        }

        let expression = self.expression()?;

        let location = start.extend(&expression.location());
        Ok(Located::new(MatchBranch::new(patterns, expression), location))
    }

    fn pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let token = self.peek_one_of(PATTERN_TOKEN_STARTS)?;
        match token.data() {
            Token::U64(u64) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Pattern::U64(*u64), token.location()))
            }
            Token::F32(f32) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Pattern::F32(*f32), token.location()))
            }
            Token::String(string_idx) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Pattern::String(*string_idx), token.location()))
            }
            Token::Identifier(_) => self.any_pattern(),
            Token::Dot => self.varint_case_pattern(),
            Token::LeftParenthesis => self.pattern_grouping(),
            _ => unreachable!()
        }
    }

    fn any_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let identifier = self.expect_identifier()?;

        let pattern = Pattern::Any(*identifier.data());
        Ok(Located::new(pattern, identifier.location()))
    }

    fn varint_case_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        self.expect(Token::Dot)?;
        let name = self.expect_identifier()?;
        let (fields, location) = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            let (fields, end) = self.until(
                Token::RightParenthesis,
                Self::pattern,
                Some(Token::Comma)
            )?;
            (Some(fields), name.location().extend(&end))
        } else {
            (None, name.location())
        };

        let variant_case = VariantCasePattern { name, fields };
        Ok(Located::new(Pattern::VariantCase(variant_case), location))
    }

    fn pattern_grouping(&mut self) -> ReportableResult<Located<Pattern>> {
        let start = self.expect(Token::LeftParenthesis)?.location();
        if self.peek_is(Token::RightParenthesis) {
            let end = self.advance()?.unwrap().location();

            let pattern = Pattern::Unit;
            Ok(Located::new(pattern, start.extend(&end)))
        } else {
            let pattern = self.pattern()?;
            let end = self.expect(Token::RightParenthesis)?.location();

            Ok(Located::new(pattern.data().to_owned(), start.extend(&end)))
        }
    }

    pub fn declaration(&mut self) -> ReportableResult<Declaration> {
        let token = self.peek_one_of(DECLARATION_KEYWORDS)?;
        match token.data() {
            Token::ModuleKeyword => self.modul(),
            Token::ImportKeyword => self.import(),
            Token::FunKeyword => self.function(),
            Token::VariantKeyword => self.variant(),
            Token::InterfaceKeyword => self.interface(),
            Token::StructKeyword => self.strct(),
            Token::BuiltInKeyword => self.builtin(),
            Token::ExternalKeyword => self.external(),
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
        let (interfaces, end) = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                |parser| {
                    Ok((parser.path_parts()?, Path::empty()))
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
        let parts = self.path_parts()?;

        let module = ModuleDeclaration { parts };
        Ok(Declaration::Module(module))
    }

    fn import(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ImportKeyword)?;
        let name = self.import_name()?;

        let import = ImportDeclaration { name };
        Ok(Declaration::Import(import))
    }

    fn import_name(&mut self) -> ReportableResult<ImportName> {
        let name = self.expect_identifier()?;
        let as_name = if self.peek_is(Token::AsKeyword) {
            self.advance()?;
            Some(self.expect_identifier()?)
        } else {
            None
        };
        let subnames = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            let (subnames, _) = self.until(
                Token::RightParenthesis,
                Self::import_name,
                Some(Token::Comma)
            )?;

            Some(subnames)
        } else {
            None
        };

        Ok(ImportName { name, subnames, as_name })
    }

    fn function(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::FunKeyword)?;
        let name = self.expect_identifier()?;

        let type_vars = if self.peek_is(Token::Colon) {
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

        let return_type = if self.peek_is(Token::Colon) {
            self.advance()?;
            Some(self.type_expression()?)
        } else {
            None
        };

        self.expect(Token::Equals)?;
        let body = self.expression()?;

        let function = FunctionDeclaration {
            name,
            type_vars,
            arguments,
            return_type,
            body,
            path: Path::empty(),
        };

        Ok(Declaration::Function(function))
    }

    fn method_declaration(&mut self) -> ReportableResult<MethodDeclaration> {
        let signature = self.method_signature()?;
        self.expect(Token::Equals)?;
        let body = self.expression()?;

        Ok(MethodDeclaration {
            signature,
            body,
        })
    }

    fn constraint(&mut self) -> ReportableResult<Constraint> {
        let type_var = self.type_var()?;

        let constraint = Constraint {
            nth: usize::default(),
            type_var,
        };

        Ok(constraint)
    }

    fn variant(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::VariantKeyword)?;
        let name = self.expect_identifier()?;
        let type_vars = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                Self::expect_identifier,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };
        self.expect(Token::LeftCurly)?;
        let mut cases = vec![];
        let mut methods = vec![];
        while self.terminator(Token::RightCurly)?.is_none() {
            if self.peek_is(Token::FunKeyword) {
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

    fn interface_method_signature(&mut self) -> ReportableResult<InterfaceMethodSignature> {
        self.expect(Token::FunKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::LeftParenthesis)?;
        let (arguments, _) = self.until(
            Token::RightParenthesis,
            Self::typed_identifier,
            Some(Token::Comma)
        )?;

        let return_type = if self.peek_is(Token::Colon) {
            self.advance()?;
            Some(self.type_expression()?)
        } else {
            None
        };

        Ok(InterfaceMethodSignature { name, arguments, return_type, path: Path::empty() })
    }

    fn method_signature(&mut self) -> ReportableResult<MethodSignature> {
        self.expect(Token::FunKeyword)?;

        let constraints = if self.peek_is(Token::Colon) {
            self.advance()?;
            self.expect(Token::LeftParenthesis)?;
            self.until(
                Token::RightParenthesis,
                Self::constraint,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };

        let name = self.expect_identifier()?;

        let type_vars = if self.peek_is(Token::Colon) {
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

        let return_type = if self.peek_is(Token::Colon) {
            self.advance()?;
            Some(self.type_expression()?)
        } else {
            None
        };

        Ok(MethodSignature { name, constraints, type_vars, instance, arguments, return_type })
    }

    fn interface(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::InterfaceKeyword)?;
        let name = self.expect_identifier()?;
        let type_name = self.type_var()?;
        self.expect(Token::LeftCurly)?;
        let (methods, _) = self.until(Token::RightCurly, Self::interface_method_signature, None)?;


        let interface = InterfaceDeclaration { name, type_name, methods, path: Path::empty() };
        Ok(Declaration::Interface(interface))
    }

    fn strct(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::StructKeyword)?;
        let name = self.expect_identifier()?;
        let type_vars = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                Self::expect_identifier,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };
        self.expect(Token::LeftCurly)?;
        let mut fields = vec![];
        let mut methods = vec![];
        while self.terminator(Token::RightCurly)?.is_none() {
            if self.peek_is(Token::FunKeyword) {
                methods.push(self.method_declaration()?);
            } else {
                fields.push(self.typed_identifier()?)
            }
        }

        let strct = StructDeclaration {
            name,
            type_vars,
            fields,
            methods,
            path: Path::empty(),
        };

        Ok(Declaration::Struct(strct))
    }

    fn builtin(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::BuiltInKeyword)?;
        let name = self.expect_identifier()?;
        let type_vars = if self.peek_is(Token::LeftParenthesis) {
            self.advance()?;
            self.until(
                Token::RightParenthesis,
                Self::expect_identifier,
                Some(Token::Comma)
            )?.0
        } else {
            vec![]
        };
        self.expect(Token::LeftCurly)?;
        let (methods, _) = self.until(Token::RightCurly, Self::method_signature, None)?;

        let builtin = BuiltInDeclaration { name, methods, path: Path::empty(), type_vars };
        Ok(Declaration::BuiltIn(builtin))
    }

    fn external(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ExternalKeyword)?;
        let name = self.expect_identifier()?;

        let type_vars = if self.peek_is(Token::Colon) {
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

        let return_type = if self.peek_is(Token::Colon) {
            self.advance()?;
            Some(self.type_expression()?)
        } else {
            None
        };

        let external = ExternalDeclaration { name, type_vars, arguments, return_type, path: Path::empty() };
        Ok(Declaration::External(external))
    }

    fn type_expression(&mut self) -> ReportableResult<Located<TypeExpression>> {
        self.type_application()
    }

    fn type_application(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let mut expression = self.type_expression_primary()?;
        loop {
            if self.peek_is(Token::LeftParenthesis) {
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
            Token::FunKeyword => self.function_type(),
            Token::LeftParenthesis => self.type_grouping(),
            _ => unreachable!()
        }
    }

    fn type_path(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let parts = self.path_parts()?;
        let path = PathTypeExpression { parts: parts.data().to_owned(), bound: Bound::Undetermined };
        let type_expression = TypeExpression::Path(path);
        Ok(Located::new(
            type_expression,
            parts.location(),
        ))
    }

    fn function_type(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let start = self.expect(Token::FunKeyword)?.location();
        self.expect(Token::LeftParenthesis)?;
        let (arguments, end) = self.until(
            Token::RightParenthesis,
            Self::type_expression,
            Some(Token::Comma)
        )?;

        let (return_type, end) = if self.peek_is(Token::Colon) {
            self.advance()?;
            let return_type = self.type_expression()?;
            let end = return_type.location();
            (Some(Box::new(return_type)), end)
        } else {
            (None, end)
        };

        let location = start.extend(&end);
        let function_type = FunctionTypeExpression {
            arguments,
            return_type,
        };
        Ok(Located::new(TypeExpression::Function(function_type), location))
    }

    fn type_grouping(&mut self) -> ReportableResult<Located<TypeExpression>> {
        let start = self.expect(Token::LeftParenthesis)?.location();
        if self.peek_is(Token::RightParenthesis) {
            let end = self.advance()?.unwrap().location();

            let type_expression = TypeExpression::Unit;
            Ok(Located::new(type_expression, start.extend(&end)))
        } else {
            let type_expression = self.type_expression()?;
            let end = self.expect(Token::RightParenthesis)?.location();

            Ok(Located::new(type_expression.data().to_owned(), start.extend(&end)))
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
        let (arguments, location) = if self.peek_is(Token::LeftParenthesis) {
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
