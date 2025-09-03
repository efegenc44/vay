use std::{collections::HashMap, iter::Peekable};

use crate::{
    bound::Path,
    declaration::{self, Declaration},
    expression::{
        self,
        pattern,
        Expression,
        pattern::Pattern,
    },
    type_expression::{self, TypeExpression},
    interner::{interner_mut, InternIdx},
    lexer::Lexer,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    token::Token
};

const PRIMARY_TOKEN_STARTS: &[Token] = &[
    Token::dummy_identifier(),
    Token::U64(0),
    Token::F32(0.),
    Token::String(InternIdx::dummy_idx()),
    Token::Char(' '),
    Token::LetKeyword,
    Token::LeftSquare,
    Token::LeftParenthesis,
    Token::LeftCurly,
    Token::FunKeyword,
    Token::MatchKeyword,
    Token::ReturnKeyword,
    Token::WhileKeyword,
    Token::ContinueKeyword,
    Token::BreakKeyword,
];

const PRIMARY_TYPE_TOKEN_STARTS: &[Token] = &[
    Token::dummy_identifier(),
    Token::FunKeyword,
    Token::LeftParenthesis,
];


const DECLARATION_KEYWORDS: &[Token] = &[
    Token::ModuleKeyword,
    Token::FunKeyword,
    Token::DefineKeyword,
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
    Token::Char(' '),
    Token::Dot,
    Token::LeftSquare,
    Token::LeftParenthesis,
];

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
enum Operator {
    Multiplication,
    Division,
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
    (Operator::Division, "Core::Dividable::divide"),
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
        Token::Slash => Some((Operator::Division, Associativity::Left, 4)),

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

pub struct Parser<'source_content> {
    tokens: Peekable<Lexer<'source_content>>,
    source: String,
    operator_parts: HashMap<Operator, Vec<InternIdx>>
}

impl<'source> Parser<'source> {
    pub fn new(lexer: Lexer<'source>) -> Self {
        let operator_parts = Self::prepare_operator_paths();
        let source = lexer.source().to_string();
        Self {
            tokens: lexer.peekable(),
            source,
            operator_parts
        }
    }

    fn prepare_operator_paths() -> HashMap<Operator, Vec<InternIdx>> {
        let mut table = HashMap::new();
        for (operator, path) in OPERATOR_PATHS {
            let parts = path
                .split("::")
                .map(|part| interner_mut().intern(part.into()))
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
                } else if matches!(expected, Token::Char(_)) {
                    matches!(token.data(), Token::Char(_))
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
            let assignment = expression::Assignment::new(Box::new(expression), Box::new(assign_expression));

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
            let operator_path = expression::Path::new(self.operator_parts[&operator].clone());
            let operator = Box::new(Located::new(Expression::Path(operator_path), operator_location));

            let next_precedence = precedence + (associativity != Associativity::Right) as usize;
            let rhs = self.binary(next_precedence)?;
            let location = expression.location().extend(&rhs.location());

            let application = expression::Application::new(operator, vec![expression, rhs]);
            let binary = Expression::Application(application);
            expression = Located::new(binary, location);

            if associativity == Associativity::None {
                if let Some(token) = self.peek()?    {
                    if let Some((_, Associativity::None, _)) = operators(token.data()) {
                        break;
                    };
                }
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
                let application = expression::Application::new(Box::new(expression), arguments);
                expression = Located::new(Expression::Application(application), location);
            } else if self.peek_is(Token::Dot) {
                self.advance()?;
                let name = self.expect_identifier()?;

                let location = expression.location().extend(&name.location());
                let projection = expression::Projection::new(Box::new(expression), name);
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
            Token::Char(ch) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Expression::Char(*ch), token.location()))
            },
            Token::Identifier(_) => self.path(),
            Token::LeftSquare => self.array(),
            Token::LetKeyword => self.lett(),
            Token::LeftParenthesis => self.sequence(),
            Token::LeftCurly => self.block(),
            Token::FunKeyword => self.lambda(),
            Token::MatchKeyword => self.matc(),
            Token::ReturnKeyword => self.retrn(),
            Token::WhileKeyword => self.whilee(),
            Token::ContinueKeyword => self.continuee(),
            Token::BreakKeyword => self.breakk(),
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

        let path = expression::Path::new(parts.data().to_owned());
        let expression = Expression::Path(path);
        Ok(Located::new(expression, parts.location()))
    }

    fn array(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::LeftSquare)?.location();

        let (expressions, end) = self.until(
            Token::RightSquare,
            Self::expression,
            Some(Token::Comma)
        )?;

        let array = expression::Array::new(expressions);
        let expression = Expression::Array(array);
        Ok(Located::new(expression, start.extend(&end)))
    }

    fn lett(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::LetKeyword)?.location();
        let identifier = self.expect_identifier()?;
        self.expect(Token::Equals)?;
        let value_expression = Box::new(self.expression()?);
        self.expect(Token::InKeyword)?;
        let body_expression = Box::new(self.expression()?);
        let end = body_expression.location();

        let lett = expression::Let::new(identifier, value_expression, body_expression);
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

        let sequence = expression::Sequence::new(expressions);
        let expression = Expression::Sequence(sequence);
        Ok(Located::new(expression, start.extend(&end)))
    }

    fn block(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::LeftCurly)?.location();
        let (expressions, end) = self.until(
            Token::RightCurly,
            Self::expression,
            None
        )?;

        let block = expression::Block::new(expressions);
        let expression = Expression::Block(block);
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

        let lambda = expression::Lambda::new(arguments, body);
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
        let matc = expression::Match::new(expressions, branches);

        Ok(Located::new(Expression::Match(matc), location))
    }

    fn retrn(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::ReturnKeyword)?.location();
        let expression = Box::new(self.expression()?);
        let end = expression.location();

        let retrn = expression::Return::new(expression);
        Ok(Located::new(Expression::Return(retrn), start.extend(&end)))
    }

    fn whilee(&mut self) -> ReportableResult<Located<Expression>> {
        let start = self.expect(Token::WhileKeyword)?.location();

        let condition = Box::new(self.expression()?);
        let post = if self.peek_is(Token::Comma) {
            self.advance()?;
            Some(Box::new(self.expression()?))
        } else {
            None
        };

        self.expect(Token::ThenKeyword)?;

        let body = Box::new(self.expression()?);

        let location = start.extend(&body.location());
        let whilee = expression::While::new(condition, post, body);
        Ok(Located::new(Expression::While(whilee), location))
    }

    fn continuee(&mut self) -> ReportableResult<Located<Expression>> {
        let location = self.expect(Token::ContinueKeyword)?.location();

        Ok(Located::new(Expression::Continue, location))
    }

    fn breakk(&mut self) -> ReportableResult<Located<Expression>> {
        let location = self.expect(Token::BreakKeyword)?.location();

        Ok(Located::new(Expression::Break, location))
    }

    fn match_branch(&mut self) -> ReportableResult<Located<expression::MatchBranch>> {
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
        Ok(Located::new(expression::MatchBranch::new(patterns, expression), location))
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
            Token::Char(ch) => {
                // TODO: Factor this out like the other ones
                self.advance()?;
                Ok(Located::new(Pattern::Char(*ch), token.location()))
            }
            Token::Identifier(_) => self.any_pattern(),
            Token::Dot => self.varint_case_pattern(),
            Token::LeftSquare => self.array_pattern(),
            Token::LeftParenthesis => self.pattern_grouping(),
            _ => unreachable!()
        }
    }

    fn any_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let identifier = self.expect_identifier()?;

        let pattern = Pattern::Any(*identifier.data());
        Ok(Located::new(pattern, identifier.location()))
    }

    fn array_pattern(&mut self) -> ReportableResult<Located<Pattern>> {
        let start = self.expect(Token::LeftSquare)?.location();

        let mut before = vec![];
        let mut after = vec![];

        let mut is_after = false;
        let mut is_first = true;
        let mut rest = None;
        let end = loop {
            if let Some(token) = self.terminator(Token::RightSquare)? {
                break token.location();
            }

            if is_first {
                is_first = false;
            } else {
                self.expect(Token::Comma)?;
            }

            if self.peek_is(Token::DoubleDot) {
                is_after = true;
                self.advance()?;
                if self.peek_is(Token::Identifier(InternIdx::dummy_idx())) {
                    rest = Some(*self.expect_identifier()?.data());
                }
            } else {
                #[allow(clippy::collapsible_else_if)]
                if is_after {
                    after.push(self.pattern()?);
                } else {
                    before.push(self.pattern()?);
                }
            }
        };

        let array = pattern::Array::new(before, after, rest);
        let pattern = Pattern::Array(array);
        Ok(Located::new(pattern, start.extend(&end)))
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

        let variant_case = pattern::VariantCase::new(name, fields);
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
            Token::DefineKeyword => self.define(),
            Token::VariantKeyword => self.variant(),
            Token::InterfaceKeyword => self.interface(),
            Token::StructKeyword => self.strct(),
            Token::BuiltInKeyword => self.builtin(),
            Token::ExternalKeyword => self.external(),
            _ => unreachable!()
        }
    }

    pub fn module(&mut self) -> ReportableResult<declaration::Module> {
        let mut declarations = vec![];
        while self.peek()?.is_some() {
            declarations.push(self.declaration()?);
        }
        Ok(declaration::Module::new(declarations, self.source.clone()))
    }

    fn type_var(&mut self) -> ReportableResult<Located<declaration::TypeVar>> {
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

        let type_var = declaration::TypeVar::new(name, interfaces);
        Ok(Located::new(type_var, name.location().extend(&end)))
    }

    fn modul(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ModuleKeyword)?;
        let parts = self.path_parts()?;

        let module = declaration::ModulePath::new(parts);
        Ok(Declaration::ModulePath(module))
    }

    fn import(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::ImportKeyword)?;
        let import = self.import_name()?;

        Ok(Declaration::Import(import))
    }

    fn import_name(&mut self) -> ReportableResult<declaration::Import> {
        let import_in = if self.peek_is(Token::InKeyword) {
            self.advance()?;
            true
        } else {
            false
        };

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

        Ok(declaration::Import::new(import_in, name, subnames, as_name))
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

        let function = declaration::Function::new(name, type_vars, arguments, return_type, body);
        Ok(Declaration::Function(function))
    }

    fn define(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::DefineKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(Token::Colon)?;
        let type_expression = self.type_expression()?;
        self.expect(Token::Equals)?;
        let expression = self.expression()?;

        let define = declaration::Define::new(name, type_expression, expression);
        Ok(Declaration::Define(define))
    }

    fn method_declaration(&mut self) -> ReportableResult<declaration::Method> {
        let signature = self.method_signature()?;
        self.expect(Token::Equals)?;
        let body = self.expression()?;

        Ok(declaration::Method::new(signature, body))
    }

    fn constraint(&mut self) -> ReportableResult<declaration::MethodConstraint> {
        let type_var = self.type_var()?;

        let constraint = declaration::MethodConstraint::new(type_var);
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

        let variant = declaration::Variant::new(name, type_vars, cases, methods);
        Ok(Declaration::Variant(variant))
    }

    fn interface_signature(&mut self) -> ReportableResult<declaration::InterfaceSignature> {
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

        Ok(declaration::InterfaceSignature::new(name, arguments, return_type))
    }

    fn method_signature(&mut self) -> ReportableResult<declaration::MethodSignature> {
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

        Ok(declaration::MethodSignature::new(name, constraints, type_vars, instance, arguments, return_type))
    }

    fn interface(&mut self) -> ReportableResult<Declaration> {
        self.expect(Token::InterfaceKeyword)?;
        let name = self.expect_identifier()?;
        let type_name = self.type_var()?;
        self.expect(Token::LeftCurly)?;
        let (methods, _) = self.until(Token::RightCurly, Self::interface_signature, None)?;

        let interface = declaration::Interface::new(name, type_name, methods);
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

        let strct = declaration::Struct::new(name, type_vars, fields, methods);
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
        let (methods, _) = self.until(
            Token::RightCurly,
            |parser| {
                let method_signature = parser.method_signature()?;
                let body = if parser.peek_is(Token::Equals) {
                    parser.advance()?;
                    Some(parser.expression()?)
                } else {
                    None
                };

                Ok((method_signature, body))
            },
            None
        )?;

        let builtin = declaration::BuiltIn::new(name, type_vars, methods);
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

        let external = declaration::External::new(name, type_vars, arguments, return_type);
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
                let application = type_expression::Application::new(Box::new(expression), arguments);
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
        let path = type_expression::Path::new(parts.data().to_owned());
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
        let function_type = type_expression::Function::new(arguments, return_type);
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

    fn typed_identifier(&mut self) -> ReportableResult<Located<declaration::TypedIdentifier>> {
        let identifier = self.expect_identifier()?;
        self.expect(Token::Colon)?;
        let type_expression = self.type_expression()?;

        let location = identifier.location().extend(&type_expression.location());
        let typed_identifier = declaration::TypedIdentifier::new(identifier, type_expression);
        Ok(Located::new(typed_identifier, location))
    }

    fn variant_case(&mut self) -> ReportableResult<Located<declaration::VariantCase>> {
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

        let variant_case = declaration::VariantCase::new(identifier, arguments);
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

    fn description(&self) -> String {
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
