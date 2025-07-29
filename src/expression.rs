use crate::{bound::Bound, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Expression {
    Path(PathExpression),
    Application(ApplicationExpression),
    Projection(ProjectionExpression),
    Let(LetExpression),
    Sequence(SequenceExpression),
    Lambda(LambdaExpression),
    Match(MatchExpression)
}

#[derive(Clone)]
pub struct PathExpression {
    pub parts: Vec<InternIdx>,
    pub bound: Bound
}

#[derive(Clone)]
pub struct ApplicationExpression {
    pub function: Box<Located<Expression>>,
    pub arguments: Vec<Located<Expression>>,
}

#[derive(Clone)]
pub struct ProjectionExpression {
    pub expression: Box<Located<Expression>>,
    pub name: Located<InternIdx>
}

#[derive(Clone)]
pub struct LetExpression {
    pub name: Located<InternIdx>,
    pub value_expression: Box<Located<Expression>>,
    pub body_expression: Box<Located<Expression>>,
}

#[derive(Clone)]
pub struct SequenceExpression {
    pub expressions: Vec<Located<Expression>>,
}

#[derive(Clone)]
pub struct LambdaExpression {
    pub arguments: Vec<Located<InternIdx>>,
    pub body: Box<Located<Expression>>,
}

#[derive(Clone)]
pub struct MatchExpression {
    pub expression: Box<Located<Expression>>,
    pub branches: Vec<Located<MatchBranch>>,
}

#[derive(Clone)]
pub struct MatchBranch {
    pattern: Located<Pattern>,
    expression: Located<Expression>,
}

impl MatchBranch {
    pub fn new(pattern: Located<Pattern>, expression: Located<Expression>) -> Self {
        Self { pattern, expression }
    }

    pub fn pattern(&self) -> &Located<Pattern> {
        &self.pattern
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }
}

#[derive(Clone)]
pub enum Pattern {
    Any(InternIdx),
    VariantCase(VariantCasePattern)
}

#[derive(Clone)]
pub struct VariantCasePattern {
    pub name: Located<InternIdx>,
    pub fields: Option<Vec<Located<Pattern>>>,
}

#[derive(Clone)]
pub enum TypeExpression {
    Path(PathTypeExpression),
    Function(FunctionTypeExpression),
    Application(TypeApplicationExpression)
}

#[derive(Clone)]
pub struct PathTypeExpression {
    pub parts: Vec<InternIdx>,
    pub bound: Bound
}

#[derive(Clone)]
pub struct FunctionTypeExpression {
    pub arguments: Vec<Located<TypeExpression>>,
    pub return_type: Box<Located<TypeExpression>>
}

#[derive(Clone)]
pub struct TypeApplicationExpression {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}
