use crate::{bound::Bound, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Expression {
    Path(PathExpression),
    Application(ApplicationExpression),
    Projection(ProjectionExpression),
    Let(LetExpression),
    Sequence(SequenceExpression),
    Lambda(LambdaExpression),
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
pub enum TypeExpression {
    Path(PathTypeExpression),
    Procedure(ProcedureTypeExpression),
    Application(TypeApplicationExpression)
}

#[derive(Clone)]
pub struct PathTypeExpression {
    pub parts: Vec<InternIdx>,
    pub bound: Bound
}

#[derive(Clone)]
pub struct ProcedureTypeExpression {
    pub arguments: Vec<Located<TypeExpression>>,
    pub return_type: Box<Located<TypeExpression>>
}

#[derive(Clone)]
pub struct TypeApplicationExpression {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}
