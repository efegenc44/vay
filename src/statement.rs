use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Statement {
    Expression(Located<Expression>),
    Return(ReturnStatement),
    Match(MatchStatement),
}

#[derive(Clone)]
pub struct ReturnStatement {
    pub expression: Located<Expression>
}

#[derive(Clone)]
pub struct MatchStatement {
    pub expression: Located<Expression>,
    pub branches: Vec<Located<MatchBranch>>,
}

#[derive(Clone)]
pub struct MatchBranch {
    pub pattern: Located<Pattern>,
    pub statement: Located<Statement>,
}

#[derive(Clone)]
pub enum Pattern {
    VariantCase(VariantCasePattern)
}

#[derive(Clone)]
pub struct VariantCasePattern {
    pub name: Located<InternIdx>,
    pub fields: Option<Vec<Located<InternIdx>>>,
}