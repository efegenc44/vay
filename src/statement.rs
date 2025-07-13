use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Statement {
    Expression(Located<Expression>),
    Return(Located<Expression>),
    Match {
        expression: Located<Expression>,
        branches: Vec<Located<MatchBranch>>,
    },
}

#[derive(Clone)]
pub struct MatchBranch {
    pub pattern: Located<Pattern>,
    pub statement: Located<Statement>,
}

#[derive(Clone)]
pub enum Pattern {
    VariantCase {
        name: Located<InternIdx>,
        fields: Option<Vec<Located<InternIdx>>>,
    },
}
