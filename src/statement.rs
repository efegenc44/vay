use crate::{expression::Expression, interner::InternIdx, location::Located};

pub enum Statement {
    Expression(Located<Expression>),
    Return(Located<Expression>),
    Match {
        expression: Located<Expression>,
        branches: Vec<Located<MatchBranch>>,
    },
}

pub struct MatchBranch {
    pub pattern: Located<Pattern>,
    pub statement: Located<Statement>,
}

pub enum Pattern {
    VariantCase {
        name: Located<InternIdx>,
        fields: Option<Vec<Located<InternIdx>>>,
    },
}
