use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Statement {
    Expression(Located<Expression>),
    Return(ReturnStatement),
    Match(MatchStatement),
}

impl Statement {
    pub fn returns(&self) -> bool {
        match self {
            Statement::Expression(_) => false,
            Statement::Return(_) => true,
            Statement::Match(matc) => {
                for branch in &matc.branches {
                    if !branch.data().statement().data().returns() {
                        return false;
                    }
                }
                true
            },
        }
    }
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
    pattern: Located<Pattern>,
    statement: Located<Statement>,
}

impl MatchBranch {
    pub fn new(pattern: Located<Pattern>, statement: Located<Statement>) -> Self {
        Self { pattern, statement }
    }

    pub fn pattern(&self) -> &Located<Pattern> {
        &self.pattern
    }

    pub fn statement(&self) -> &Located<Statement> {
        &self.statement
    }

    pub fn statement_mut(&mut self) -> &mut Located<Statement> {
        &mut self.statement
    }
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