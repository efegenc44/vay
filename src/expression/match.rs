use crate::{expression::{pattern::Pattern, Expression}, location::Located};

#[derive(Clone)]
pub struct T {
    expressions: Vec<Located<Expression>>,
    branches: Vec<Located<Branch>>,
}

impl T {
    pub fn new(expressions: Vec<Located<Expression>>, branches: Vec<Located<Branch>>) -> Self {
        Self { expressions, branches }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }

    pub fn branches(&self) -> &[Located<Branch>] {
        &self.branches
    }

    pub fn branches_mut(&mut self) -> &mut Vec<Located<Branch>> {
        &mut self.branches
    }
}

#[derive(Clone)]
pub struct Branch {
    patterns: Vec<Located<Pattern>>,
    expression: Located<Expression>,
}

impl Branch {
    pub fn new(patterns: Vec<Located<Pattern>>, expression: Located<Expression>) -> Self {
        Self { patterns, expression }
    }

    pub fn patterns(&self) -> &[Located<Pattern>] {
        &self.patterns
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }
}
