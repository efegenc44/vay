use crate::{expression::Expression, location::Located};

#[derive(Clone)]
pub struct T {
    expressions: Vec<Located<Expression>>,
}

impl T {
    pub fn new(expressions: Vec<Located<Expression>>) -> Self {
        Self { expressions }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }
}

