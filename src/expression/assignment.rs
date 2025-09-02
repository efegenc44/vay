use crate::{expression::Expression, location::Located};

#[derive(Clone)]
pub struct T {
    assignable: Box<Located<Expression>>,
    expression: Box<Located<Expression>>,
}

impl T {
    pub fn new(assignable: Box<Located<Expression>>, expression: Box<Located<Expression>>) -> Self {
        Self { assignable, expression }
    }

    pub fn assignable(&self) -> &Located<Expression> {
        &self.assignable
    }

    pub fn assignable_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.assignable
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }
}