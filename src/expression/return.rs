use crate::{expression::Expression, location::Located};

#[derive(Clone)]
pub struct T {
    expression: Box<Located<Expression>>,
}

impl T {
    pub fn new(expression: Box<Located<Expression>>) -> Self {
        Self { expression }
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }
}
