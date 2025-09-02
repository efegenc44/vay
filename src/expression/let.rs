use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub struct T {
    identifier: Located<InternIdx>,
    value_expression: Box<Located<Expression>>,
    body_expression: Box<Located<Expression>>,
}

impl T {
    pub fn new(
        identifier: Located<InternIdx>,
        value_expression: Box<Located<Expression>>,
        body_expression: Box<Located<Expression>>
    ) -> Self {
        Self { identifier, value_expression, body_expression }
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn value_expression(&self) -> &Located<Expression> {
        &self.value_expression
    }

    pub fn value_expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.value_expression
    }

    pub fn body_expression(&self) -> &Located<Expression> {
        &self.body_expression
    }

    pub fn body_expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body_expression
    }
}