use crate::{interner::InternIdx, location::Located, type_expression::TypeExpression};

#[derive(Clone)]
pub struct T {
    identifier: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
}

impl T {
    pub fn new(identifier: Located<InternIdx>, type_expression: Located<TypeExpression>) -> Self {
        Self { identifier, type_expression }
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn type_expression(&self) -> &Located<TypeExpression> {
        &self.type_expression
    }

    pub fn type_expression_mut(&mut self) -> &mut Located<TypeExpression> {
        &mut self.type_expression
    }
}
