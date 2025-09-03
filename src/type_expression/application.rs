use crate::{location::Located, type_expression::TypeExpression};

#[derive(Clone)]
pub struct T {
    function: Box<Located<TypeExpression>>,
    arguments: Vec<Located<TypeExpression>>,
}

impl T {
    pub fn new(function: Box<Located<TypeExpression>>, arguments: Vec<Located<TypeExpression>>) -> Self {
        Self { function, arguments }
    }

    pub fn function(&self) -> &Located<TypeExpression> {
        &self.function
    }

    pub fn function_mut(&mut self) -> &mut Box<Located<TypeExpression>> {
        &mut self.function
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<TypeExpression>> {
        &mut self.arguments
    }
}