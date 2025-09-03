use crate::{location::Located, type_expression::TypeExpression};


#[derive(Clone)]
pub struct T {
    arguments: Vec<Located<TypeExpression>>,
    return_type: Option<Box<Located<TypeExpression>>>
}

impl T {
    pub fn new(arguments: Vec<Located<TypeExpression>>, return_type: Option<Box<Located<TypeExpression>>>) -> Self {
        Self { arguments, return_type }
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<TypeExpression>> {
        &mut self.arguments
    }

    pub fn return_type(&self) -> Option<&Box<Located<TypeExpression>>> {
        self.return_type.as_ref()
    }

    pub fn return_type_mut(&mut self) -> &mut Option<Box<Located<TypeExpression>>> {
        &mut self.return_type
    }
}