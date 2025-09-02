use crate::{expression::Expression, location::Located};

#[derive(Clone)]
pub struct T {
    function: Box<Located<Expression>>,
    arguments: Vec<Located<Expression>>,
}

impl T {
    pub fn new(function: Box<Located<Expression>>, arguments: Vec<Located<Expression>>) -> Self {
        Self { function, arguments }
    }

    pub fn function(&self) -> &Located<Expression> {
        &self.function
    }

    pub fn function_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.function
    }

    pub fn arguments(&self) -> &[Located<Expression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.arguments
    }
}