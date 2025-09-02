use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub struct T {
    arguments: Vec<Located<InternIdx>>,
    body: Box<Located<Expression>>,
}

impl T {
    pub fn new(arguments: Vec<Located<InternIdx>>, body: Box<Located<Expression>>) -> Self {
        Self { arguments, body }
    }

    pub fn arguments(&self) -> &[Located<InternIdx>] {
        &self.arguments
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body
    }
}
