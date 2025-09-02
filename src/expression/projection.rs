use crate::{expression::Expression, interner::InternIdx, location::Located};

#[derive(Clone)]
pub struct T {
    expression: Box<Located<Expression>>,
    projected: Located<InternIdx>
}

impl T {
    pub fn new(expression: Box<Located<Expression>>, projected: Located<InternIdx>) -> Self {
        Self { expression, projected }
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }

    pub fn projected(&self) -> Located<InternIdx> {
        self.projected
    }
}