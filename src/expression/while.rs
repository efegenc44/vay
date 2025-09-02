use crate::{expression::Expression, location::Located};

#[derive(Clone)]
pub struct T {
    condition: Box<Located<Expression>>,
    post: Option<Box<Located<Expression>>>,
    body: Box<Located<Expression>>
}

impl T {
    pub fn new(
        condition: Box<Located<Expression>>,
        post: Option<Box<Located<Expression>>>,
        body: Box<Located<Expression>>
    ) -> Self {
        Self { condition, post, body }
    }

    pub fn condition(&self) -> &Located<Expression> {
        &self.condition
    }

    pub fn condition_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.condition
    }

    pub fn post(&self) -> Option<&Box<Located<Expression>>> {
        self.post.as_ref()
    }

    pub fn post_mut(&mut self) -> &mut Option<Box<Located<Expression>>> {
        &mut self.post
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body
    }
}
