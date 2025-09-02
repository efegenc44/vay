use crate::{expression::pattern::Pattern, interner::InternIdx, location::Located};

#[derive(Clone)]
pub struct T {
    before: Vec<Located<Pattern>>,
    after: Vec<Located<Pattern>>,
    rest: Option<InternIdx>,
}

impl T {
    pub fn new(
        before: Vec<Located<Pattern>>,
        after: Vec<Located<Pattern>>,
        rest: Option<InternIdx>
    ) -> Self {
        Self { before, after, rest }
    }

    pub fn before(&self) -> &[Located<Pattern>] {
        &self.before
    }

    pub fn after(&self) -> &[Located<Pattern>] {
        &self.after
    }

    pub fn rest(&self) -> Option<InternIdx> {
        self.rest
    }
}
