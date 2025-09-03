use crate::{interner::InternIdx, location::Located};

pub struct T {
    parts: Located<Vec<InternIdx>>,
}

impl T {
    pub fn new(parts: Located<Vec<InternIdx>>) -> Self {
        Self { parts }
    }

    pub fn parts(&self) -> &Located<Vec<InternIdx>> {
        &self.parts
    }
}
