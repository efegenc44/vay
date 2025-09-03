use crate::{bound::Bound, interner::InternIdx};


#[derive(Clone)]
pub struct T {
    parts: Vec<InternIdx>,
    bound: Bound
}

impl T {
    pub fn new(parts: Vec<InternIdx>) -> Self {
        Self {
            parts,
            bound: Bound::Undetermined
        }
    }

    pub fn parts(&self) -> &[InternIdx] {
        &self.parts
    }

    pub fn bound(&self) -> &Bound {
        &self.bound
    }

    pub fn set_bound(&mut self, bound: Bound) {
        self.bound = bound;
    }
}