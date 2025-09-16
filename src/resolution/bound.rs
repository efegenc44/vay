use std::fmt::Display;

use crate::interner::{interner, InternIdx};

#[derive(Clone, Debug)]
pub enum Bound {
    Local(usize),
    Absolute(Path),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Path(Vec<InternIdx>);

impl Path {
    pub fn empty() -> Self {
        Self(vec![])
    }

    pub fn append(&self, name: InternIdx) -> Self {
        let mut path = self.clone();
        path.0.push(name);
        path
    }

    pub fn append_parts(&self, parts: &[InternIdx]) -> Self {
        let mut result = self.clone();
        result.0.extend(parts);
        result
    }

    pub fn push(&mut self, name: InternIdx) {
        self.0.push(name);
    }

    pub fn pop(&mut self) -> Option<InternIdx> {
        self.0.pop()
    }

    pub fn starts_with(&self, path: &Path) -> bool {
        self.0.starts_with(&path.0)
    }

    pub fn last(&self) -> InternIdx {
        *self.0.last().unwrap()
    }
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let interner = interner();

        let path = self.0
            .iter()
            .map(|intern_idx| interner.get(intern_idx))
            .collect::<Vec<_>>()
            .join("::");

        write!(f, "{}", path)
    }
}
