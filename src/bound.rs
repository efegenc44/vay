use crate::interner::{interner, InternIdx};

#[derive(Clone)]
pub enum Bound {
    Undetermined,
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

    pub fn as_string(&self) -> String {
        let interner = interner();

        self.0
            .iter()
            .map(|intern_idx| interner.get(intern_idx).to_string())
            .collect::<Vec<_>>()
            .join("::")
    }
}
