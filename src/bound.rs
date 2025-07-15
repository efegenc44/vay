use crate::interner::{InternIdx, Interner};

#[derive(Clone)]
pub enum Bound {
    Undetermined,
    Local(BoundIdx),
    Absolute(Path),
}

impl Bound {
    pub fn local(index: usize) -> Self {
        Self::Local(BoundIdx(index))
    }

    pub fn absolute(path: Vec<InternIdx>) -> Self {
        Self::Absolute(Path(path))
    }
}

#[derive(Clone)]
pub struct BoundIdx(usize);

impl BoundIdx {
    pub fn idx(&self) -> usize {
        self.0
    }
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

    pub fn as_string(&self, interner: &Interner) -> String {
        self.0
            .iter()
            .map(|intern_idx| interner.get(intern_idx))
            .collect::<Vec<_>>()
            .join("::")
    }
}
