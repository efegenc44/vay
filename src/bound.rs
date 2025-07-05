use crate::interner::InternIdx;

pub enum Bound {
    Undetermined,
    Local(BoundIdx),
    Absolute(Vec<InternIdx>),
}

impl Bound {
    pub fn local(index: usize) -> Self {
        Self::Local(BoundIdx(index))
    }

    pub fn absolute(path: Vec<InternIdx>) -> Self {
        Self::Absolute(path)
    }
}

pub struct BoundIdx(usize);
