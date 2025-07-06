use std::fmt::Debug;

#[derive(Debug)]
pub struct Interner {
    strings: Vec<String>,
}

impl Interner {
    pub const fn new() -> Self {
        Self { strings: vec![] }
    }

    pub fn intern(&mut self, new_string: String) -> InternIdx {
        if let Some(idx) = self.strings.iter().position(|string| string == &new_string) {
            InternIdx(idx)
        } else {
            self.strings.push(new_string);
            InternIdx(self.strings.len() - 1)
        }
    }

    pub fn get(&self, intern_idx: &InternIdx) -> &str {
        &self.strings[intern_idx.0]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct InternIdx(usize);

impl InternIdx {
    pub const fn dummy_idx() -> Self {
        Self(0)
    }
}
