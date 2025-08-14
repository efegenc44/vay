use std::fmt::Debug;

static mut INTERNER: Interner = Interner::new();

// NOTE : These functions are a bit hacky but they are fine,
//   because everything is single-threaded and also
//   if there will be multiple threads in the future
//   changing just the implementation of these getter
//   functions with RwLock counter parts
//   is going to be enough I think, though it may cause
//   deadlocks in places both interner and interner_mut
//   used in the same scope because Rust unlocks the lock when
//   the lock is dropped.

pub fn interner() -> &'static Interner {
    unsafe { (&raw const INTERNER).as_ref().unwrap() }
}

pub fn interner_mut() -> &'static mut Interner {
    unsafe { (&raw mut INTERNER).as_mut().unwrap() }
}

#[derive(Debug)]
pub struct Interner {
    strings: Vec<String>,
}

impl Interner {
    const fn new() -> Self {
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

    pub fn intern_idx(&self, interned: &str) -> InternIdx {
        self.strings
            .iter().position(|string| string == interned)
            .map(InternIdx)
            .unwrap()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct InternIdx(usize);

impl InternIdx {
    pub const fn dummy_idx() -> Self {
        Self(0)
    }
}
