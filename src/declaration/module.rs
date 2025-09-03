use crate::{bound::Path, declaration::Declaration};

pub struct T {
    declarations: Vec<Declaration>,
    source: String,
    path: Path
}

impl T {
    pub fn new(declarations: Vec<Declaration>, source: String) -> Self {
        Self {
            declarations,
            source,
            path: Path::empty()
        }
    }

    pub fn declarations(&self) -> &[Declaration] {
        &self.declarations
    }

    pub fn declarations_mut(&mut self) -> &mut [Declaration] {
        &mut self.declarations
    }

    pub fn source(&self) -> &str {
        &self.source
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}
