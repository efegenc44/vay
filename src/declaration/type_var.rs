use crate::{bound::Path, interner::InternIdx, location::Located};

pub struct T {
    name: Located<InternIdx>,
    interfaces: Vec<(Located<Vec<InternIdx>>, Path)>
}

impl T {
    pub fn new(name: Located<InternIdx>, interfaces: Vec<(Located<Vec<InternIdx>>, Path)>) -> Self {
        Self { name, interfaces }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn interfaces(&self) -> &[(Located<Vec<InternIdx>>, Path)] {
        &self.interfaces
    }

    pub fn interfaces_mut(&mut self) -> &mut Vec<(Located<Vec<InternIdx>>, Path)> {
        &mut self.interfaces
    }
}
