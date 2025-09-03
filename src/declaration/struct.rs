use crate::{
    bound::Path,
    declaration::{self, TypedIdentifier},
    interner::InternIdx,
    location::Located
};

pub struct T {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    fields: Vec<Located<TypedIdentifier>>,
    methods: Vec<declaration::Method>,
    path: Path,
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        fields: Vec<Located<TypedIdentifier>>,
        methods: Vec<declaration::Method>,
    ) -> Self {
        Self {
            name,
            type_vars,
            fields,
            methods,
            path: Path::empty()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_vars(&self) -> &[Located<InternIdx>] {
        &self.type_vars
    }

    pub fn fields(&self) -> &[Located<TypedIdentifier>] {
        &self.fields
    }

    pub fn fields_mut(&mut self) -> &mut Vec<Located<TypedIdentifier>> {
        &mut self.fields
    }

    pub fn methods(&self) -> &[declaration::Method] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<declaration::Method> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}


