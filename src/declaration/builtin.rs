use crate::{
    declaration,
    bound::Path,
    expression::Expression,
    interner::InternIdx,
    location::Located
};


pub struct T {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    methods: Vec<(declaration::MethodSignature, Option<Located<Expression>>)>,
    path: Path,
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        methods: Vec<(declaration::MethodSignature, Option<Located<Expression>>)>
    ) -> Self {
        Self {
            name,
            type_vars,
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

    pub fn methods(&self) -> &[(declaration::MethodSignature, Option<Located<Expression>>)] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<(declaration::MethodSignature, Option<Located<Expression>>)> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}