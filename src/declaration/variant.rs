use crate::{
    bound::Path,
    declaration,
    interner::InternIdx,
    location::Located,
    type_expression::TypeExpression
};

pub struct T {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    cases: Vec<Located<Case>>,
    methods: Vec<declaration::Method>,
    path: Path,
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        cases: Vec<Located<Case>>,
        methods: Vec<declaration::Method>,
    ) -> Self {
        Self {
            name,
            type_vars,
            cases,
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

    pub fn cases(&self) -> &[Located<Case>] {
        &self.cases
    }

    pub fn cases_mut(&mut self) -> &mut Vec<Located<Case>> {
        &mut self.cases
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

pub struct Case {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypeExpression>>>,
    path: Path,
}

impl Case {
    pub fn new(identifier: Located<InternIdx>, arguments: Option<Vec<Located<TypeExpression>>>) -> Self {
        Self {
            identifier,
            arguments,
            path: Path::empty()
        }
    }

    pub fn arguments(&self) -> Option<&Vec<Located<TypeExpression>>> {
        self.arguments.as_ref()
    }

    pub fn arguments_mut(&mut self) -> &mut Option<Vec<Located<TypeExpression>>> {
        &mut self.arguments
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}
