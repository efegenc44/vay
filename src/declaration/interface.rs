use crate::{
    bound::Path,
    declaration::{TypeVar, TypedIdentifier},
    interner::InternIdx,
    location::Located,
    type_expression::TypeExpression
};

pub struct T {
    name: Located<InternIdx>,
    type_name: Located<TypeVar>,
    methods: Vec<Signature>,
    path: Path,
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_name: Located<TypeVar>,
        methods: Vec<Signature>
    ) -> Self {
        Self {
            name,
            type_name,
            methods,
            path: Path::empty()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_name(&self) -> &Located<TypeVar> {
        &self.type_name
    }

    pub fn type_name_mut(&mut self) -> &mut Located<TypeVar> {
        &mut self.type_name
    }

    pub fn methods(&self) -> &[Signature] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<Signature> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}

pub struct Signature {
    name: Located<InternIdx>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
    path: Path,
}

impl Signature {
    pub fn new(
        name: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Option<Located<TypeExpression>>,
    ) -> Self {
        Self {
            name,
            arguments,
            return_type,
            path: Path::empty()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn arguments(&self) -> &[Located<TypedIdentifier>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<TypedIdentifier>> {
        &mut self.arguments
    }

    pub fn return_type(&self) -> Option<&Located<TypeExpression>> {
        self.return_type.as_ref()
    }

    pub fn return_type_mut(&mut self) -> &mut Option<Located<TypeExpression>> {
        &mut self.return_type
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}