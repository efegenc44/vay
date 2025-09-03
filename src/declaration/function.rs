use crate::{
    bound::Path,
    declaration::{TypeVar, TypedIdentifier},
    expression::Expression,
    interner::InternIdx,
    location::Located,
    type_expression::TypeExpression
};

pub struct T {
    name: Located<InternIdx>,
    type_vars: Vec<Located<TypeVar>>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
    body: Located<Expression>,
    path: Path,
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<TypeVar>>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Option<Located<TypeExpression>>,
        body: Located<Expression>) -> Self {
        Self {
            name,
            type_vars,
            arguments,
            return_type,
            body,
            path: Path::empty()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_vars(&self) -> &[Located<TypeVar>] {
        &self.type_vars
    }

    pub fn type_vars_mut(&mut self) -> &mut Vec<Located<TypeVar>> {
        &mut self.type_vars
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

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Located<Expression> {
        &mut self.body
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}