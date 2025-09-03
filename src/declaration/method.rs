use crate::{
    declaration::{TypeVar, TypedIdentifier},
    expression::Expression,
    interner::InternIdx,
    location::Located,
    type_expression::TypeExpression
};

pub struct T {
    signature: Signature,
    body: Located<Expression>,
}

impl T {
    pub fn new(signature: Signature, body: Located<Expression>) -> Self {
        Self { signature, body }
    }

    pub fn signature(&self) -> &Signature {
        &self.signature
    }

    pub fn signature_mut(&mut self) -> &mut Signature {
        &mut self.signature
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Located<Expression> {
        &mut self.body
    }
}

pub struct Signature {
    name: Located<InternIdx>,
    constraints: Vec<Constraint>,
    type_vars: Vec<Located<TypeVar>>,
    instance: Located<InternIdx>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
}

impl Signature {
    pub fn new(
        name: Located<InternIdx>,
        constraints: Vec<Constraint>,
        type_vars: Vec<Located<TypeVar>>,
        instance: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Option<Located<TypeExpression>>
    ) -> Self {
        Self { name, constraints, type_vars, instance, arguments, return_type }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn constraints(&self) -> &[Constraint] {
        &self.constraints
    }

    pub fn constraints_mut(&mut self) -> &mut Vec<Constraint> {
        &mut self.constraints
    }

    pub fn type_vars(&self) -> &[Located<TypeVar>] {
        &self.type_vars
    }

    pub fn type_vars_mut(&mut self) -> &mut Vec<Located<TypeVar>> {
        &mut self.type_vars
    }

    pub fn instance(&self) -> Located<InternIdx> {
        self.instance
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
}

pub struct Constraint {
    nth: usize,
    type_var: Located<TypeVar>
}

impl Constraint {
    pub fn new(type_var: Located<TypeVar>) -> Self {
        Self {
            nth: usize::default(),
            type_var
        }
    }

    pub fn nth(&self) -> usize {
        self.nth
    }

    pub fn set_nth(&mut self, nth: usize) {
        self.nth = nth;
    }

    pub fn type_var(&self) -> &Located<TypeVar> {
        &self.type_var
    }

    pub fn type_var_mut(&mut self) -> &mut Located<TypeVar> {
        &mut self.type_var
    }
}