use crate::{
    name::bound::Bound,
    interner::InternIdx,
    lex::location::Located
};

#[derive(Clone)]
pub enum TypeExpression {
    Path(Path),
    Function(Function),
    Application(Application),
    Unit
}

#[derive(Clone)]
pub struct Path {
    parts: Vec<InternIdx>,
    bound: Bound
}

impl Path {
    pub fn new(parts: Vec<InternIdx>) -> Self {
        Self {
            parts,
            bound: Bound::Undetermined
        }
    }

    pub fn parts(&self) -> &[InternIdx] {
        &self.parts
    }

    pub fn bound(&self) -> &Bound {
        &self.bound
    }

    pub fn set_bound(&mut self, bound: Bound) {
        self.bound = bound;
    }
}

#[derive(Clone)]
pub struct Application {
    function: Box<Located<TypeExpression>>,
    arguments: Vec<Located<TypeExpression>>,
}

impl Application {
    pub fn new(function: Box<Located<TypeExpression>>, arguments: Vec<Located<TypeExpression>>) -> Self {
        Self { function, arguments }
    }

    pub fn function(&self) -> &Located<TypeExpression> {
        &self.function
    }

    pub fn function_mut(&mut self) -> &mut Box<Located<TypeExpression>> {
        &mut self.function
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<TypeExpression>> {
        &mut self.arguments
    }
}

#[derive(Clone)]
pub struct Function {
    arguments: Vec<Located<TypeExpression>>,
    return_type: Option<Box<Located<TypeExpression>>>
}

impl Function {
    pub fn new(arguments: Vec<Located<TypeExpression>>, return_type: Option<Box<Located<TypeExpression>>>) -> Self {
        Self { arguments, return_type }
    }

    pub fn arguments(&self) -> &[Located<TypeExpression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<TypeExpression>> {
        &mut self.arguments
    }

    pub fn return_type(&self) -> Option<&Box<Located<TypeExpression>>> {
        self.return_type.as_ref()
    }

    pub fn return_type_mut(&mut self) -> &mut Option<Box<Located<TypeExpression>>> {
        &mut self.return_type
    }
}