use crate::{
    expression::TypeExpression, interner::InternIdx, location::Located, statement::Statement,
};

pub enum Declaration {
    Module {
        name: Located<InternIdx>,
    },
    Import {
        name: Located<InternIdx>,
    },
    Procedure {
        name: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        body: Vec<Located<Statement>>,
    },
    Variant {
        name: Located<InternIdx>,
        cases: Vec<Located<VariantCase>>,
        methods: Vec<Method>,
    },
}

pub struct Method {
    pub name: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub body: Vec<Located<Statement>>,
}

impl Method {
    pub fn new(
        name: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        body: Vec<Located<Statement>>,
    ) -> Self {
        Self {
            name,
            arguments,
            body,
        }
    }
}

pub struct VariantCase {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypedIdentifier>>>,
}

impl VariantCase {
    pub fn new(name: Located<InternIdx>, arguments: Option<Vec<Located<TypedIdentifier>>>) -> Self {
        Self {
            identifier: name,
            arguments,
        }
    }

    pub fn arguments_mut(&mut self) -> &mut Option<Vec<Located<TypedIdentifier>>> {
        &mut self.arguments
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }
}

pub struct TypedIdentifier {
    identifier: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
}

impl TypedIdentifier {
    pub fn new(identifier: Located<InternIdx>, type_expression: Located<TypeExpression>) -> Self {
        Self {
            identifier,
            type_expression,
        }
    }

    pub fn indentifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn type_expression_mut(&mut self) -> &mut Located<TypeExpression> {
        &mut self.type_expression
    }
}

pub struct Module {
    declarations: Vec<Declaration>,
    source: String,
}

impl Module {
    pub fn new(declarations: Vec<Declaration>, source: String) -> Self {
        Self {
            declarations,
            source,
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
}
