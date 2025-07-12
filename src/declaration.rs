use crate::{
    bound::Path, expression::TypeExpression, interner::InternIdx, location::Located,
    statement::Statement,
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
        return_type: Located<TypeExpression>,
        body: Vec<Located<Statement>>,
        path: Path,
    },
    Variant {
        name: Located<InternIdx>,
        cases: Vec<Located<VariantCase>>,
        methods: Vec<Method>,
        path: Path,
    },
}

pub struct Method {
    pub name: Located<InternIdx>,
    pub this: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub body: Vec<Located<Statement>>,
}

impl Method {
    pub fn new(
        name: Located<InternIdx>,
        this: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Located<TypeExpression>,
        body: Vec<Located<Statement>>,
    ) -> Self {
        Self {
            name,
            this,
            arguments,
            return_type,
            body,
        }
    }
}

pub struct VariantCase {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypedIdentifier>>>,
    path: Path,
}

impl VariantCase {
    pub fn new(
        name: Located<InternIdx>,
        arguments: Option<Vec<Located<TypedIdentifier>>>,
        path: Path,
    ) -> Self {
        Self {
            identifier: name,
            arguments,
            path,
        }
    }

    pub fn arguments(&self) -> Option<&Vec<Located<TypedIdentifier>>> {
        self.arguments.as_ref()
    }

    pub fn arguments_mut(&mut self) -> &mut Option<Vec<Located<TypedIdentifier>>> {
        &mut self.arguments
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn path_mut(&mut self) -> &mut Path {
        &mut self.path
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

    pub fn type_expression(&self) -> &Located<TypeExpression> {
        &self.type_expression
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
