use crate::{
    bound::Path, expression::TypeExpression, interner::InternIdx, location::Located,
    statement::Statement,
};

pub enum Declaration {
    Module(ModuleDeclaration),
    Import(ImportDeclaration),
    Procedure(ProcedureDeclaration),
    Variant(VariantDeclaration),
}

pub struct ModuleDeclaration {
    pub name: Located<InternIdx>,
}

pub struct ImportDeclaration {
    pub name: Located<InternIdx>,
}

pub struct ProcedureDeclaration {
    pub name: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub body: Vec<Located<Statement>>,
    pub path: Path,
}

pub struct VariantDeclaration {
    pub name: Located<InternIdx>,
    pub cases: Vec<Located<VariantCase>>,
    pub methods: Vec<MethodDeclaration>,
    pub path: Path,
}

pub struct MethodDeclaration {
    pub name: Located<InternIdx>,
    pub instance: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub body: Vec<Located<Statement>>,
}

pub struct VariantCase {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypeExpression>>>,
    path: Path,
}

impl VariantCase {
    pub fn new(
        identifier: Located<InternIdx>,
        arguments: Option<Vec<Located<TypeExpression>>>,
        path: Path,
    ) -> Self {
        Self {
            identifier,
            arguments,
            path,
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

    pub fn path_mut(&mut self) -> &mut Path {
        &mut self.path
    }
}

#[derive(Clone)]
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
    name: InternIdx
}

impl Module {
    pub fn new(declarations: Vec<Declaration>, source: String) -> Self {
        Self {
            declarations,
            source,
            name: InternIdx::dummy_idx()
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

    pub fn name(&mut self) -> InternIdx {
        self.name
    }

    pub fn name_mut(&mut self) -> &mut InternIdx {
        &mut self.name
    }
}
