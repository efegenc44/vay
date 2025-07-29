use crate::{
    bound::Path, expression::{Expression, TypeExpression}, interner::InternIdx, location::Located,
};

pub enum Declaration {
    Module(ModuleDeclaration),
    Import(ImportDeclaration),
    Function(FunctionDeclaration),
    Variant(VariantDeclaration),
    Interface(InterfaceDeclaration)
}

pub struct TypeVar {
    pub name: Located<InternIdx>,
    pub interfaces: Vec<(Located<Vec<InternIdx>>, Path)>
}

pub struct ModuleDeclaration {
    pub parts: Located<Vec<InternIdx>>,
}

pub struct ImportDeclaration {
    pub name: ImportName,
}

pub struct ImportName {
    pub name: Located<InternIdx>,
    pub subnames: Option<Vec<ImportName>>,
    pub as_name: Option<Located<InternIdx>>
}

pub struct FunctionDeclaration {
    pub name: Located<InternIdx>,
    pub type_vars: Vec<Located<TypeVar>>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub body: Located<Expression>,
    pub path: Path,
}

pub struct VariantDeclaration {
    pub name: Located<InternIdx>,
    pub type_vars: Vec<Located<InternIdx>>,
    pub cases: Vec<Located<VariantCase>>,
    pub methods: Vec<MethodDeclaration>,
    pub path: Path,
}

pub struct InterfaceDeclaration {
    pub name: Located<InternIdx>,
    pub type_name: Located<InternIdx>,
    pub methods: Vec<MethodSignature>,
    pub path: Path,
}

pub struct MethodSignature {
    pub name: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub path: Path,
}

pub struct MethodDeclaration {
    pub name: Located<InternIdx>,
    pub constraints: Vec<Constraint>,
    pub instance: Located<InternIdx>,
    pub arguments: Vec<Located<TypedIdentifier>>,
    pub return_type: Located<TypeExpression>,
    pub body: Located<Expression>,
}

pub struct Constraint {
    pub nth: usize,
    pub type_var: Located<TypeVar>
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
    path: Path
}

impl Module {
    pub fn new(declarations: Vec<Declaration>, source: String) -> Self {
        Self {
            declarations,
            source,
            path: Path::empty()
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

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn path_mut(&mut self) -> &mut Path {
        &mut self.path
    }
}
