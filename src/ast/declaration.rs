use std::cell::OnceCell;

use crate::{
    resolution::bound::Path,
    ast::{
        expression::Expression,
        type_expression::TypeExpression
    },
    interner::InternIdx,
    lex::location::Located,
};

pub enum Declaration {
    ModulePath(ModulePath),
    Define(Define),
    Import(Import),
    Function(Function),
    Variant(Variant),
    Interface(Interface),
    Struct(Struct),
    BuiltIn(BuiltIn),
    External(External),
}


pub struct ModulePath {
    parts: Located<Vec<InternIdx>>,
}

impl ModulePath {
    pub fn new(parts: Located<Vec<InternIdx>>) -> Self {
        Self { parts }
    }

    pub fn parts(&self) -> &Located<Vec<InternIdx>> {
        &self.parts
    }
}

pub struct Define {
    name: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
    expression: Located<Expression>,
    path: OnceCell<Path>
}

impl Define {
    pub fn new(
        name: Located<InternIdx>,
        type_expression: Located<TypeExpression>,
        expression: Located<Expression>
    ) -> Self {
        Self {
            name,
            type_expression,
            expression,
            path: OnceCell::new()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_expression(&self) -> &Located<TypeExpression> {
        &self.type_expression
    }

    pub fn type_expression_mut(&mut self) -> &mut Located<TypeExpression> {
        &mut self.type_expression
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct Import {
    import_in: bool,
    name: Located<InternIdx>,
    subnames: Option<Vec<Import>>,
    as_name: Option<Located<InternIdx>>
}

impl Import {
    pub fn new(
        import_in: bool,
        name: Located<InternIdx>,
        subnames: Option<Vec<Import>>,
        as_name: Option<Located<InternIdx>>
    ) -> Self {
        Self { import_in, name, subnames, as_name }
    }

    pub fn is_import_in(&self) -> bool {
        self.import_in
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn subnames(&self) -> Option<&Vec<Import>> {
        self.subnames.as_ref()
    }

    pub fn as_name(&self) -> Option<Located<InternIdx>> {
        self.as_name
    }
}

pub struct Function {
    name: Located<InternIdx>,
    type_vars: Vec<Located<TypeVar>>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
    body: Located<Expression>,
    path: OnceCell<Path>,
}

impl Function {
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
            path: OnceCell::new()
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
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}


pub struct Variant {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    cases: Vec<Located<VariantCase>>,
    methods: Vec<Method>,
    path: OnceCell<Path>,
}

impl Variant {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        cases: Vec<Located<VariantCase>>,
        methods: Vec<Method>,
    ) -> Self {
        Self {
            name,
            type_vars,
            cases,
            methods,
            path: OnceCell::new()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_vars(&self) -> &[Located<InternIdx>] {
        &self.type_vars
    }

    pub fn cases(&self) -> &[Located<VariantCase>] {
        &self.cases
    }

    pub fn cases_mut(&mut self) -> &mut Vec<Located<VariantCase>> {
        &mut self.cases
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<Method> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct VariantCase {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypeExpression>>>,
    path: OnceCell<Path>,
}

impl VariantCase {
    pub fn new(identifier: Located<InternIdx>, arguments: Option<Vec<Located<TypeExpression>>>) -> Self {
        Self {
            identifier,
            arguments,
            path: OnceCell::new()
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
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct Interface {
    name: Located<InternIdx>,
    type_name: Located<TypeVar>,
    methods: Vec<InterfaceSignature>,
    path: OnceCell<Path>,
}

impl Interface {
    pub fn new(
        name: Located<InternIdx>,
        type_name: Located<TypeVar>,
        methods: Vec<InterfaceSignature>
    ) -> Self {
        Self {
            name,
            type_name,
            methods,
            path: OnceCell::new()
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

    pub fn methods(&self) -> &[InterfaceSignature] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<InterfaceSignature> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct InterfaceSignature {
    name: Located<InternIdx>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
    path: OnceCell<Path>,
}

impl InterfaceSignature {
    pub fn new(
        name: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Option<Located<TypeExpression>>,
    ) -> Self {
        Self {
            name,
            arguments,
            return_type,
            path: OnceCell::new()
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
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct Struct {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    fields: Vec<Located<TypedIdentifier>>,
    methods: Vec<Method>,
    path: OnceCell<Path>,
}

impl Struct {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        fields: Vec<Located<TypedIdentifier>>,
        methods: Vec<Method>,
    ) -> Self {
        Self {
            name,
            type_vars,
            fields,
            methods,
            path: OnceCell::new()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_vars(&self) -> &[Located<InternIdx>] {
        &self.type_vars
    }

    pub fn fields(&self) -> &[Located<TypedIdentifier>] {
        &self.fields
    }

    pub fn fields_mut(&mut self) -> &mut Vec<Located<TypedIdentifier>> {
        &mut self.fields
    }

    pub fn methods(&self) -> &[Method] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<Method> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct BuiltIn {
    name: Located<InternIdx>,
    type_vars: Vec<Located<InternIdx>>,
    methods: Vec<(MethodSignature, Option<Located<Expression>>)>,
    path: OnceCell<Path>,
}

impl BuiltIn {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<InternIdx>>,
        methods: Vec<(MethodSignature, Option<Located<Expression>>)>
    ) -> Self {
        Self {
            name,
            type_vars,
            methods,
            path: OnceCell::new()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_vars(&self) -> &[Located<InternIdx>] {
        &self.type_vars
    }

    pub fn methods(&self) -> &[(MethodSignature, Option<Located<Expression>>)] {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut Vec<(MethodSignature, Option<Located<Expression>>)> {
        &mut self.methods
    }

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct External {
    name: Located<InternIdx>,
    type_vars: Vec<Located<TypeVar>>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
    path: OnceCell<Path>,
}

impl External {
    pub fn new(
        name: Located<InternIdx>,
        type_vars: Vec<Located<TypeVar>>,
        arguments: Vec<Located<TypedIdentifier>>,
        return_type: Option<Located<TypeExpression>>) -> Self {
        Self {
            name,
            type_vars,
            arguments,
            return_type,
            path: OnceCell::new()
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

    pub fn path(&self) -> &Path {
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}

pub struct Method {
    signature: MethodSignature,
    body: Located<Expression>,
}

impl Method {
    pub fn new(signature: MethodSignature, body: Located<Expression>) -> Self {
        Self { signature, body }
    }

    pub fn signature(&self) -> &MethodSignature {
        &self.signature
    }

    pub fn signature_mut(&mut self) -> &mut MethodSignature {
        &mut self.signature
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Located<Expression> {
        &mut self.body
    }
}

pub struct MethodSignature {
    name: Located<InternIdx>,
    constraints: Vec<MethodConstraint>,
    type_vars: Vec<Located<TypeVar>>,
    instance: Located<InternIdx>,
    arguments: Vec<Located<TypedIdentifier>>,
    return_type: Option<Located<TypeExpression>>,
}

impl MethodSignature {
    pub fn new(
        name: Located<InternIdx>,
        constraints: Vec<MethodConstraint>,
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

    pub fn constraints(&self) -> &[MethodConstraint] {
        &self.constraints
    }

    pub fn constraints_mut(&mut self) -> &mut Vec<MethodConstraint> {
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

pub struct MethodConstraint {
    nth: OnceCell<usize>,
    type_var: Located<TypeVar>
}

impl MethodConstraint {
    pub fn new(type_var: Located<TypeVar>) -> Self {
        Self {
            nth: OnceCell::new(),
            type_var
        }
    }

    pub fn nth(&self) -> usize {
        *self.nth.get().unwrap()
    }

    pub fn set_nth(&mut self, nth: usize) {
        self.nth.set(nth).unwrap();
    }

    pub fn type_var(&self) -> &Located<TypeVar> {
        &self.type_var
    }

    pub fn type_var_mut(&mut self) -> &mut Located<TypeVar> {
        &mut self.type_var
    }
}

#[derive(Clone)]
pub struct TypedIdentifier {
    identifier: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
}

impl TypedIdentifier {
    pub fn new(identifier: Located<InternIdx>, type_expression: Located<TypeExpression>) -> Self {
        Self { identifier, type_expression }
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn type_expression(&self) -> &Located<TypeExpression> {
        &self.type_expression
    }

    pub fn type_expression_mut(&mut self) -> &mut Located<TypeExpression> {
        &mut self.type_expression
    }
}

pub struct TypeVar {
    name: Located<InternIdx>,
    interfaces: Vec<(Located<Vec<InternIdx>>, Path)>
}

impl TypeVar {
    pub fn new(name: Located<InternIdx>, interfaces: Vec<(Located<Vec<InternIdx>>, Path)>) -> Self {
        Self { name, interfaces }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn interfaces(&self) -> &[(Located<Vec<InternIdx>>, Path)] {
        &self.interfaces
    }

    pub fn interfaces_mut(&mut self) -> &mut Vec<(Located<Vec<InternIdx>>, Path)> {
        &mut self.interfaces
    }
}

pub struct Module {
    declarations: Vec<Declaration>,
    source: String,
    path: OnceCell<Path>
}

impl Module {
    pub fn new(declarations: Vec<Declaration>, source: String) -> Self {
        Self {
            declarations,
            source,
            path: OnceCell::new()
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
        self.path.get().unwrap()
    }

    pub fn set_path(&mut self, path: Path) {
        self.path.set(path).unwrap();
    }
}