use std::collections::{HashMap, HashSet};

use crate::{
    bound::{Bound, Path},
    declaration::{
        BuiltInDeclaration, Constraint, Declaration, ExternalDeclaration,
        FunctionDeclaration, ImportDeclaration, ImportName, InterfaceDeclaration,
        InterfaceMethodSignature, MethodDeclaration, MethodSignature, Module,
        ModuleDeclaration, StructDeclaration, TypeVar, VariantDeclaration,
        DefineDeclaration
    },
    expression::{
        ApplicationExpression, ArrayExpression, ArrayPattern, AssignmentExpression,
        Expression, FunctionTypeExpression, LambdaExpression, LetExpression,
        MatchExpression, PathExpression, PathTypeExpression, Pattern,
        ProjectionExpression, ReturnExpression, SequenceExpression, TypeApplicationExpression,
        TypeExpression, VariantCasePattern, WhileExpression, BlockExpression
    },
    interner::{interner, InternIdx},
    intrinsics::INTRINSICS_MODULE_NAME,
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    core::CORE_MODULE_NAME,
    runner
};

macro_rules! scoped {
    ($self:expr, $body:block) => {
        {
            let locals_len = $self.locals.len();
            $body
            $self.locals.truncate(locals_len);
        }
    };
}

struct ModuleInformation {
    imports: HashMap<InternIdx, (Path, SourceLocation)>,
    import_ins: HashMap<Path, SourceLocation>,
    path_location: SourceLocation,
}

impl ModuleInformation {
    fn with_path_location(path_location: SourceLocation) -> Self {
        // NOTE : Automatically import Core and Intrinsics for all modules
        //   locations are dummy but they supposed to be present at this point
        //   so no error.
        let instrinsics = interner().intern_idx(INTRINSICS_MODULE_NAME);
        let intrinsics_path = Path::empty().append(instrinsics);

        let core = interner().intern_idx(CORE_MODULE_NAME);
        let core_path = Path::empty().append(core);

        let boole = interner().intern_idx("Bool");
        let boole_path = core_path.append(boole);

        let dummyloc = SourceLocation::dummy();

        let imports = HashMap::from([
            (instrinsics, (intrinsics_path.clone(), dummyloc)),
            (core, (core_path.clone(), dummyloc))
        ]);

        let import_ins = HashMap::from([
            (intrinsics_path.clone(), dummyloc),
            (core_path.clone(), dummyloc),
            (boole_path.clone(), dummyloc)
        ]);

        Self {
            imports,
            import_ins,
            path_location,
        }
    }
}

pub struct Resolver {
    modules: HashMap<Path, ModuleInformation>,

    // TODO: Seperete interface and type names
    type_names: HashSet<Path>,
    value_names: HashSet<Path>,

    defines: HashMap<Path, (Located<Expression>, bool)>,

    // TODO: Seperate type and value locals
    locals: Vec<InternIdx>,

    current_module_path: Path,
    current_source: String,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),

            type_names: HashSet::new(),
            value_names: HashSet::new(),

            defines: HashMap::new(),

            locals: vec![],

            current_module_path: Path::empty(),
            current_source: String::new(),
        }
    }

    pub fn init_interactive_module(&mut self) {
        self.current_module_path = Path::empty();
        self.current_source = runner::SESSION_SOURCE.into();

        self.modules.insert(
            Path::empty(),
            ModuleInformation::with_path_location(SourceLocation::dummy())
        );
    }

    fn current_imports(&self) -> &HashMap<InternIdx, (Path, SourceLocation)> {
        &self.modules[&self.current_module_path].imports
    }

    fn current_import_ins(&self) -> &HashMap<Path, SourceLocation> {
        &self.modules[&self.current_module_path].import_ins
    }

    fn current_path(&self) -> &Path {
        &self.current_module_path
    }

    fn current_path_mut(&mut self) -> &mut Path {
        &mut self.current_module_path
    }

    pub fn resolve(&mut self, mut modules: Vec<Module>) -> ReportableResult<Vec<Module>> {
        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.collect_module(module)?;
        }

        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.module_path(module)?;
        }

        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.current_module_path = module.path().clone();
            self.collect_names(module)?;
        }

        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.current_module_path = module.path().clone();
            self.module(module)?;
        }

        Ok(modules)
    }

    fn module(&mut self, module: &mut Module) -> ReportableResult<()> {
        self.import_names()?;

        for declaration in module.declarations_mut() {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn collect_module(&mut self, module: &mut Module) -> ReportableResult<()> {
        let mut module_path = None;
        let mut declared = false;
        for declaration in module.declarations() {
            if let Declaration::Module(module) = declaration {
                let ModuleDeclaration { parts } = module;

                if !declared {
                    declared = true;
                    let path = Path::empty().append_parts(parts.data());
                    module_path = Some(path.clone());
                    if self.modules.insert(path.clone(), ModuleInformation::with_path_location(parts.location())).is_some() {
                        // Without this error, behaviour is extending the existing module
                        //   maybe that's an interesting idea!
                        return self.error(ResolveError::CollidingModulePaths(path), parts.location())
                    }
                } else {
                    return self.error(ResolveError::DuplicateModuleDeclaration, parts.location());
                }
            }
        }

        let Some(module_name) = module_path else {
            return self.error(ResolveError::ModuleIsNotDeclared, SourceLocation::dummy());
        };
        *module.path_mut() = module_name.clone();

        let module_information = self.modules.get_mut(&module_name).unwrap();
        for declaration in module.declarations() {
            if let Declaration::Import(import) = declaration {
                let ImportDeclaration { name } = import;

                Self::module_imports(name, module_information);
            }
        }

        Ok(())
    }

    fn module_imports(import_name: &ImportName, module_information: &mut ModuleInformation) {
        fn f(import_name: &ImportName, path: Path, module_information: &mut ModuleInformation) {
            let import_path = path.append(*import_name.name.data());

            let name = if let Some(as_name) = import_name.as_name {
                as_name
            } else {
                import_name.name
            };

            if import_name.import_in {
                module_information.import_ins.insert(
                    import_path.clone(),
                    name.location()
                );
            }

            module_information.imports.insert(
                *name.data(),
                (import_path.clone(), name.location())
            );

            if let Some(subnames) = &import_name.subnames {
                for import_name in subnames {
                    f(import_name, import_path.clone(), module_information);
                }
            }
        }

        f(import_name, Path::empty(), module_information);
    }

    fn collect_names(&mut self, module: &mut Module) -> ReportableResult<()> {
        for declaration in module.declarations_mut() {
            match declaration {
                Declaration::Module(..) => {}
                Declaration::Import(..) => {}
                Declaration::Define(define) => self.collect_define_name(define)?,
                Declaration::Function(precodure) => self.collect_function_name(precodure)?,
                Declaration::Variant(variant) => self.collect_variant_name(variant)?,
                Declaration::Interface(interface) => self.collect_interface_name(interface)?,
                Declaration::Struct(strct) => self.collect_struct_name(strct)?,
                Declaration::BuiltIn(builtin) => self.collect_builtin_name(builtin)?,
                Declaration::External(external) => self.collect_external_name(external)?,
            }
        }

        Ok(())
    }

    fn collect_define_name(&mut self, define: &mut DefineDeclaration) -> ReportableResult<()> {
        let DefineDeclaration { name, path, expression, .. } = define;

        let define_path = self.current_path().append(*name.data());
        if !self.value_names.contains(&define_path) {
            self.defines.insert(define_path.clone(), (expression.clone(), false));
            self.value_names.insert(define_path.clone());
            *path = define_path;
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(define_path),
                name.location(),
            );
        }

        Ok(())
    }

    fn collect_function_name(&mut self, function: &mut FunctionDeclaration) -> ReportableResult<()> {
        let FunctionDeclaration { name, path, .. } = function;

        let function_path = self.current_path().append(*name.data());
        if !self.value_names.contains(&function_path) {
            self.value_names.insert(function_path.clone());
            *path = function_path;
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(function_path),
                name.location(),
            );
        }

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &mut VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { name, cases, path, .. } = variant;

        let variant_path = self.current_path().append(*name.data());
        if self.type_names.contains(&variant_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(variant_path),
                name.location(),
            );
        }

        self.current_path_mut().push(*name.data());
        for case in cases.iter_mut() {
            let constructor = *case.data().identifier().data();
            let constructor_path = self.current_path().append(constructor);
            if !self.value_names.contains(&constructor_path) {
                self.value_names.insert(constructor_path.clone());
                *case.data_mut().path_mut() = constructor_path;
            } else {
                return self.error(
                    ResolveError::DuplicateConstructorDeclaration {
                        constructor,
                        variant_path,
                    },
                    case.data().identifier().location(),
                );
            }
        }
        self.current_path_mut().pop();
        self.type_names.insert(variant_path.clone());
        *path = variant_path;

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &mut InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { name, path, methods, .. } = interface;

        let interface_path = self.current_path().append(*name.data());
        if self.type_names.contains(&interface_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(interface_path),
                name.location(),
            );
        }

        self.type_names.insert(interface_path.clone());
        *path = interface_path;

        self.current_path_mut().push(*name.data());
        for method in methods {
            let function_path = self.current_path().append(*method.name.data());
            method.path = function_path.clone();
            // TODO: Check for duplicate interface method declarations
            self.value_names.insert(function_path);
        }
        self.current_path_mut().pop();

        Ok(())
    }

    fn collect_struct_name(&mut self, strct: &mut StructDeclaration) -> ReportableResult<()> {
        let StructDeclaration { name, path, .. } = strct;

        let struct_path = self.current_path().append(*name.data());
        if self.type_names.contains(&struct_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(struct_path),
                name.location(),
            );
        }

        self.type_names.insert(struct_path.clone());
        self.value_names.insert(struct_path.clone());
        *path = struct_path;

        Ok(())
    }

    fn collect_builtin_name(&mut self, builtin: &mut BuiltInDeclaration) -> ReportableResult<()> {
        let BuiltInDeclaration { name, path, .. } = builtin;

        if self.current_path().to_string() != INTRINSICS_MODULE_NAME {
            panic!("Not allowed in builtin declarations outside of Intrinsics Module.")
        }

        let builtin = self.current_path().append(*name.data());
        if self.type_names.contains(&builtin) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(builtin),
                name.location(),
            );
        }

        self.type_names.insert(builtin.clone());
        *path = builtin;

        Ok(())
    }

    fn collect_external_name(&mut self, external: &mut ExternalDeclaration) -> ReportableResult<()> {
        let ExternalDeclaration { name, path, .. } = external;

        let external = self.current_path().append(*name.data());
        if !self.value_names.contains(&external) {
            self.value_names.insert(external.clone());
            *path = external;
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(external),
                name.location(),
            );
        }

        Ok(())
    }

    fn find_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // Local Scope
        for (index, name_idx) in self.locals.iter().rev().enumerate() {
            if name_idx == intern_idx {
                return Some(Bound::Local(index));
            }
        }

        if let Some((path, _)) = self.current_imports().get(intern_idx).cloned() {
            return Some(Bound::Absolute(path))
        }

        for path in self.current_import_ins().keys() {
            let path = path.append(*intern_idx);
            if self.value_names.contains(&path) || self.type_names.contains(&path) {
                return Some(Bound::Absolute(path));
            }
        }

        None
    }

    fn find_type_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // Local Scope
        for (index, name_idx) in self.locals.iter().rev().enumerate() {
            if name_idx == intern_idx {
                return Some(Bound::Local(index));
            }
        }

        if let Some((path, _)) = self.current_imports().get(intern_idx).cloned() {
            return Some(Bound::Absolute(path))
        }

        for path in self.current_import_ins().keys() {
            let path = path.append(*intern_idx);
            if self.type_names.contains(&path) {
                return Some(Bound::Absolute(path));
            }
        }

        None
    }

    fn find_interface_path(&self, parts: &[InternIdx], location: SourceLocation) -> ReportableResult<Path> {
        let base = if let Some((path, _)) = self.current_imports().get(&parts[0]).cloned() {
            path
        } else {
            let mut interface_path = None;
            for path in self.current_import_ins().keys() {
                let path = path.append(parts[0]);
                if self.type_names.contains(&path) {
                    interface_path = Some(path);
                }
            }

            if let Some(interface_path) = interface_path {
                interface_path
            } else {
                self.current_path().append(parts[0])
            }
        };

        let path = base.append_parts(&parts[1..]);
        let Some(path) = self.type_names.get(&path) else {
            return self.error(
                ResolveError::UnboundInterfacePath(Path::empty().append_parts(parts)),
                location
            );
        };

        Ok(path.clone())
    }

    pub fn expression(&mut self, expression: &mut Located<Expression>) -> ReportableResult<()> {
        let location = expression.location();
        match expression.data_mut() {
            Expression::U64(_) |
            Expression::F32(_) |
            Expression::String(_) => Ok(()),
            Expression::Path(path) => self.path(path, location),
            Expression::Array(expressions) => self.array(expressions),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
            Expression::Let(lett) => self.lett(lett),
            Expression::Sequence(sequence) => self.sequence(sequence),
            Expression::Block(block) => self.block(block),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Match(matc) => self.matc(matc),
            Expression::Return(retrn) => self.retrn(retrn),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::While(whilee) => self.whilee(whilee),
            Expression::Continue |
            Expression::Break => Ok(())
        }
    }

    fn path(&mut self, path: &mut PathExpression, location: SourceLocation) -> ReportableResult<()> {
        let PathExpression { parts, bound } = path;

        let base = self
            .find_name(&parts[0])
            .unwrap_or(Bound::Absolute(self.current_path().append(parts[0])));

        match base {
            Bound::Local(_) => {
                assert!(parts.len() == 1);
                *bound = base
            }
            Bound::Absolute(base_path) => {
                let path = base_path.append_parts(&parts[1..]);
                let Some(path) = self.value_names.get(&path) else {
                    return self.error(ResolveError::UnboundValuePath(path), location);
                };
                *bound = Bound::Absolute(path.clone());
            }
            Bound::Undetermined => unreachable!(),
        };

        Ok(())
    }

    fn array(&mut self, array: &mut ArrayExpression) -> ReportableResult<()> {
        let ArrayExpression { expressions } = array;

        for expression in expressions {
            self.expression(expression)?;
        }

        Ok(())
    }

    fn application(&mut self, application: &mut ApplicationExpression) -> ReportableResult<()> {
        let ApplicationExpression { function, arguments } = application;

        self.expression(function)?;
        for argument in arguments {
            self.expression(argument)?;
        }

        Ok(())
    }

    fn projection(&mut self, projection: &mut ProjectionExpression) -> ReportableResult<()> {
        let ProjectionExpression { expression, .. } = projection;

        self.expression(expression)
    }

    fn lett(&mut self, lett: &mut LetExpression) -> ReportableResult<()> {
        let LetExpression { name, value_expression, body_expression } = lett;

        self.expression(value_expression)?;

        scoped!(self, {
            self.locals.push(*name.data());
            self.expression(body_expression)?;
        });

        Ok(())
    }

    fn sequence(&mut self, sequence: &mut SequenceExpression) -> ReportableResult<()> {
        let SequenceExpression { expressions } = sequence;

        expressions
            .iter_mut().map(|expression| self.expression(expression))
            .collect::<ReportableResult<Vec<_>>>()
            .map(|_| ())
    }

    fn block(&mut self, block: &mut BlockExpression) -> ReportableResult<()> {
        let BlockExpression { expressions } = block;

        expressions
            .iter_mut().map(|expression| self.expression(expression))
            .collect::<ReportableResult<Vec<_>>>()
            .map(|_| ())
    }

    fn lambda(&mut self, lambda: &mut LambdaExpression) -> ReportableResult<()> {
        let LambdaExpression { arguments, body } = lambda;

        scoped!(self, {
            let argument_names = arguments.iter().map(|idx| *idx.data());
            self.locals.extend(argument_names);

            self.expression(body)?;
        });

        Ok(())
    }

    fn assignment(&mut self, assignment: &mut AssignmentExpression) -> ReportableResult<()> {
        let AssignmentExpression { assignable, expression } = assignment;

        self.expression(assignable)?;
        self.expression(expression)
    }

    fn whilee(&mut self, whilee: &mut WhileExpression) -> ReportableResult<()> {
        let WhileExpression { condition, post, body } = whilee;

        self.expression(condition)?;

        if let Some(post) = post {
            self.expression(post)?;
        }

        self.expression(body)
    }

    fn type_expression(&mut self, type_expression: &mut Located<TypeExpression>) -> ReportableResult<()> {
        let location = type_expression.location();
        match type_expression.data_mut() {
            TypeExpression::Path(path) => self.path_type(path, location),
            TypeExpression::Function(function_type) => self.function_type(function_type),
            // TODO: Inconsistent naming?
            TypeExpression::Application(type_application) => self.type_application(type_application),
            TypeExpression::Unit => Ok(()),
        }
    }

    fn path_type(&mut self, path: &mut PathTypeExpression, location: SourceLocation) -> ReportableResult<()> {
        let PathTypeExpression { parts, bound } = path;

        let base = self
            .find_type_name(&parts[0])
            .unwrap_or(Bound::Absolute(self.current_path().append(parts[0])));

        match base {
            Bound::Local(_) => {
                assert!(parts.len() == 1);
                *bound = base
            }
            Bound::Absolute(base_path) => {
                let path = base_path.append_parts(&parts[1..]);
                let Some(path) = self.type_names.get(&path) else {
                    return self.error(ResolveError::UnboundTypePath(path), location);
                };
                *bound = Bound::Absolute(path.clone());
            }
            Bound::Undetermined => unreachable!(),
        };

        Ok(())
    }

    fn function_type(&mut self, function_type: &mut FunctionTypeExpression) -> ReportableResult<()> {
        let FunctionTypeExpression { arguments, return_type } = function_type;

        for argument in arguments {
            self.type_expression(argument)?;
        }

        if let Some(return_type) = return_type {
            self.type_expression(return_type)?;
        }

        Ok(())
    }

    fn type_application(&mut self, type_application: &mut TypeApplicationExpression) -> ReportableResult<()> {
        let TypeApplicationExpression { function, arguments } = type_application;

        self.type_expression(function)?;
        for argument in arguments {
            self.type_expression(argument)?;
        }

        Ok(())
    }

    fn matc(&mut self, matc: &mut MatchExpression) -> ReportableResult<()> {
        let MatchExpression { expressions, branches } = matc;

        for expression in expressions {
            self.expression(expression)?;
        }

        for branch in branches {
            scoped!(self, {
                for pattern in branch.data().patterns() {
                    self.name_pattern_match(pattern);
                }
                self.expression(branch.data_mut().expression_mut())?;
            });
        }

        Ok(())
    }

    fn name_pattern_match(&mut self, pattern: &Located<Pattern>) {
        match pattern.data() {
            Pattern::Any(identifier) => {
                self.locals.push(*identifier);
            }
            Pattern::U64(_) |
            Pattern::F32(_) |
            Pattern::String(_) => (),
            Pattern::VariantCase(variant_case) => {
                let VariantCasePattern { fields, .. } = variant_case;

                if let Some(fields) = fields {
                    for field in fields {
                        self.name_pattern_match(field);
                    }
                }
            },
            Pattern::Array(array) => {
                let ArrayPattern { before, after, rest } = array;

                for pattern in before {
                    self.name_pattern_match(pattern);
                }

                if let Some(intern_idx) = rest {
                    self.locals.push(*intern_idx);
                }

                for pattern in after {
                    self.name_pattern_match(pattern);
                }
            }
            Pattern::Unit => ()
        }
    }

    fn retrn(&mut self, retrn: &mut ReturnExpression) -> ReportableResult<()> {
        let ReturnExpression { expression } = retrn;

        self.expression(expression)?;

        Ok(())
    }

    pub fn declaration(&mut self, declaration: &mut Declaration) -> ReportableResult<()> {
        // TODO: maybe take Located<Declaration> for better error reporting
        match declaration {
            Declaration::Module(..) => {}
            Declaration::Import(..) => {},
            Declaration::Define(define) => self.define(define)?,
            Declaration::Function(function) => self.function(function)?,
            Declaration::Variant(variant) => self.variant(variant)?,
            Declaration::Interface(interface) => self.interface(interface)?,
            Declaration::Struct(strct) => self.strct(strct)?,
            Declaration::BuiltIn(builtin) => self.builtin(builtin)?,
            Declaration::External(external) => self.external(external)?,
        };

        Ok(())
    }

    fn type_var(&mut self, type_var: &mut Located<TypeVar>) -> ReportableResult<()> {
        let TypeVar { interfaces, .. } = type_var.data_mut();

        for (interface, path) in interfaces.iter_mut() {
            *path = self.find_interface_path(interface.data(), interface.location())?;
        }

        Ok(())
    }

    // TODO: Better error reporting
    fn import_names(&mut self) -> ReportableResult<()> {
        for (path, location) in self.current_imports().values() {
            if !(self.value_names.contains(path) ||
                 self.type_names.contains(path)  ||
                 self.modules.contains_key(path)) {
                return self.error(
                    ResolveError::ImportPathDoesNotExist(path.clone()),
                    *location
                )
            }
        }

        for (path, location) in self.current_import_ins() {
            if !(self.type_names.contains(path)  ||
                 self.modules.contains_key(path)) {
                return self.error(
                    ResolveError::ImportPathDoesNotExist(path.clone()),
                    *location
                )
            }
        }

        Ok(())
    }

    fn module_path(&mut self, module: &Module) -> ReportableResult<()> {
        let mut path = module.path().clone();
        let module_information = &self.modules[&path];
        path.pop();

        if path != Path::empty() && !self.modules.contains_key(&path) {
            return self.error(
                ResolveError::ModuleDoesNotExist(path),
                module_information.path_location
            );
        }

        Ok(())
    }

    // TODO: Detect cycles
    fn define(&mut self, define: &mut DefineDeclaration) -> ReportableResult<()> {
        let DefineDeclaration { type_expression, expression, .. } = define;

        self.type_expression(type_expression)?;
        self.expression(expression)?;

        Ok(())
    }

    fn function(&mut self, function: &mut FunctionDeclaration) -> ReportableResult<()> {
        let FunctionDeclaration { type_vars, arguments, return_type, body, .. } = function;

        for type_var in type_vars.iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in type_vars {
                self.locals.push(*type_var.data().name.data());
            }

            for argument in arguments.iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = return_type {
                self.type_expression(return_type)?;
            }
        });

        scoped!(self, {
            let argument_names = arguments.iter().map(|idx| *idx.data().indentifier().data());
            self.locals.extend(argument_names);

            self.expression(body)?;
        });

        Ok(())
    }

    fn method_signature(&mut self, signature: &mut MethodSignature) -> ReportableResult<()> {
        let MethodSignature { constraints, type_vars, arguments, return_type, .. } = signature;

        for constraint in constraints.iter_mut() {
            self.constraint(constraint)?;
        }

        for type_var in type_vars.iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in type_vars {
                self.locals.push(*type_var.data().name.data());
            }

            for argument in arguments.iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = return_type {
                self.type_expression(return_type)?;
            }
        });

        Ok(())
    }

    fn method(&mut self, method: &mut MethodDeclaration) -> ReportableResult<()> {
        let MethodDeclaration { signature, body, .. } = method;

        self.method_signature(signature)?;

        scoped!(self, {
            self.locals.push(*signature.instance.data());
            let argument_names = signature.arguments.iter().map(|idx| *idx.data().indentifier().data());
            self.locals.extend(argument_names);

            self.expression(body)?;
        });

        Ok(())
    }

    fn constraint(&mut self, constraint: &mut Constraint) -> ReportableResult<()> {
        // NOTE: At this point we only have type parameters of the type
        //   so index represent the order of the type parameter
        let mut found = false;
        for (index, name_idx) in self.locals.iter().enumerate() {
            if name_idx == constraint.type_var.data().name.data() {
                constraint.nth = index;
                found = true;
            }
        }

        if !found {
            todo!("Not a type var of type");
        }

        self.type_var(&mut constraint.type_var)?;

        Ok(())
    }

    fn variant(&mut self, variant: &mut VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { cases, methods, type_vars, .. } = variant;

        scoped!(self, {
            // TODO: These ones are leaked
            let type_vars = type_vars.iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for case in cases {
                if let Some(arguments) = case.data_mut().arguments_mut() {
                    for argument in arguments {
                        self.type_expression(argument)?;
                    }
                }
            }

            // TODO: Here we leak type variables in value names, fix
            for method in methods {
                self.method(method)?;
            }
        });

        Ok(())
    }

    fn interface(&mut self, interface: &mut InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { methods, type_name, .. } = interface;

        self.type_var(type_name)?;

        scoped!(self, {
            self.locals.push(*type_name.data().name.data());

            for method in methods {
                let InterfaceMethodSignature { arguments, return_type, .. } = method;

                for argument in arguments {
                    self.type_expression(argument.data_mut().type_expression_mut())?;
                }

                if let Some(return_type) = return_type {
                    self.type_expression(return_type)?;
                }
            }
        });

        Ok(())
    }

    fn strct(&mut self, strct: &mut StructDeclaration) -> ReportableResult<()> {
        let StructDeclaration { type_vars, fields, methods, .. } = strct;

        scoped!(self, {
            // TODO: These ones are leaked
            let type_vars = type_vars.iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for field in fields {
                self.type_expression(field.data_mut().type_expression_mut())?;
            }

            // TODO: Here we leak type variables in value names, fix
            for method in methods {
                self.method(method)?;
            }
        });

        Ok(())
    }

    fn builtin(&mut self, builtin: &mut BuiltInDeclaration) -> ReportableResult<()> {
        let BuiltInDeclaration { type_vars, methods, .. } = builtin;

        scoped!(self, {
            let type_vars = type_vars.iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for method in methods {
                self.method_signature(method)?;
            }
        });

        Ok(())
    }

    fn external(&mut self, external: &mut ExternalDeclaration) -> ReportableResult<()> {
        let ExternalDeclaration { type_vars, arguments, return_type, .. } = external;

        for type_var in type_vars.iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in type_vars {
                self.locals.push(*type_var.data().name.data());
            }

            for argument in arguments.iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = return_type {
                self.type_expression(return_type)?;
            }
        });

        Ok(())
    }

    fn error<T>(&self, error: ResolveError, location: SourceLocation) -> ReportableResult<T> {
        let reportable = (Located::new(error, location), self.current_source.clone());
        Err(Box::new(reportable))
    }
}

pub enum ResolveError {
    ModuleIsNotDeclared,
    ImportPathDoesNotExist(Path),
    ModuleDoesNotExist(Path),
    CollidingModulePaths(Path),
    DuplicateModuleDeclaration,
    DuplicateNameDeclaration(Path),
    DuplicateTypeDeclaration(Path),
    DuplicateConstructorDeclaration {
        constructor: InternIdx,
        variant_path: Path,
    },
    UnboundValuePath(Path),
    UnboundTypePath(Path),
    UnboundInterfacePath(Path),
}

impl Reportable for (Located<ResolveError>, String) {
    fn location(&self) -> SourceLocation {
        self.0.location()
    }

    fn source(&self) -> &str {
        &self.1
    }

    fn description(&self) -> String {
        match self.0.data() {
            ResolveError::ModuleIsNotDeclared => "No module declarations found.".into(),
            ResolveError::ImportPathDoesNotExist(path) => {
                format!("Imported path `{}` does not exist.", path)
            }
            ResolveError::ModuleDoesNotExist(path) => {
                format!("Module `{}` does not exist.", path)
            }
            ResolveError::CollidingModulePaths(path) => {
                format!("Already imported a module `{}`.", path)
            }
            ResolveError::DuplicateModuleDeclaration => "Duplicate declaration of module.".into(),
            ResolveError::DuplicateNameDeclaration(path) => {
                format!("Duplicate declaration of name `{}`.", path)
            }
            ResolveError::DuplicateTypeDeclaration(path) => {
                format!("Duplicate declaration of type `{}`.", path)
            }
            ResolveError::DuplicateConstructorDeclaration {
                constructor,
                variant_path,
            } => {
                format!(
                    "Duplicate declaration of constructor `{}` in variant type `{}`.",
                    interner().get(constructor),
                    variant_path
                )
            }
            ResolveError::UnboundValuePath(path) => {
                format!("`{}` is not bound to a value.", path)
            }
            ResolveError::UnboundTypePath(path) => {
                format!("`{}` is not bound to a type.", path)
            }
            ResolveError::UnboundInterfacePath(path) => {
                format!("`{}` is not bound to an interface.", path)
            }
        }
    }
}
