use std::collections::{HashMap, HashSet};

use crate::{
    bound::{Bound, Path},
    declaration::{self, Declaration},
    expression::{
        self,
        Expression,
        pattern::Pattern,
    },
    type_expression::{self, TypeExpression},
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

    pub fn resolve(&mut self, mut modules: Vec<declaration::Module>) -> ReportableResult<Vec<declaration::Module>> {
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

    fn module(&mut self, module: &mut declaration::Module) -> ReportableResult<()> {
        self.import_names()?;

        for declaration in module.declarations_mut() {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn collect_module(&mut self, module: &mut declaration::Module) -> ReportableResult<()> {
        let mut module_path = None;
        let mut declared = false;
        for declaration in module.declarations() {
            if let Declaration::ModulePath(module) = declaration {
                if !declared {
                    declared = true;
                    let path = Path::empty().append_parts(module.parts().data());
                    module_path = Some(path.clone());
                    if self.modules.insert(path.clone(), ModuleInformation::with_path_location(module.parts().location())).is_some() {
                        // Without this error, behaviour is extending the existing module
                        //   maybe that's an interesting idea!
                        return self.error(ResolveError::CollidingModulePaths(path), module.parts().location())
                    }
                } else {
                    return self.error(ResolveError::DuplicateModuleDeclaration, module.parts().location());
                }
            }
        }

        let Some(module_name) = module_path else {
            return self.error(ResolveError::ModuleIsNotDeclared, SourceLocation::dummy());
        };
        module.set_path(module_name.clone());

        let module_information = self.modules.get_mut(&module_name).unwrap();
        for declaration in module.declarations() {
            if let Declaration::Import(import) = declaration {
                Self::module_imports(import, module_information);
            }
        }

        Ok(())
    }

    fn module_imports(import_name: &declaration::Import, module_information: &mut ModuleInformation) {
        fn f(import: &declaration::Import, path: Path, module_information: &mut ModuleInformation) {
            let import_path = path.append(*import.name().data());

            let name = if let Some(as_name) = import.as_name() {
                as_name
            } else {
                import.name()
            };

            if import.import_in() {
                module_information.import_ins.insert(
                    import_path.clone(),
                    name.location()
                );
            }

            module_information.imports.insert(
                *name.data(),
                (import_path.clone(), name.location())
            );

            if let Some(subnames) = import.subnames() {
                for import_name in subnames {
                    f(import_name, import_path.clone(), module_information);
                }
            }
        }

        f(import_name, Path::empty(), module_information);
    }

    fn collect_names(&mut self, module: &mut declaration::Module) -> ReportableResult<()> {
        for declaration in module.declarations_mut() {
            match declaration {
                Declaration::ModulePath(..) => {}
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

    fn collect_define_name(&mut self, define: &mut declaration::Define) -> ReportableResult<()> {
        let define_path = self.current_path().append(*define.name().data());
        if !self.value_names.contains(&define_path) {
            self.value_names.insert(define_path.clone());
            define.set_path(define_path);
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(define_path),
                define.name().location(),
            );
        }

        Ok(())
    }

    fn collect_function_name(&mut self, function: &mut declaration::Function) -> ReportableResult<()> {
        let function_path = self.current_path().append(*function.name().data());
        if !self.value_names.contains(&function_path) {
            self.value_names.insert(function_path.clone());
            function.set_path(function_path);
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(function_path),
                function.name().location(),
            );
        }

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &mut declaration::Variant) -> ReportableResult<()> {
        let variant_path = self.current_path().append(*variant.name().data());
        if self.type_names.contains(&variant_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(variant_path),
                variant.name().location(),
            );
        }

        self.current_path_mut().push(*variant.name().data());
        for case in variant.cases_mut().iter_mut() {
            let constructor = *case.data().identifier().data();
            let constructor_path = self.current_path().append(constructor);
            if !self.value_names.contains(&constructor_path) {
                self.value_names.insert(constructor_path.clone());
                case.data_mut().set_path(constructor_path);
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
        variant.set_path(variant_path);

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &mut declaration::Interface) -> ReportableResult<()> {
        let interface_path = self.current_path().append(*interface.name().data());
        if self.type_names.contains(&interface_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(interface_path),
                interface.name().location(),
            );
        }

        self.type_names.insert(interface_path.clone());
        interface.set_path(interface_path);

        self.current_path_mut().push(*interface.name().data());
        for method in interface.methods_mut() {
            let function_path = self.current_path().append(*method.name().data());
            method.set_path(function_path.clone());
            // TODO: Check for duplicate interface method declarations
            self.value_names.insert(function_path);
        }
        self.current_path_mut().pop();

        Ok(())
    }

    fn collect_struct_name(&mut self, strct: &mut declaration::Struct) -> ReportableResult<()> {
        let struct_path = self.current_path().append(*strct.name().data());
        if self.type_names.contains(&struct_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(struct_path),
                strct.name().location(),
            );
        }

        self.type_names.insert(struct_path.clone());
        self.value_names.insert(struct_path.clone());
        strct.set_path(struct_path);

        Ok(())
    }

    fn collect_builtin_name(&mut self, builtin: &mut declaration::BuiltIn) -> ReportableResult<()> {
        if self.current_path().to_string() != INTRINSICS_MODULE_NAME {
            panic!("Not allowed in builtin declarations outside of Intrinsics Module.")
        }

        let builtin_path = self.current_path().append(*builtin.name().data());
        if self.type_names.contains(&builtin_path) {
            return self.error(
                ResolveError::DuplicateTypeDeclaration(builtin_path),
                builtin.name().location(),
            );
        }

        self.type_names.insert(builtin_path.clone());
        builtin.set_path(builtin_path);

        Ok(())
    }

    fn collect_external_name(&mut self, external: &mut declaration::External) -> ReportableResult<()> {
        let external_path = self.current_path().append(*external.name().data());
        if !self.value_names.contains(&external_path) {
            self.value_names.insert(external_path.clone());
            external.set_path(external_path);
        } else {
            return self.error(
                ResolveError::DuplicateNameDeclaration(external_path),
                external.name().location(),
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
            Expression::String(_) |
            Expression::Char(_) => Ok(()),
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

    fn path(&mut self, path: &mut expression::Path, location: SourceLocation) -> ReportableResult<()> {
        let base = self
            .find_name(&path.parts()[0])
            .unwrap_or(Bound::Absolute(self.current_path().append(path.parts()[0])));

        match base {
            Bound::Local(_) => {
                assert!(path.parts().len() == 1);
                path.set_bound(base);
            }
            Bound::Absolute(base_path) => {
                let absolute_path = base_path.append_parts(&path.parts()[1..]);
                let Some(absolute_path) = self.value_names.get(&absolute_path) else {
                    return self.error(ResolveError::UnboundValuePath(absolute_path), location);
                };
                path.set_bound(Bound::Absolute(absolute_path.clone()));
            }
            Bound::Undetermined => unreachable!(),
        };

        Ok(())
    }

    fn array(&mut self, array: &mut expression::Array) -> ReportableResult<()> {
        for expression in array.expressions_mut() {
            self.expression(expression)?;
        }

        Ok(())
    }

    fn application(&mut self, application: &mut expression::Application) -> ReportableResult<()> {
        self.expression(application.function_mut())?;
        for argument in application.arguments_mut() {
            self.expression(argument)?;
        }

        Ok(())
    }

    fn projection(&mut self, projection: &mut expression::Projection) -> ReportableResult<()> {
        self.expression(projection.expression_mut())
    }

    fn lett(&mut self, lett: &mut expression::Let) -> ReportableResult<()> {
        self.expression(lett.value_expression_mut())?;

        scoped!(self, {
            self.locals.push(*lett.identifier().data());
            self.expression(lett.body_expression_mut())?;
        });

        Ok(())
    }

    fn sequence(&mut self, sequence: &mut expression::Sequence) -> ReportableResult<()> {
        sequence
            .expressions_mut()
            .iter_mut()
            .map(|expression| self.expression(expression))
            .collect::<ReportableResult<Vec<_>>>()
            .map(|_| ())
    }

    fn block(&mut self, block: &mut expression::Block) -> ReportableResult<()> {
        block
            .expressions_mut()
            .iter_mut()
            .map(|expression| self.expression(expression))
            .collect::<ReportableResult<Vec<_>>>()
            .map(|_| ())
    }

    fn lambda(&mut self, lambda: &mut expression::Lambda) -> ReportableResult<()> {
        scoped!(self, {
            let argument_names = lambda.arguments().iter().map(|idx| *idx.data());
            self.locals.extend(argument_names);

            self.expression(lambda.body_mut())?;
        });

        Ok(())
    }

    fn assignment(&mut self, assignment: &mut expression::Assignment) -> ReportableResult<()> {
        self.expression(assignment.assignable_mut())?;
        self.expression(assignment.expression_mut())
    }

    fn whilee(&mut self, whilee: &mut expression::While) -> ReportableResult<()> {
        self.expression(whilee.condition_mut())?;

        if let Some(post) = whilee.post_mut() {
            self.expression(post)?;
        }

        self.expression(whilee.body_mut())
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

    fn path_type(&mut self, path: &mut type_expression::Path, location: SourceLocation) -> ReportableResult<()> {
        let base = self
            .find_type_name(&path.parts()[0])
            .unwrap_or(Bound::Absolute(self.current_path().append(path.parts()[0])));

        match base {
            Bound::Local(_) => {
                assert!(path.parts().len() == 1);
                path.set_bound(base);
            }
            Bound::Absolute(base_path) => {
                let absolute_path = base_path.append_parts(&path.parts()[1..]);
                let Some(absolute_path) = self.type_names.get(&absolute_path) else {
                    return self.error(ResolveError::UnboundTypePath(absolute_path), location);
                };
                path.set_bound(Bound::Absolute(absolute_path.clone()));
            }
            Bound::Undetermined => unreachable!(),
        };

        Ok(())
    }

    fn function_type(&mut self, function_type: &mut type_expression::Function) -> ReportableResult<()> {
        for argument in function_type.arguments_mut() {
            self.type_expression(argument)?;
        }

        if let Some(return_type) = function_type.return_type_mut() {
            self.type_expression(return_type)?;
        }

        Ok(())
    }

    fn type_application(&mut self, type_application: &mut type_expression::Application) -> ReportableResult<()> {
        self.type_expression(type_application.function_mut())?;
        for argument in type_application.arguments_mut() {
            self.type_expression(argument)?;
        }

        Ok(())
    }

    fn matc(&mut self, matc: &mut expression::Match) -> ReportableResult<()> {
        for expression in matc.expressions_mut() {
            self.expression(expression)?;
        }

        for branch in matc.branches_mut() {
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
            Pattern::String(_) |
            Pattern::Char(_) => (),
            Pattern::VariantCase(variant_case) => {
                if let Some(fields) = variant_case.fields() {
                    for field in fields {
                        self.name_pattern_match(field);
                    }
                }
            },
            Pattern::Array(array) => {
                for pattern in array.before() {
                    self.name_pattern_match(pattern);
                }

                if let Some(intern_idx) = array.rest() {
                    self.locals.push(intern_idx);
                }

                for pattern in array.after() {
                    self.name_pattern_match(pattern);
                }
            }
            Pattern::Unit => ()
        }
    }

    fn retrn(&mut self, retrn: &mut expression::Return) -> ReportableResult<()> {
        self.expression(retrn.expression_mut())
    }

    pub fn declaration(&mut self, declaration: &mut Declaration) -> ReportableResult<()> {
        // TODO: maybe take Located<Declaration> for better error reporting
        match declaration {
            Declaration::ModulePath(..) => {}
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

    fn type_var(&mut self, type_var: &mut Located<declaration::TypeVar>) -> ReportableResult<()> {
        for (interface, path) in type_var.data_mut().interfaces_mut().iter_mut() {
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

    fn module_path(&mut self, module: &declaration::Module) -> ReportableResult<()> {
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

    fn define(&mut self, define: &mut declaration::Define) -> ReportableResult<()> {
        self.type_expression(define.type_expression_mut())?;
        self.expression(define.expression_mut())?;

        Ok(())
    }

    fn function(&mut self, function: &mut declaration::Function) -> ReportableResult<()> {
        for type_var in function.type_vars_mut().iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in function.type_vars() {
                self.locals.push(*type_var.data().name().data());
            }

            for argument in function.arguments_mut().iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = function.return_type_mut() {
                self.type_expression(return_type)?;
            }
        });

        scoped!(self, {
            let argument_names = function.arguments().iter().map(|idx| *idx.data().identifier().data());
            self.locals.extend(argument_names);

            self.expression(function.body_mut())?;
        });

        Ok(())
    }

    fn method_signature(&mut self, signature: &mut declaration::MethodSignature) -> ReportableResult<()> {
        for constraint in signature.constraints_mut().iter_mut() {
            self.constraint(constraint)?;
        }

        for type_var in signature.type_vars_mut().iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in signature.type_vars() {
                self.locals.push(*type_var.data().name().data());
            }

            for argument in signature.arguments_mut().iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = signature.return_type_mut() {
                self.type_expression(return_type)?;
            }
        });

        Ok(())
    }

    fn method(&mut self, method: &mut declaration::Method) -> ReportableResult<()> {
        self.method_signature(method.signature_mut())?;

        scoped!(self, {
            self.locals.push(*method.signature().instance().data());
            let argument_names = method.signature().arguments().iter().map(|idx| *idx.data().identifier().data());
            self.locals.extend(argument_names);

            self.expression(method.body_mut())?;
        });

        Ok(())
    }

    fn constraint(&mut self, constraint: &mut declaration::MethodConstraint) -> ReportableResult<()> {
        // NOTE: At this point we only have type parameters of the type
        //   so index represent the order of the type parameter
        let mut found = false;
        for (index, name_idx) in self.locals.iter().enumerate() {
            if name_idx == constraint.type_var().data().name().data() {
                constraint.set_nth(index);
                found = true;
            }
        }

        if !found {
            todo!("Not a type var of type");
        }

        self.type_var(constraint.type_var_mut())?;

        Ok(())
    }

    fn variant(&mut self, variant: &mut declaration::Variant) -> ReportableResult<()> {
        scoped!(self, {
            // TODO: These ones are leaked
            let type_vars = variant.type_vars().iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for case in variant.cases_mut() {
                if let Some(arguments) = case.data_mut().arguments_mut() {
                    for argument in arguments {
                        self.type_expression(argument)?;
                    }
                }
            }

            // TODO: Here we leak type variables in value names, fix
            for method in variant.methods_mut() {
                self.method(method)?;
            }
        });

        Ok(())
    }

    fn interface(&mut self, interface: &mut declaration::Interface) -> ReportableResult<()> {
        self.type_var(interface.type_name_mut())?;

        scoped!(self, {
            self.locals.push(*interface.type_name().data().name().data());

            for method in interface.methods_mut() {
                for argument in method.arguments_mut() {
                    self.type_expression(argument.data_mut().type_expression_mut())?;
                }

                if let Some(return_type) = method.return_type_mut() {
                    self.type_expression(return_type)?;
                }
            }
        });

        Ok(())
    }

    fn strct(&mut self, strct: &mut declaration::Struct) -> ReportableResult<()> {
        scoped!(self, {
            // TODO: These ones are leaked
            let type_vars = strct.type_vars().iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for field in strct.fields_mut() {
                self.type_expression(field.data_mut().type_expression_mut())?;
            }

            // TODO: Here we leak type variables in value names, fix
            for method in strct.methods_mut() {
                self.method(method)?;
            }
        });

        Ok(())
    }

    fn builtin(&mut self, builtin: &mut declaration::BuiltIn) -> ReportableResult<()> {
        scoped!(self, {
            let type_vars = builtin.type_vars().iter().map(|type_var| type_var.data());
            self.locals.extend(type_vars);

            for (signature, body) in builtin.methods_mut() {
                self.method_signature(signature)?;
                if let Some(body) = body {
                    // TODO : Factor out here
                    scoped!(self, {
                        self.locals.push(*signature.instance().data());
                        let argument_names = signature.arguments().iter().map(|idx| *idx.data().identifier().data());
                        self.locals.extend(argument_names);

                        self.expression(body)?;
                    });
                }
            }
        });

        Ok(())
    }

    fn external(&mut self, external: &mut declaration::External) -> ReportableResult<()> {
        for type_var in external.type_vars_mut().iter_mut() {
            self.type_var(type_var)?;
        }

        scoped!(self, {
            for type_var in external.type_vars() {
                self.locals.push(*type_var.data().name().data());
            }

            for argument in external.arguments_mut().iter_mut() {
                self.type_expression(argument.data_mut().type_expression_mut())?;
            }

            if let Some(return_type) = external.return_type_mut() {
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
