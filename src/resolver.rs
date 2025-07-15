use std::{collections::{HashMap, HashSet}, vec};

use crate::{
    bound::{Bound, Path},
    declaration::{Declaration, ImportDeclaration, MethodDeclaration, Module, ModuleDeclaration, ProcedureDeclaration, VariantDeclaration},
    expression::{ApplicationExpression, Expression, PathExpression, PathTypeExpression, ProcedureTypeExpression, ProjectionExpression, TypeExpression},
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern},
};

struct ModuleInformation {
    imports: HashSet<InternIdx>,
    path: Path,
}

impl ModuleInformation {
    fn empty() -> Self {
        Self {
            imports: HashSet::new(),
            path: Path::empty(),
        }
    }
}

pub struct Resolver {
    modules: HashMap<InternIdx, ModuleInformation>,

    type_names: HashSet<Path>,
    value_names: HashSet<Path>,

    locals: Vec<InternIdx>,

    current_module_name: InternIdx,
    current_source: String,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),

            type_names: HashSet::new(),
            value_names: HashSet::new(),

            locals: vec![],

            current_module_name: InternIdx::dummy_idx(),
            current_source: String::new(),
        }
    }

    fn current_imports(&self) -> &HashSet<InternIdx> {
        &self.modules[&self.current_module_name].imports
    }

    fn current_path(&self) -> &Path {
        &self.modules[&self.current_module_name].path
    }

    fn current_path_mut(&mut self) -> &mut Path {
        &mut self.modules.get_mut(&self.current_module_name).unwrap().path
    }

    pub fn resolve(&mut self, mut modules: Vec<Module>) -> ReportableResult<Vec<Module>> {
        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.collect_module(module)?;
        }

        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.current_module_name = module.name();
            self.collect_names(module)?;
        }

        for module in &mut modules {
            self.current_source = module.source().to_string();
            self.current_module_name = module.name();
            self.module(module)?;
        }

        Ok(modules)
    }

    fn module(&mut self, module: &mut Module) -> ReportableResult<()> {
        for declaration in module.declarations_mut() {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn collect_module(&mut self, module: &mut Module) -> ReportableResult<()> {
        let mut module_name = None;
        let mut declared = false;
        for declaration in module.declarations() {
            if let Declaration::Module(module) = declaration {
                let ModuleDeclaration { name } = module;

                if !declared {
                    declared = true;
                    module_name = Some(*name.data());
                    if self.modules.insert(*name.data(), ModuleInformation::empty()).is_some() {
                        // Without this error, behaviour is extending the existing module
                        //   maybe that's an interesting idea!
                        return self.error(ResolveError::CollidingModuleNames(*name.data()), name.location())
                    }
                } else {
                    return self.error(ResolveError::DuplicateModuleDeclaration, name.location());
                }
            }
        }

        let Some(module_name) = module_name else {
            return self.error(ResolveError::ModuleIsNotDeclared, SourceLocation::dummy());
        };
        *module.name_mut() = module_name;

        let module_information = self.modules.get_mut(&module_name).unwrap();
        for declaration in module.declarations() {
            if let Declaration::Import(import) = declaration {
                let ImportDeclaration { name } = import;

                module_information.imports.insert(*name.data());
            }
        }

        module_information.path.push(module_name);

        Ok(())
    }

    fn collect_names(&mut self, module: &mut Module) -> ReportableResult<()> {
        for declaration in module.declarations_mut() {
            match declaration {
                Declaration::Module { .. } => {}
                Declaration::Import { .. } => {}
                Declaration::Procedure(precodure) => self.collect_procedure_name(precodure)?,
                Declaration::Variant(variant) => self.collect_variant_name(variant)?,
            }
        }

        Ok(())
    }

    fn collect_procedure_name(&mut self, procedure: &mut ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { name, path, .. } = procedure;

        let procedure_path = self.current_path().append(*name.data());
        if !self.value_names.contains(&procedure_path) {
            self.value_names.insert(procedure_path.clone());
            *path = procedure_path;
        } else {
            return self.error(
                ResolveError::DuplicateProcedureDeclaration(procedure_path),
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

    fn find_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // Local Scope
        for (index, name_idx) in self.locals.iter().rev().enumerate() {
            if name_idx == intern_idx {
                return Some(Bound::local(index));
            }
        }

        if self.current_imports().contains(intern_idx) {
            Some(Bound::absolute(vec![*intern_idx]))
        } else {
            None
        }
    }

    fn find_type_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // TODO: Local Scope (type variables)

        if self.current_imports().contains(intern_idx) {
            Some(Bound::absolute(vec![*intern_idx]))
        } else {
            None
        }
    }

    fn expression(&mut self, expression: &mut Located<Expression>) -> ReportableResult<()> {
        let location = expression.location();
        match expression.data_mut() {
            Expression::Path(path) => self.path(path, location),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
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

    fn type_expression(&mut self, type_expression: &mut Located<TypeExpression>) -> ReportableResult<()> {
        let location = type_expression.location();
        match type_expression.data_mut() {
            TypeExpression::Path(path) => self.path_type(path, location),
            TypeExpression::Procedure(procedure_type) => self.procedure_type(procedure_type),
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

    fn procedure_type(&mut self, procdeure_type: &mut ProcedureTypeExpression) -> ReportableResult<()> {
        let ProcedureTypeExpression { arguments, return_type } = procdeure_type;

        for argument in arguments {
            self.type_expression(argument)?;
        }
        self.type_expression(return_type)
    }

    fn statement(&mut self, statement: &mut Located<Statement>) -> ReportableResult<()> {
        match statement.data_mut() {
            Statement::Expression(expression) => self.expression(expression),
            Statement::Return(retrn) => self.retrn(retrn),
            Statement::Match(matc) => self.matc(matc),
        }
    }

    fn retrn(&mut self, retrn: &mut ReturnStatement) -> ReportableResult<()> {
        let ReturnStatement { expression } = retrn;

        self.expression(expression)
    }

    fn matc(&mut self, matc: &mut MatchStatement) -> ReportableResult<()> {
        let MatchStatement { expression, branches } = matc;

        self.expression(expression)?;
        let locals_len = self.locals.len();

        for branch in branches {
            match branch.data().pattern().data() {
                Pattern::VariantCase(variant_case) => {
                    let VariantCasePattern { fields, .. } = variant_case;

                    if let Some(fields) = fields {
                        let fields = fields.iter().map(|field| *field.data());
                        self.locals.extend(fields);
                    }
                }
            }

            self.statement(branch.data_mut().statement_mut())?;
            self.locals.truncate(locals_len);
        }

        Ok(())
    }

    fn declaration(&mut self, declaration: &mut Declaration) -> ReportableResult<()> {
        // TODO: maybe take Located<Declaration> for better error reporting
        match declaration {
            Declaration::Module { .. } => {}
            Declaration::Import(import) => self.import(import)?,
            Declaration::Procedure(procedure) => self.procedure(procedure)?,
            Declaration::Variant(variant) => self.variant(variant)?,
        };

        Ok(())
    }

    fn import(&self, import: &ImportDeclaration) -> ReportableResult<()> {
        let ImportDeclaration { name } = import;

        if !self.modules.contains_key(name.data()) {
            return self.error(
                ResolveError::ModuleDoesNotExist(*name.data()),
                name.location(),
            );
        }

        Ok(())
    }

    fn procedure(&mut self, procedure: &mut ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { arguments, return_type, body, .. } = procedure;

        for argument in arguments.iter_mut() {
            self.type_expression(argument.data_mut().type_expression_mut())?;
        }
        self.type_expression(return_type)?;

        let argument_names = arguments.iter().map(|idx| *idx.data().indentifier().data());
        self.locals.extend(argument_names);
            for statement in body {
                self.statement(statement)?
            }
        self.locals.truncate(self.locals.len() - arguments.len());

        Ok(())
    }

    fn method(&mut self, method: &mut MethodDeclaration) -> ReportableResult<()> {
        let MethodDeclaration { instance, arguments, return_type, body, .. } = method;

        for argument in arguments.iter_mut() {
            self.type_expression(argument.data_mut().type_expression_mut())?;
        }
        self.type_expression(return_type)?;

        self.locals.push(*instance.data());
        let argument_names = arguments.iter().map(|idx| *idx.data().indentifier().data());
        self.locals.extend(argument_names);
            for statement in body {
                self.statement(statement)?
            }
        self.locals.truncate(self.locals.len() - arguments.len());
        self.locals.pop();

        Ok(())
    }

    fn variant(&mut self, variant: &mut VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { cases, methods, .. } = variant;

        for case in cases {
            if let Some(arguments) = case.data_mut().arguments_mut() {
                for argument in arguments {
                    self.type_expression(argument)?;
                }
            }
        }

        for method in methods {
            self.method(method)?;
        }

        Ok(())
    }

    fn error<T>(&self, error: ResolveError, location: SourceLocation) -> ReportableResult<T> {
        let reportable = (Located::new(error, location), self.current_source.clone());
        Err(Box::new(reportable))
    }
}

pub enum ResolveError {
    ModuleIsNotDeclared,
    ModuleDoesNotExist(InternIdx),
    CollidingModuleNames(InternIdx),
    DuplicateModuleDeclaration,
    DuplicateProcedureDeclaration(Path),
    DuplicateTypeDeclaration(Path),
    DuplicateConstructorDeclaration {
        constructor: InternIdx,
        variant_path: Path,
    },
    UnboundValuePath(Path),
    UnboundTypePath(Path),
}

impl Reportable for (Located<ResolveError>, String) {
    fn location(&self) -> SourceLocation {
        self.0.location()
    }

    fn source(&self) -> &str {
        &self.1
    }

    fn description(&self, interner: &Interner) -> String {
        match self.0.data() {
            ResolveError::ModuleIsNotDeclared => "No module declarations found.".into(),
            ResolveError::ModuleDoesNotExist(name) => {
                format!("Imported module `{}` does not exist.", interner.get(name))
            }
            ResolveError::CollidingModuleNames(name) => {
                format!("Already imported a module named `{}`.", interner.get(name))
            }
            ResolveError::DuplicateModuleDeclaration => "Duplicate declaration of module.".into(),
            ResolveError::DuplicateProcedureDeclaration(path) => {
                format!(
                    "Duplicate declaration of procedure `{}`.",
                    path.as_string(interner)
                )
            }
            ResolveError::DuplicateTypeDeclaration(path) => {
                format!(
                    "Duplicate declaration of type `{}`.",
                    path.as_string(interner)
                )
            }
            ResolveError::DuplicateConstructorDeclaration {
                constructor,
                variant_path: variant,
            } => {
                format!(
                    "Duplicate declaration of constructor `{}` in variant type `{}`.",
                    interner.get(constructor),
                    variant.as_string(interner)
                )
            }
            ResolveError::UnboundValuePath(path) => {
                format!("`{}` is not bound to a value.", path.as_string(interner))
            }
            ResolveError::UnboundTypePath(path) => {
                format!("`{}` is not bound to a type.", path.as_string(interner))
            }
        }
    }
}
