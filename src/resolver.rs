use std::{collections::HashSet, vec};

use crate::{
    bound::Bound,
    declaration::Declaration,
    expression::{Expression, TypeExpression},
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::Statement,
};

pub struct Resolver {
    variants: HashSet<Vec<InternIdx>>,
    procedures: HashSet<Vec<InternIdx>>,
    locals: Vec<InternIdx>,

    imports: HashSet<InternIdx>,
    current_absolute_path: Vec<InternIdx>,
}

impl Resolver {
    pub fn new() -> Self {
        Self {
            variants: HashSet::new(),
            procedures: HashSet::new(),
            locals: vec![],

            imports: HashSet::new(),
            current_absolute_path: vec![],
        }
    }

    fn absolute_path(&self, name: InternIdx) -> Vec<InternIdx> {
        let mut path = self.current_absolute_path.clone();
        path.push(name);
        path
    }

    pub fn resolve(
        &mut self,
        mut modules: Vec<(Vec<Declaration>, String)>,
    ) -> Result<Vec<(Vec<Declaration>, String)>, (Box<dyn Reportable>, String)> {
        for module in &mut modules {
            self.collect_names(&module.0)
                .map_err(|error| (error, module.1.clone()))?;
        }

        for module in &mut modules {
            self.which_module(&module.0)
                .map_err(|error| (error, module.1.clone()))?;

            self.program(&mut module.0)
                .map_err(|error| (error, module.1.clone()))?;
        }

        Ok(modules)
    }

    fn program(&mut self, declarations: &mut [Declaration]) -> ReportableResult<()> {
        for declaration in declarations {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn which_module(&mut self, declarations: &[Declaration]) -> ReportableResult<()> {
        self.current_absolute_path.clear();
        self.imports.clear();

        let mut declared = false;
        for declaration in declarations {
            if let Declaration::Module { name } = declaration {
                if !declared {
                    declared = true;
                    self.current_absolute_path.push(*name.data());
                } else {
                    return Self::multiple_module_declarations(name.location());
                }
            }

            if let Declaration::Import { name } = declaration {
                self.imports.insert(*name.data());
            }
        }

        if self.current_absolute_path.is_empty() {
            return Self::module_is_not_declared();
        }

        Ok(())
    }

    fn collect_names(&mut self, declarations: &[Declaration]) -> ReportableResult<()> {
        self.which_module(declarations)?;
        for declaration in declarations {
            match declaration {
                Declaration::Module { .. } => {}
                Declaration::Import { .. } => {}
                Declaration::Procedure { name, .. } => {
                    self.procedures.insert(self.absolute_path(*name.data()));
                }
                Declaration::Variant { name, cases, .. } => {
                    self.variants.insert(self.absolute_path(*name.data()));
                    self.current_absolute_path.push(*name.data());
                    for case in cases {
                        self.procedures
                            .insert(self.absolute_path(*case.data().identifier().data()));
                    }
                    self.current_absolute_path.pop();
                }
            }
        }

        Ok(())
    }

    fn find_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // Local Scope
        for (index, name_idx) in self.locals.iter().rev().enumerate() {
            if name_idx == intern_idx {
                return Some(Bound::local(index));
            }
        }

        if self.imports.contains(intern_idx) {
            Some(Bound::absolute(vec![*intern_idx]))
        } else {
            None
        }
    }

    fn find_type_name(&self, intern_idx: &InternIdx) -> Option<Bound> {
        // TODO: Local Scope (type variables)

        if self.imports.contains(intern_idx) {
            Some(Bound::absolute(vec![*intern_idx]))
        } else {
            None
        }
    }

    fn expression(&mut self, expression: &mut Located<Expression>) -> ReportableResult<()> {
        let location = expression.location();
        match expression.data_mut() {
            Expression::Path(parts, bound) => {
                let base = self
                    .find_name(&parts[0])
                    .or(Some(Bound::absolute(self.absolute_path(parts[0]))));

                match base.as_ref().unwrap() {
                    Bound::Local(_) => *bound = base.unwrap(),
                    Bound::Absolute(path) => {
                        let mut path = path.clone();
                        path.extend(&parts[1..]);

                        let Some(path) = self.procedures.get(&path) else {
                            return Self::unbound_value_path(path, location);
                        };

                        *bound = Bound::absolute(path.clone());
                    }
                    Bound::Undetermined => unreachable!(),
                };

                Ok(())
            }
        }
    }

    fn type_expression(
        &mut self,
        type_expression: &mut Located<TypeExpression>,
    ) -> ReportableResult<()> {
        let location = type_expression.location();
        match type_expression.data_mut() {
            TypeExpression::Path(parts, bound) => {
                let base = self
                    .find_type_name(&parts[0])
                    .or(Some(Bound::absolute(self.absolute_path(parts[0]))));

                match base.as_ref().unwrap() {
                    Bound::Local(_) => *bound = base.unwrap(),
                    Bound::Absolute(path) => {
                        let mut path = path.clone();
                        path.extend(&parts[1..]);

                        let Some(path) = self.variants.get(&path) else {
                            return Self::unbound_type_path(path, location);
                        };

                        *bound = Bound::absolute(path.clone());
                    }
                    Bound::Undetermined => unreachable!(),
                };

                Ok(())
            }
        }
    }

    fn statement(&mut self, statement: &mut Located<Statement>) -> ReportableResult<()> {
        match statement.data_mut() {
            Statement::Expression(expression) => return self.expression(expression),
        };
    }

    fn declaration(&mut self, declaration: &mut Declaration) -> ReportableResult<()> {
        // TODO: maybe take Located<Declaration> for better error reporting
        match declaration {
            Declaration::Module { .. } => {}
            // TODO: check if the module exists
            Declaration::Import { .. } => {}
            Declaration::Procedure {
                arguments, body, ..
            } => {
                for argument in arguments.iter_mut() {
                    self.type_expression(argument.data_mut().type_expression_mut())?;
                }

                self.locals
                    .extend(arguments.iter().map(|idx| *idx.data().indentifier().data()));
                for statement in body {
                    self.statement(statement)?
                }
                self.locals.truncate(self.locals.len() - arguments.len());
            }
            Declaration::Variant { cases, methods, .. } => {
                for case in cases {
                    if let Some(arguments) = case.data_mut().arguments_mut() {
                        for argument in arguments {
                            self.type_expression(argument.data_mut().type_expression_mut())?;
                        }
                    }
                }

                for method in methods {
                    for argument in method.arguments.iter_mut() {
                        self.type_expression(argument.data_mut().type_expression_mut())?;
                    }

                    // TODO : implicit (or explicit) `self` variable
                    self.locals.extend(
                        method
                            .arguments
                            .iter()
                            .map(|idx| *idx.data().indentifier().data()),
                    ); // ?

                    for statement in &mut method.body {
                        self.statement(statement)?
                    }
                    self.locals
                        .truncate(self.locals.len() - method.arguments.len());
                }
            }
        };

        Ok(())
    }

    fn module_is_not_declared() -> ReportableResult<()> {
        let resolve_error = ResolveError::ModuleIsNotDeclared;

        Err(Box::new(Located::new(
            resolve_error,
            SourceLocation::dummy(),
        )))
    }

    fn multiple_module_declarations(location: SourceLocation) -> ReportableResult<()> {
        let resolve_error = ResolveError::MultipleModuleDeclarations;

        Err(Box::new(Located::new(resolve_error, location)))
    }

    fn unbound_value_path(path: Vec<InternIdx>, location: SourceLocation) -> ReportableResult<()> {
        let resolve_error = ResolveError::UnboundValuePath(path);

        Err(Box::new(Located::new(resolve_error, location)))
    }

    fn unbound_type_path(path: Vec<InternIdx>, location: SourceLocation) -> ReportableResult<()> {
        let resolve_error = ResolveError::UnboundTypePath(path);

        Err(Box::new(Located::new(resolve_error, location)))
    }
}

pub enum ResolveError {
    ModuleIsNotDeclared,
    MultipleModuleDeclarations,
    UnboundValuePath(Vec<InternIdx>),
    UnboundTypePath(Vec<InternIdx>),
}

impl Reportable for Located<ResolveError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self, interner: &Interner) -> String {
        match self.data() {
            ResolveError::ModuleIsNotDeclared => "No module declarations found.".into(),
            ResolveError::MultipleModuleDeclarations => "Duplicate declaration of module.".into(),
            ResolveError::UnboundValuePath(path) => {
                let path_string = path
                    .iter()
                    .map(|intern_idx| interner.get(intern_idx))
                    .collect::<Vec<_>>()
                    .join("::");

                format!("`{path_string}` is not bound to value.")
            }
            ResolveError::UnboundTypePath(path) => {
                let path_string = path
                    .iter()
                    .map(|intern_idx| interner.get(intern_idx))
                    .collect::<Vec<_>>()
                    .join("::");

                format!("`{path_string}` is not bound to type.")
            }
        }
    }
}
