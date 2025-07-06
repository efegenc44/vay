use std::{collections::HashSet, fmt::Display, vec};

use crate::{
    bound::Bound, declaration::Declaration, error::Error, expression::{Expression, TypeExpression}, interner::InternIdx, location::{Located, SourceLocation}, statement::Statement
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

    pub fn resolve(&mut self, mut modules: Vec<(Vec<Declaration>, String)>) -> Result<Vec<(Vec<Declaration>, String)>, (Located<ResolveError>, String)> {
        for module in &mut modules {
            self.collect_names(&mut module.0).map_err(|error| {
                (error, module.1.clone())
            })?;
        }

        for module in &mut modules {
            self.which_module(&mut module.0).map_err(|error| {
                (error, module.1.clone())
            })?;

            self.program(&mut module.0).map_err(|error| {
                (error, module.1.clone())
            })?;
        }

        Ok(modules)
    }

    fn program(&mut self, declarations: &mut [Declaration]) -> ResolveResult {
        for declaration in declarations {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn which_module(&mut self, declarations: &[Declaration]) -> ResolveResult {
        self.current_absolute_path.clear();
        self.imports.clear();

        let mut declared = false;
        for declaration in declarations {
            if let Declaration::Module { name } = declaration {
                if !declared {
                    declared = true;
                    self.current_absolute_path.push(*name.data());
                } else {
                    return Err(Located::new(
                        ResolveError::MultipleModuleDeclaration,
                        name.location()
                    ));
                }
            }

            if let Declaration::Import { name } = declaration {
                self.imports.insert(*name.data());
            }
        }

        if self.current_absolute_path.is_empty() {
            // TODO: absence errors
            return Err(Located::new(
                ResolveError::ModuleIsNotDeclared,
                SourceLocation::dummy()
            ));
        }

        Ok(())
    }

    fn collect_names(&mut self, declarations: &[Declaration]) -> ResolveResult {
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
                        self.procedures.insert(self.absolute_path(*case.data().identifier().data()));
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

    fn expression(&mut self, expression: &mut Located<Expression>) -> ResolveResult {
        let location = expression.location();
        match expression.data_mut() {
            Expression::Path(parts, bound) => {
                let base = self
                    .find_name(&parts[0])
                    .or(Some(Bound::absolute(self.absolute_path(parts[0]))));

                match base.as_ref().unwrap() {
                    Bound::Local(_) => *bound = base.unwrap(),
                    Bound::Absolute(items) => {
                        let mut items = items.clone();
                        items.extend(&parts[1..]);
                        *bound = self.procedures
                            .get(&items)
                            .map(|items| Bound::absolute(items.clone()))
                            .ok_or(Located::new(
                                ResolveError::UnboundValueIdentifier(parts[0]),
                                location
                            ))?;
                    },
                    Bound::Undetermined => unreachable!(),
                };

                Ok(())
            },
        }
    }

    fn type_expression(&mut self, type_expression: &mut Located<TypeExpression>) -> ResolveResult {
        let location = type_expression.location();
        match type_expression.data_mut() {
            TypeExpression::Path(parts, bound) => {
                let base = self
                    .find_type_name(&parts[0])
                    .or(Some(Bound::absolute(self.absolute_path(parts[0]))));

                match base.as_ref().unwrap() {
                    Bound::Local(_) => *bound = base.unwrap(),
                    Bound::Absolute(items) => {
                        let mut items = items.clone();
                        items.extend(&parts[1..]);
                        *bound = self.variants
                            .get(&items)
                            .map(|items| Bound::absolute(items.clone()))
                            .ok_or(Located::new(
                                ResolveError::UnboundTypeIdentifier(parts[0]),
                                location
                            ))?;
                    },
                    Bound::Undetermined => unreachable!(),
                };

                Ok(())
            },
        }
    }

    fn statement(&mut self, statement: &mut Located<Statement>) -> ResolveResult {
        match statement.data_mut() {
            Statement::Expression(expression) => return self.expression(expression),
        };
    }

    fn declaration(&mut self, declaration: &mut Declaration) -> ResolveResult {
        match declaration {
            Declaration::Module { .. } => {}
            Declaration::Import { .. } => {}
            Declaration::Procedure {
                arguments, body, ..
            } => {
                for argument in arguments.iter_mut() {
                    self.type_expression(
                        argument.data_mut().type_expression_mut(),
                    )?;
                }

                self.locals
                    .extend(arguments.iter().map(|idx| *idx.data().indentifier().data())); // ?

                for statement in body {
                    self.statement(statement)?
                }
                self.locals.truncate(self.locals.len() - arguments.len());
            }
            Declaration::Variant { cases, methods, .. } => {
                for case in cases {
                    if let Some(arguments) = case.data_mut().arguments_mut() {
                        for argument in arguments {
                            self.type_expression(
                                argument.data_mut().type_expression_mut(),
                            )?;
                        }
                    }
                }

                for method in methods {
                    for argument in method.arguments.iter_mut() {
                        self.type_expression(
                            argument.data_mut().type_expression_mut(),
                        )?;
                    }

                    // TODO : implicit (or explicit) `self` variable
                    self.locals
                        .extend(method.arguments.iter().map(|idx| *idx.data().indentifier().data())); // ?

                    for statement in &mut method.body {
                        self.statement(statement)?
                    }
                    self.locals.truncate(self.locals.len() - method.arguments.len());
                }
            }
        };

        Ok(())
    }
}

pub enum ResolveError {
    ModuleIsNotDeclared,
    MultipleModuleDeclaration,
    UnboundValueIdentifier(InternIdx),
    UnboundTypeIdentifier(InternIdx),
}

impl Error for Located<ResolveError> {
    fn location(&self) -> SourceLocation {
        self.location()
    }

    fn description(&self) -> String {
        self.data().to_string()
    }
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResolveError::ModuleIsNotDeclared => write!(f, "No module declarations found."),
            ResolveError::MultipleModuleDeclaration => {
                write!(f, "Duplicate declaration of module.")
            }
            ResolveError::UnboundValueIdentifier(intern_idx) => write!(
                f,
                "Identifier `{}` is not bound to value.",
                intern_idx.idx()
            ),
            ResolveError::UnboundTypeIdentifier(intern_idx) => {
                write!(f, "Identifier `{}` is not bound to type.", intern_idx.idx())
            }
        }
    }
}

type ResolveResult = Result<(), Located<ResolveError>>;
