use std::collections::HashMap;

use crate::{
    bound::{Bound, Path},
    declaration::{Declaration, Method, Module, TypedIdentifier, VariantCase},
    expression::{Expression, TypeExpression},
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchBranch, Pattern, Statement},
    typ::Type,
};

pub struct Checker {
    names: HashMap<Path, Type>,
    variants: HashMap<
        Path,
        (
            Type,
            HashMap<InternIdx, HashMap<InternIdx, Type>>,
            // TODO: here we have only methods so maybe store only
            //   a procedure type instead of Type
            HashMap<InternIdx, Type>,
        ),
    >,

    locals: Vec<Type>,
    return_type: Option<Type>,

    current_source: String,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            variants: HashMap::new(),
            locals: vec![],
            return_type: None,
            current_source: String::new(),
        }
    }

    fn eval_type_expression(
        &mut self,
        type_expression: &Located<TypeExpression>,
    ) -> ReportableResult<Type> {
        match type_expression.data() {
            TypeExpression::Path(_, bound) => self.eval_type_path(bound),
            TypeExpression::Procedure { arguments, return_type } => self.eval_type_procedure(arguments, return_type),
        }
    }

    fn eval_type_path(&mut self, bound: &Bound) -> ReportableResult<Type> {
        match bound {
            Bound::Local(_) => todo!("Type variables."),
            Bound::Absolute(path) => Ok(self.variants[path].0.clone()),
            Bound::Undetermined => unreachable!(),
        }
    }

    fn eval_type_procedure(
        &mut self,
        arguments: &[Located<TypeExpression>],
        return_type: &Located<TypeExpression>
    ) -> ReportableResult<Type> {
        let mut argument_types = vec![];
        for argument in arguments {
            argument_types.push(self.eval_type_expression(argument)?);
        }

        let return_type = Box::new(self.eval_type_expression(return_type)?);

        Ok(Type::Procedure { arguments: argument_types, return_type })
    }

    pub fn type_check(&mut self, modules: &[Module]) -> ReportableResult<()> {
        for module in modules {
            self.current_source = module.source().to_string();
            self.collect_types(module)?;
        }

        for module in modules {
            self.current_source = module.source().to_string();
            self.collect_names(module)?;
        }

        for module in modules {
            self.current_source = module.source().to_string();
            self.module(module)?;
        }

        Ok(())
    }

    fn module(&mut self, module: &Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn collect_types(&mut self, module: &Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Module { .. } => (),
                Declaration::Import { .. } => (),
                Declaration::Procedure { .. } => (),
                Declaration::Variant { path, .. } => self.collect_variant_type(path.clone())?,
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, module: &Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Variant {
                    cases,
                    methods,
                    path,
                    ..
                } => {
                    self.collect_variant_name(cases, methods, path.clone())?;
                }
                Declaration::Procedure {
                    arguments,
                    return_type,
                    path,
                    ..
                } => self.collect_procedure_name(arguments, return_type, path.clone())?,

                _ => (),
            }
        }

        Ok(())
    }

    fn collect_procedure_name(
        &mut self,
        arguments: &[Located<TypedIdentifier>],
        return_type: &Located<TypeExpression>,
        path: Path,
    ) -> ReportableResult<()> {
        let mut argument_types = vec![];
        for argument in arguments {
            argument_types.push(self.eval_type_expression(argument.data().type_expression())?);
        }

        let return_type = self.eval_type_expression(return_type)?;

        self.names.insert(
            path,
            Type::Procedure {
                arguments: argument_types,
                return_type: Box::new(return_type),
            },
        );

        Ok(())
    }

    fn collect_variant_type(&mut self, path: Path) -> ReportableResult<()> {
        let variant_type = Type::Variant(path.clone());
        let variant = (variant_type, HashMap::new(), HashMap::new());
        self.variants.insert(path, variant);

        Ok(())
    }

    fn collect_variant_name(
        &mut self,
        cases: &[Located<VariantCase>],
        methods: &[Method],
        path: Path,
    ) -> ReportableResult<()> {
        for method in methods {
            let mut argument_types = vec![];
            for argument in &method.arguments {
                argument_types.push(self.eval_type_expression(argument.data().type_expression())?);
            }

            let return_type = self.eval_type_expression(&method.return_type)?;
            let method_type = Type::Procedure {
                arguments: argument_types,
                return_type: Box::new(return_type),
            };
            if self
                .variants
                .get_mut(&path)
                .unwrap()
                .2
                .insert(*method.name.data(), method_type)
                .is_some()
            {
                return self.error(
                    TypeCheckError::DuplicateMethodDeclaration {
                        variant_path: path,
                        method_name: *method.name.data(),
                    },
                    method.name.location(),
                );
            };
        }

        let variant_type = self.variants[&path].0.clone();
        for case in cases {
            if let Some(arguments) = case.data().arguments() {
                let mut argument_types_vec = vec![];
                let mut argument_types = HashMap::new();
                for argument in arguments {
                    let argument_type =
                        self.eval_type_expression(argument.data().type_expression())?;
                    if argument_types
                        .insert(*argument.data().indentifier().data(), argument_type.clone())
                        .is_some()
                    {
                        return self.error(
                            TypeCheckError::DuplicateConstructorFieldDeclaration {
                                variant_path: path.clone(),
                                constructor_name: *case.data().identifier().data(),
                                field_name: *argument.data().indentifier().data(),
                            },
                            argument.location(),
                        );
                    }
                    argument_types_vec.push(argument_type);
                }
                self.variants
                    .get_mut(&path)
                    .unwrap()
                    .1
                    .insert(*case.data().identifier().data(), argument_types);
                self.names.insert(
                    case.data().path().clone(),
                    Type::Procedure {
                        arguments: argument_types_vec,
                        return_type: Box::new(variant_type.clone()),
                    },
                );
            } else {
                self.names
                    .insert(case.data().path().clone(), variant_type.clone());
                self.variants
                    .get_mut(&path)
                    .unwrap()
                    .1
                    .insert(*case.data().identifier().data(), HashMap::new());
                self.names
                    .insert(case.data().path().clone(), variant_type.clone());
            }
        }

        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> ReportableResult<()> {
        match declaration {
            Declaration::Module { .. } | Declaration::Import { .. } => Ok(()),
            Declaration::Variant { methods, path, .. } => self.variant_methods(path, methods),
            Declaration::Procedure {
                body, path, name, ..
            } => self.procedure(path, body, name.location()),
        }
    }

    fn variant_methods(&mut self, path: &Path, methods: &[Method]) -> ReportableResult<()> {
        for method in methods {
            let Type::Procedure {
                arguments,
                return_type,
            } = self.variants[path].2[method.name.data()].clone()
            else {
                unreachable!();
            };

            let mut returns = false;
            for statement in &method.body {
                if Self::returns(statement) {
                    returns = true;
                }
            }

            if !returns {
                return self.error(
                    TypeCheckError::MethodDoesNotReturn {
                        type_path: path.clone(),
                        method: *method.name.data(),
                        expceted: *return_type,
                    },
                    method.name.location(),
                );
            }

            let variant_type = self.variants[path].0.clone();

            let arguments_len = arguments.len();

            self.locals.push(variant_type);
            self.locals.extend(arguments);
            self.return_type = Some(*return_type.clone());
            for statement in &method.body {
                self.statement(statement)?;
            }
            self.return_type = None;
            self.locals.truncate(self.locals.len() - arguments_len);
            self.locals.pop();
        }

        Ok(())
    }

    fn procedure(
        &mut self,
        path: &Path,
        body: &[Located<Statement>],
        location: SourceLocation,
    ) -> ReportableResult<()> {
        let Type::Procedure {
            arguments,
            return_type,
        } = self.names[path].clone()
        else {
            unreachable!();
        };

        let mut returns = false;
        for statement in body {
            if Self::returns(statement) {
                returns = true;
            }
        }

        if !returns {
            return self.error(
                TypeCheckError::ProcedureDoesNotReturn {
                    procedure: path.clone(),
                    expceted: *return_type,
                },
                location,
            );
        }

        let arguments_len = arguments.len();
        self.locals.extend(arguments);
        self.return_type = Some(*return_type.clone());
        for statement in body {
            self.statement(statement)?;
        }
        self.return_type = None;
        self.locals.truncate(self.locals.len() - arguments_len);

        Ok(())
    }

    fn returns(statement: &Located<Statement>) -> bool {
        match statement.data() {
            Statement::Expression(_) => false,
            Statement::Return(_) => true,
            Statement::Match {
                expression: _,
                branches,
            } => {
                let mut returns = true;
                for branch in branches {
                    if !Self::returns(&branch.data().statement) {
                        returns = false;
                    }
                }
                returns
            },
        }
    }

    fn statement(&mut self, statement: &Located<Statement>) -> ReportableResult<()> {
        match statement.data() {
            Statement::Expression(expression) => {
                self.infer(expression)?;
            }
            Statement::Match {
                expression,
                branches,
            } => {
                self.matc(expression, branches)?;
            }
            Statement::Return(expression) => {
                let Some(ty) = self.return_type.clone() else {
                    unreachable!();
                };
                self.check(expression, ty)?;
            },
        };

        Ok(())
    }

    fn matc(
        &mut self,
        expression: &Located<Expression>,
        branches: &[Located<MatchBranch>],
    ) -> ReportableResult<()> {
        // TODO: Exhaustiveness check

        let locals_len = self.locals.len();
        let ty = self.infer(expression)?;
        for branch in branches {
            if !self.type_pattern_match(ty.clone(), &branch.data().pattern)? {
                self.locals.truncate(locals_len);
                return self.error(
                    TypeCheckError::NotAPatternOfType { expected: ty },
                    branch.data().pattern.location(),
                );
            }

            self.statement(&branch.data().statement)?;
            self.locals.truncate(locals_len);
        }

        Ok(())
    }

    fn type_pattern_match(
        &mut self,
        ty: Type,
        pattern: &Located<Pattern>,
    ) -> ReportableResult<bool> {
        match (ty, pattern.data()) {
            (Type::Variant(path), Pattern::VariantCase { name, fields }) => {
                let cases = &self.variants[&path].1;
                if !cases.contains_key(name.data()) {
                    return self.error(
                        TypeCheckError::CaseNotExist {
                            type_path: path,
                            case_name: *name.data(),
                        },
                        name.location(),
                    );
                }

                let case_fields = &self.variants[&path].1[name.data()];

                let fields = fields.clone().unwrap_or(vec![]);

                if case_fields.len() == fields.len() {
                    for case_field in case_fields {
                        if !fields.iter().any(|field| field.data() == case_field.0) {
                            return self.error(
                                TypeCheckError::WrongFieldNames {
                                    type_path: path,
                                    case_name: *name.data(),
                                },
                                pattern.location(),
                            );
                        }
                    }
                } else {
                    return self.error(
                        TypeCheckError::WrongFieldNames {
                            type_path: path,
                            case_name: *name.data(),
                        },
                        pattern.location(),
                    );
                }

                for field in fields {
                    let Some((_, ty)) = case_fields
                        .iter()
                        .find(|case_field| case_field.0 == field.data())
                    else {
                        unreachable!();
                    };

                    self.locals.push(ty.clone());
                }

                Ok(true)
            }
            (Type::Procedure { .. }, _) => Ok(false),
        }
    }

    fn check(&mut self, expression: &Located<Expression>, expected: Type) -> ReportableResult<()> {
        let encountered = match expression.data() {
            Expression::Path(..) |
            Expression::Application { .. } |
            Expression::Projection { .. } => self.infer(expression)?
        };

        if encountered != expected {
            return self.error(
                TypeCheckError::MismatchedTypes {
                    encountered,
                    expected,
                },
                expression.location(),
            );
        }

        Ok(())
    }

    fn infer(&mut self, expression: &Located<Expression>) -> ReportableResult<Type> {
        match expression.data() {
            Expression::Path(_, bound) => match bound {
                Bound::Local(bound_idx) => {
                    Ok(self.locals[self.locals.len() - 1 - bound_idx.idx()].clone())
                }
                Bound::Absolute(path) => Ok(self.names[path].clone()),
                Bound::Undetermined => unreachable!(),
            },
            Expression::Application {
                function,
                arguments,
            } => {
                let ty = self.infer(function)?;
                let Type::Procedure {
                    arguments: arguments_type,
                    return_type,
                } = ty
                else {
                    return self.error(
                        TypeCheckError::ExpectedAProcedure { encountered: ty },
                        function.location()
                    );
                };

                if arguments.len() != arguments_type.len() {
                    return self.error(
                        TypeCheckError::ArityMismatch {
                            expected: arguments_type.len(),
                            encountered: arguments.len()
                        },
                        function.location()
                    );                }

                for (argument, ty) in arguments.iter().zip(arguments_type) {
                    self.check(argument, ty)?;
                }

                Ok(*return_type.clone())
            }
            Expression::Projection { expression, name } => {
                let ty = self.infer(expression)?;

                let Type::Variant(path) = &ty else {
                    return self.error(
                        TypeCheckError::HasNoMethod {
                            ty,
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                let Some(method_ty) = self.variants[&path].2.get(name.data()) else {
                    return self.error(
                        TypeCheckError::HasNoMethod {
                            ty,
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                let encountered @ Type::Procedure { .. } = method_ty.clone() else {
                    unreachable!();
                };

                Ok(encountered)
            }
        }
    }

    fn error<T>(&self, error: TypeCheckError, location: SourceLocation) -> ReportableResult<T> {
        let reportable = (Located::new(error, location), self.current_source.clone());
        Err(Box::new(reportable))
    }
}

pub enum TypeCheckError {
    MismatchedTypes {
        encountered: Type,
        expected: Type,
    },
    DuplicateMethodDeclaration {
        variant_path: Path,
        method_name: InternIdx,
    },
    DuplicateConstructorFieldDeclaration {
        variant_path: Path,
        constructor_name: InternIdx,
        field_name: InternIdx,
    },
    ProcedureDoesNotReturn {
        procedure: Path,
        expceted: Type,
    },
    MethodDoesNotReturn {
        type_path: Path,
        method: InternIdx,
        expceted: Type,
    },
    CaseNotExist {
        type_path: Path,
        case_name: InternIdx,
    },
    WrongFieldNames {
        type_path: Path,
        case_name: InternIdx,
    },
    NotAPatternOfType {
        expected: Type,
    },
    ExpectedAProcedure {
        encountered: Type,
    },
    ArityMismatch {
        expected: usize,
        encountered: usize,
    },
    HasNoMethod {
        ty: Type,
        name: InternIdx,
    },
}

impl Reportable for (Located<TypeCheckError>, String) {
    fn location(&self) -> SourceLocation {
        self.0.location()
    }

    fn source(&self) -> &str {
        &self.1
    }

    fn description(&self, interner: &Interner) -> String {
        match self.0.data() {
            TypeCheckError::MismatchedTypes {
                encountered,
                expected,
            } => {
                format!(
                    "Expected type `{}` but encountered type `{}`.",
                    expected.display(interner),
                    encountered.display(interner)
                )
            }
            TypeCheckError::DuplicateMethodDeclaration {
                variant_path,
                method_name,
            } => {
                format!(
                    "Duplicate declaration of method `{}` in variant type `{}`.",
                    interner.get(method_name),
                    variant_path.as_string(interner)
                )
            }
            TypeCheckError::DuplicateConstructorFieldDeclaration {
                variant_path,
                constructor_name,
                field_name,
            } => {
                format!(
                    "Duplicate declaration of field `{}` in constructor `{}` of variant type `{}`.",
                    interner.get(field_name),
                    interner.get(constructor_name),
                    variant_path.as_string(interner)
                )
            }
            TypeCheckError::ProcedureDoesNotReturn {
                procedure,
                expceted,
            } => {
                format!(
                    "Procedure `{}` does not always return, expected to return `{}`.",
                    procedure.as_string(interner),
                    expceted.display(interner),
                )
            }
            TypeCheckError::MethodDoesNotReturn {
                type_path,
                method,
                expceted,
            } => {
                format!(
                    "Method `{}` of `{}` does not always return, expected to return `{}`.",
                    interner.get(method),
                    type_path.as_string(interner),
                    expceted.display(interner),
                )
            }
            TypeCheckError::CaseNotExist {
                type_path,
                case_name,
            } => {
                format!(
                    "Variant `{}` does not have a case named `{}`.",
                    type_path.as_string(interner),
                    interner.get(case_name),
                )
            }
            TypeCheckError::WrongFieldNames {
                type_path,
                case_name,
            } => {
                format!(
                    "Wrong field names for `{}` of variant `{}`.",
                    interner.get(case_name),
                    type_path.as_string(interner),
                )
            }
            TypeCheckError::NotAPatternOfType { expected } => {
                format!("Pattern is not of type `{}`.", expected.display(interner),)
            }
            TypeCheckError::ExpectedAProcedure { encountered } => {
                format!("A procedure is expected but encountered `{}`.", encountered.display(interner),)
            }
            TypeCheckError::ArityMismatch { encountered, expected } => {
                format!("Procedure is of arity {} but supplied {} arguments.",
                    expected, encountered
                )
            }
            TypeCheckError::HasNoMethod { ty, name } => {
                format!("`{}` has no method named `{}`.",
                    ty.display(interner), interner.get(name)
                )
            }
        }
    }
}
