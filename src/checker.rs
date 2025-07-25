use std::collections::{HashMap, HashSet};

use crate::{
    bound::{Bound, Path},
    declaration::{self, Declaration, InterfaceDeclaration, MethodDeclaration, MethodSignature, Module, ProcedureDeclaration, VariantDeclaration},
    expression::{
        ApplicationExpression, Expression, PathExpression, PathTypeExpression, ProcedureTypeExpression, ProjectionExpression, TypeApplicationExpression, TypeExpression
    },
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern},
    typ::{Interface, MonoType, ProcedureType, Type, TypeVar},
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

struct VariantInformation {
    ty: Type,
    cases: HashMap<InternIdx, Vec<MonoType>>,
    methods: HashMap<InternIdx, ProcedureType>,
}

impl VariantInformation {
    fn with_type(ty: Type) -> Self {
        Self {
            ty,
            cases: HashMap::new(),
            methods: HashMap::new(),
        }
    }
}

const INTERFACE_CONSTANT_IDX: usize = 0;

pub struct Checker {
    names: HashMap<Path, Type>,
    variants: HashMap<Path, VariantInformation>,
    interfaces: HashMap<Path, Interface>,

    // TODO: Seperate type locals and value locals
    locals: Vec<Type>,
    return_type: Option<MonoType>,

    type_var_counter: usize,

    current_source: String,

    lastest_unification: HashMap<usize, MonoType>
}

impl Checker {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            variants: HashMap::new(),
            interfaces: HashMap::new(),
            locals: vec![],
            return_type: None,
            // NOTE: 0 is reserved for interfaces' self reference type constant
            type_var_counter: 1,
            current_source: String::new(),
            lastest_unification: HashMap::new(),
        }
    }

    fn newvar(&mut self) -> TypeVar {
        let idx = self.type_var_counter;
        self.type_var_counter += 1;
        TypeVar { idx, interfaces: HashSet::new() }
    }

    fn instantiate(&mut self, t: Type) -> MonoType {
        match t {
            Type::Mono(m) => m,
            Type::Forall(vars, m) => {
                let map = vars
                    .iter()
                    .map(|var| (var.idx, MonoType::Var(self.newvar())))
                    .collect();

                m.replace_type_vars(&map)
            },
        }
    }

    #[allow(unused)]
    fn generalize(&mut self, m: MonoType) -> Type {
        let vars = m.occuring_type_vars();
        Type::Forall(vars, m)
    }

    fn type_vars(&mut self, type_vars: &[Located<declaration::TypeVar>]) -> Vec<TypeVar> {
        let mut vars = vec![];
        for var in type_vars {
            let mut newvar = self.newvar();
            for (_, path) in &var.data().interfaces {
                newvar.interfaces.insert(path.clone());
            }

            vars.push(newvar);
        }

        vars
    }

    fn eval_type_expression(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<Type> {
        match type_expression.data() {
            TypeExpression::Path(type_path) => self.eval_path_type(type_path),
            TypeExpression::Procedure(procedure_type) => self.eval_procedure_type(procedure_type),
            // TODO: Inconsistent naming?
            TypeExpression::Application(type_application) => self.eval_type_application(type_application),
        }
    }

    fn eval_path_type(&mut self, type_path: &PathTypeExpression) -> ReportableResult<Type> {
        let PathTypeExpression { bound, .. } = type_path;

        match bound {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            }
            Bound::Absolute(path) => Ok(self.variants[path].ty.clone()),
            Bound::Undetermined => unreachable!(),
        }
    }

    fn eval_procedure_type(&mut self, procedure_type: &ProcedureTypeExpression) -> ReportableResult<Type> {
        let ProcedureTypeExpression { arguments, return_type } = procedure_type;

        let mut argument_types = vec![];
        for argument in arguments {
            argument_types.push(self.eval_to_mono(argument)?);
        }

        let return_type = Box::new(self.eval_to_mono(return_type)?);

        let procedure_type = ProcedureType { arguments: argument_types, return_type };
        Ok(Type::Mono(MonoType::Procedure(procedure_type)))
    }

    fn eval_type_application(&mut self, type_application: &TypeApplicationExpression) -> ReportableResult<Type> {
        let TypeApplicationExpression { function, arguments } = &type_application;

        let t = self.eval_type_expression(function)?;
        let Type::Forall(vars, mut m) = t else {
            return self.error(
                TypeCheckError::NotAPolyType {
                    encountered: t
                },
                function.location()
            )
        };

        if vars.len() != arguments.len() {
            return self.error(
                TypeCheckError::TypeArityMismatch {
                    expected: vars.len(),
                    encountered: arguments.len()
                },
                function.location()
            )
        }

        match m {
            MonoType::Variant(_, ref mut variant_arguments) => {
                variant_arguments.clear();
                for (argument, var) in arguments.iter().zip(vars) {
                    let m = self.eval_to_mono(argument)?;
                    if !self.is_supertype_of_interface(m.clone(), &var.interfaces) {
                        return self.error(
                            TypeCheckError::DontImplementInterfaces {
                                t: Type::Mono(m),
                                interfaces: var.interfaces
                            },
                            argument.location()
                        );
                    }
                    variant_arguments.push(m);
                }
            },
            MonoType::Procedure(_procedure_type) => todo!(),

            MonoType::Constant(_) |
            MonoType::Var(..) => unreachable!(),
        };

        Ok(Type::Mono(m))
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
                Declaration::Interface(interface) => self.collect_interface_type(interface)?,
                Declaration::Variant(variant) => self.collect_variant_type(variant)?,
                _ => ()
            }
        }

        Ok(())
    }

    // TODO: This is probably not a solution, proper way is to build a some sort of dependecy graph, I guess
    fn collect_names(&mut self, module: &Module) -> ReportableResult<()> {
        let mut tricky_ones_interface = vec![];
        'declaration: for declaration in module.declarations() {
            match declaration {
                Declaration::Interface(interface) => {
                    let InterfaceDeclaration { name, methods, .. } = interface;
                    for MethodSignature { arguments, return_type, .. } in methods {
                        if arguments.iter().any(|arg| arg.data().type_expression().data().contains_application())
                        || return_type.data().contains_application() {
                            tricky_ones_interface.push(*name.data());
                            continue 'declaration;
                        }
                    }
                    self.collect_interface_name(interface)?
                },
                _ => (),
            }
        }

        let mut tricky_ones_variant = vec![];
        'declaration: for declaration in module.declarations() {
            match declaration {
                Declaration::Variant(variant) => {
                    let VariantDeclaration { name, methods, .. } = variant;
                    for MethodDeclaration { arguments, return_type, .. } in methods {
                        if arguments.iter().any(|arg| arg.data().type_expression().data().contains_application())
                        || return_type.data().contains_application() {
                            tricky_ones_variant.push(*name.data());
                            continue 'declaration;
                        }
                    }
                    self.collect_variant_name(variant)?
                },
                _ => (),
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Interface(interface) => {
                    let InterfaceDeclaration { name, .. } = interface;
                    if tricky_ones_interface.contains(name.data()) {
                        self.collect_interface_name(interface)?
                    }
                },
                _ => (),
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Variant(variant) => {
                    let VariantDeclaration { name, .. } = variant;
                    if tricky_ones_variant.contains(name.data()) {
                        self.collect_variant_name(variant)?
                    }
                },
                _ => (),
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Procedure(procedure) => self.collect_procedure_name(procedure)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn collect_procedure_name(&mut self, procedure: &ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { type_vars, arguments, return_type, path, .. } = procedure;

        let type_vars = self.type_vars(type_vars);

        scoped!(self, {
            self.locals.extend(type_vars.iter().map(|var| Type::Mono(MonoType::Var(var.clone()))));

            let mut argument_types = vec![];
            for argument in arguments {
                argument_types.push(self.eval_to_mono(argument.data().type_expression())?);
            }

            let return_type = Box::new(self.eval_to_mono(return_type)?);

            let procedure_type = ProcedureType { arguments: argument_types, return_type };
            let procedure = MonoType::Procedure(procedure_type);
            let t = if type_vars.is_empty() {
                Type::Mono(procedure)
            } else {
                Type::Forall(type_vars, procedure)
            };

            self.names.insert(path.clone(), t);
        });

        Ok(())
    }

    fn collect_variant_type(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { path, type_vars, .. } = variant;

        let variant_type = if type_vars.is_empty() {
            Type::Mono(MonoType::Variant(path.clone(), vec![]))
        } else {
            let vars = self.type_vars(type_vars);
            Type::Forall(vars, MonoType::Variant(path.clone(), vec![]))
        };
        let variant_data = VariantInformation::with_type(variant_type);
        self.variants.insert(path.clone(), variant_data);

        Ok(())
    }

    fn collect_interface_type(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { path, .. } = interface;

        self.interfaces.insert(path.clone(), Interface { methods: HashMap::new() });

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        scoped!(self, {
            if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                self.locals.extend(vars.iter().map(|var| Type::Mono(MonoType::Constant(var.clone()))));
            }

            for method in methods {
                let MethodDeclaration { name, arguments, return_type, .. } = method;

                let mut argument_types = vec![];
                for argument in arguments {
                    argument_types.push(self.eval_to_mono(argument.data().type_expression())?);
                }

                let return_type = Box::new(self.eval_to_mono(return_type)?);
                let procedure_type = ProcedureType { arguments: argument_types, return_type };
                if self.variants
                    .get_mut(path)
                    .unwrap()
                    .methods
                    .insert(*name.data(), procedure_type)
                    .is_some()
                {
                    return self.error(
                        TypeCheckError::DuplicateMethodDeclaration {
                            variant_path: path.clone(),
                            method_name: *name.data(),
                        },
                        method.name.location(),
                    );
                };
            }
        });

        scoped!(self, {
            if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                self.locals.extend(vars.iter().map(|var| Type::Mono(MonoType::Var(var.clone()))));
            };

            let variant_type = match self.variants[path].ty.clone() {
                Type::Mono(m) => m,
                Type::Forall(vars, _) => {
                    let type_vars = vars.iter().map(|var| MonoType::Var(var.clone()));
                    MonoType::Variant(path.clone(), type_vars.collect())
                },
            };

            for case in cases {
                let case_name = *case.data().identifier().data();
                let case_path = case.data().path().clone();

                if let Some(arguments) = case.data().arguments() {
                    let mut argument_types = vec![];
                    for argument in arguments {
                        argument_types.push(self.eval_to_mono(argument)?);
                    }

                    self.variants
                        .get_mut(path)
                        .unwrap()
                        .cases
                        .insert(case_name, argument_types.clone());

                    let procedure_type = ProcedureType {
                        arguments: argument_types,
                        return_type: Box::new(variant_type.clone())
                    };

                    // Generalization
                    let t = if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                        Type::Forall(vars, MonoType::Procedure(procedure_type))
                    } else {
                        Type::Mono(MonoType::Procedure(procedure_type))
                    };

                    self.names.insert(case_path, t);
                } else {
                    self.variants
                        .get_mut(path)
                        .unwrap()
                        .cases
                        .insert(case_name, vec![]);

                    // Generalization
                    let t = if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                        Type::Forall(vars, variant_type.clone())
                    } else {
                        Type::Mono(variant_type.clone())
                    };

                    self.names.insert(case_path, t);
                }
            }
        });

        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> ReportableResult<()> {
        match declaration {
            Declaration::Module(..) |
            Declaration::Import(..) |
            Declaration::Interface(..) => Ok(()),
            Declaration::Variant(variant) => self.variant(variant),
            Declaration::Procedure(procedure) => self.procedure(procedure),
        }
    }

    fn variant(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { methods, path, .. } = variant;

        for method in methods {
            let MethodDeclaration { name, body, .. } = method;

            let ProcedureType { arguments, return_type } = self.variants[path].methods[name.data()].clone();

            if let Some(statement) = body.last() {
                if !statement.data().returns() {
                    return self.error(
                        TypeCheckError::MethodDoesNotReturn {
                            type_path: path.clone(),
                            method: *method.name.data(),
                            expceted: Type::Mono(*return_type),
                        },
                        method.name.location(),
                    );
                }
            } else {
                return self.error(
                    TypeCheckError::MethodDoesNotReturn {
                        type_path: path.clone(),
                        method: *method.name.data(),
                        expceted: Type::Mono(*return_type),
                    },
                    method.name.location(),
                );
            }

            let variant_type = self.variants[path].ty.clone();
            let variant_type = match variant_type {
                Type::Mono(t) => t,
                Type::Forall(vars, _) => {
                    let type_vars = vars.iter().map(|var| MonoType::Constant(var.clone()));
                    MonoType::Variant(path.clone(), type_vars.collect())
                },
            };

            scoped!(self, {
                self.locals.push(Type::Mono(variant_type));
                self.locals.extend(arguments.into_iter().map(Type::Mono));
                self.return_type = Some(*return_type.clone());
                for statement in body {
                    self.statement(statement)?;
                }
                self.return_type = None;
            });
        }

        Ok(())
    }

    fn procedure(&mut self, procedure: &ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { name, body, path, .. } = procedure;

        let procedure = match self.names[path].clone() {
            Type::Mono(t) => {
                let MonoType::Procedure(procedure) = t else {
                    unreachable!()
                };
                procedure
            },
            Type::Forall(vars, t) => {
                let MonoType::Procedure(procedure) = t else {
                    unreachable!()
                };
                let map = vars.iter().map(|var| (var.idx, MonoType::Constant(var.clone()))).collect();
                let t = MonoType::Procedure(procedure.clone()).replace_type_vars(&map);
                let MonoType::Procedure(procedure) = t else {
                    unreachable!()
                };
                procedure
            }
        };
        let ProcedureType { arguments, return_type } = procedure;

        if let Some(statement) = body.last() {
            if !statement.data().returns() {
                return self.error(
                    TypeCheckError::ProcedureDoesNotReturn {
                        procedure: path.clone(),
                        expceted: Type::Mono(*return_type),
                    },
                    name.location(),
                );
            }
        } else {
            return self.error(
                TypeCheckError::ProcedureDoesNotReturn {
                    procedure: path.clone(),
                    expceted: Type::Mono(*return_type),
                },
                name.location(),
            );
        }

        scoped!(self, {
            self.locals.extend(arguments.into_iter().map(Type::Mono));
            self.return_type = Some(*return_type.clone());
            for statement in body {
                self.statement(statement)?;
            }
            self.return_type = None;
        });

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { methods, path, .. } = interface;

        scoped!(self, {
            self.locals.push(Type::Mono(MonoType::Constant(TypeVar {
                idx: INTERFACE_CONSTANT_IDX,
                interfaces: HashSet::new()
            })));

            for method in methods {
                let MethodSignature { name, arguments, return_type } = method;

                let mut new_arguments = vec![];
                for argument in arguments {
                    new_arguments.push(self.eval_to_mono(argument.data().type_expression())?)
                }

                let new_return_type = Box::new(self.eval_to_mono(return_type)?);

                let method_ty = ProcedureType {
                    arguments: new_arguments,
                    return_type: new_return_type
                };
                self.interfaces.get_mut(path).unwrap().methods.insert(*name.data(), method_ty);
            }
        });

        Ok(())
    }

    fn statement(&mut self, statement: &Located<Statement>) -> ReportableResult<()> {
        match statement.data() {
            Statement::Expression(expression) => self.infer(expression).map(|_| ()),
            Statement::Match(matc) => self.matc(matc),
            Statement::Return(retrn) => self.retrn(retrn),
        }
    }

    fn retrn(&mut self, retrn: &ReturnStatement) -> ReportableResult<()> {
        let ReturnStatement { expression } = retrn;

        let Some(t) = self.return_type.clone() else {
            unreachable!();
        };
        self.check(expression, t)
    }

    fn matc(&mut self, matc: &MatchStatement) -> ReportableResult<()> {
        // TODO: Exhaustiveness check
        let MatchStatement { expression, branches } = matc;

        let mut t = self.infer(expression)?;
        for branch in branches {
            scoped!(self, {
                if !self.type_pattern_match(t.clone(), branch.data().pattern())? {
                    // TODO: Remove push locals by type_pattern_match()
                    return self.error(
                        TypeCheckError::NotAPatternOfType { expected: Type::Mono(t) },
                        branch.data().pattern().location(),
                    );
                }

                self.statement(branch.data().statement())?;

                if t.contains_type_var() {
                    t = t.replace_type_vars(&self.lastest_unification);
                }
            })
        }

        Ok(())
    }

    fn type_pattern_match(&mut self, t: MonoType, pattern: &Located<Pattern>) -> ReportableResult<bool> {
        match (t, pattern.data()) {
            (MonoType::Variant(path, arguments), Pattern::VariantCase(variant_case)) => {
                let VariantCasePattern { name, fields } = variant_case;

                let cases = &self.variants[&path].cases;
                if !cases.contains_key(name.data()) {
                    return self.error(
                        TypeCheckError::CaseNotExist {
                            type_path: path,
                            case_name: *name.data(),
                        },
                        name.location(),
                    );
                }

                let case_fields = &cases[name.data()];

                let fields_len = fields.as_ref().map(|fields| fields.len()).unwrap_or(0);

                if case_fields.len() != fields_len {
                    return self.error(
                        TypeCheckError::WrongCaseArity {
                            type_path: path,
                            case_name: *name.data(),
                            expected: case_fields.len(),
                            encountered: fields_len
                        },
                        pattern.location(),
                    );
                }

                if let Type::Forall(type_vars, _) = &self.variants[&path].ty {
                    let type_var_map = type_vars
                        .to_owned()
                        .into_iter()
                        .map(|var| var.idx)
                        .zip(arguments)
                        .collect();

                    for ty in case_fields {
                        let ty = ty.clone().replace_type_vars(&type_var_map);
                        self.locals.push(Type::Mono(ty.clone()));
                    }
                } else {
                    for ty in case_fields {
                        self.locals.push(Type::Mono(ty.clone()));
                    }
                }

                Ok(true)
            }
            (MonoType::Procedure(..), _) => Ok(false),
            (MonoType::Var(..), _) => Ok(false),
            (MonoType::Constant(..), _) => Ok(false),
        }
    }

    fn check(&mut self, expression: &Located<Expression>, expected: MonoType) -> ReportableResult<()> {
        let encountered = match expression.data() {
            Expression::Path(..) |
            Expression::Application(..) |
            Expression::Projection(..) => self.infer(expression)?
        };

        let mut map = HashMap::new();
        if !self.unify(encountered.clone(), expected.clone(), &mut map) {
            return self.error(
                TypeCheckError::MismatchedTypes {
                    encountered: Type::Mono(encountered),
                    expected: Type::Mono(expected),
                },
                expression.location(),
            );
        };

        let encountered = encountered.replace_type_vars(&map);
        let expected = expected.replace_type_vars(&map);

        if encountered != expected {
            return self.error(
                TypeCheckError::MismatchedTypes {
                    encountered: Type::Mono(encountered),
                    expected: Type::Mono(expected),
                },
                expression.location(),
            );
        }

        Ok(())
    }

    fn is_supertype_of_interface(&self, t: MonoType, interfaces: &HashSet<Path>) -> bool {
        match &t {
            MonoType::Variant(path, arguments) => {
                for interface_path in interfaces {
                    let interface = &self.interfaces[interface_path];

                    for (name, interface_procedure) in &interface.methods {
                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, t.clone())]);

                        let MonoType::Procedure(interface_procedure) =
                            MonoType::Procedure(interface_procedure.clone()).replace_type_constants(&map) else {
                            unreachable!()
                        };

                        let Some(variant_procedure) = self.variants[&path].methods.get(&name) else {
                            return false;
                        };

                        let variant_procedure = if let Type::Forall(type_vars, _) = &self.variants[&path].ty {
                            let map = type_vars
                                .iter()
                                .cloned()
                                .map(|var| var.idx)
                                .zip(arguments.clone())
                                .collect();

                            let MonoType::Procedure(variant_procedure) =
                                MonoType::Procedure(variant_procedure.clone()).replace_type_constants(&map) else {
                                unreachable!()
                            };

                            variant_procedure
                        } else {
                            variant_procedure.clone()
                        };

                        if variant_procedure != interface_procedure {
                            return false;
                        }
                    }
                }

                true
            },
            MonoType::Var(type_var) | MonoType::Constant(type_var) => {
                for path in interfaces {
                    let interface = &self.interfaces[path];

                    for (name, interface_procedure) in &interface.methods {
                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, t.clone())]);

                        let MonoType::Procedure(interface_procedure) =
                        MonoType::Procedure(interface_procedure.clone()).replace_type_constants(&map) else {
                            unreachable!()
                        };

                        let mut var_procedure = None;
                        'interface: for path in &type_var.interfaces {
                            for (method_name, procedure) in &self.interfaces[path].methods {
                                if name == method_name {
                                    var_procedure = Some(procedure);
                                    break 'interface;
                                }
                            }
                        }
                        let Some(var_procedure) = var_procedure.take() else {
                            return false;
                        };

                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, t.clone())]);

                        let MonoType::Procedure(var_procedure) =
                        MonoType::Procedure(var_procedure.clone()).replace_type_constants(&map) else {
                            unreachable!()
                        };

                        if var_procedure != interface_procedure {
                            return false;
                        }
                    }
                }
                true
            }
            _ => interfaces.is_empty()
        }
    }

    fn infer(&mut self, expression: &Located<Expression>) -> ReportableResult<MonoType> {
        match expression.data() {
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
        }
    }

    fn path(&mut self, path: &PathExpression) -> ReportableResult<MonoType> {
        let PathExpression { bound, .. } = path;

        match bound {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                let t = self.locals[index].clone();
                Ok(self.instantiate(t))
            }
            Bound::Absolute(path) => {
                let t = self.names[path].clone();
                Ok(self.instantiate(t))
            },
            Bound::Undetermined => unreachable!(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression) -> ReportableResult<MonoType> {
        let ApplicationExpression { function, arguments } = application;

        let t = self.infer(function)?;
        match t {
            MonoType::Procedure(procedure) => {
                let ProcedureType { arguments: arguments_type, return_type } = procedure;

                if arguments.len() != arguments_type.len() {
                    return self.error(
                        TypeCheckError::ArityMismatch {
                            expected: arguments_type.len(),
                            encountered: arguments.len()
                        },
                        function.location()
                    );
                }

                if !arguments_type.iter().any(MonoType::contains_type_var) && !return_type.contains_type_var() {
                    for (argument, ty) in arguments.iter().zip(arguments_type) {
                        self.check(argument, ty)?;
                    }

                    return Ok(*return_type.clone())
                }


                let mut value_types = vec![];
                for argument in arguments {
                    value_types.push((self.infer(argument)?, argument.location()));
                }

                let mut map = HashMap::new();
                for ((argument, location), ty) in value_types.iter().zip(arguments_type) {
                    let old_map = map.clone();
                    if !self.unify(argument.clone(), ty.clone(), &mut map) {
                        return self.error(
                            TypeCheckError::MismatchedTypes {
                                encountered: Type::Mono(argument.clone().replace_type_vars(&map)),
                                expected: Type::Mono(ty.replace_type_vars(&old_map))
                            },
                            *location
                        );
                    };
                }

                let idx = self.newvar();
                self.unify(*return_type.clone(), MonoType::Var(idx.clone()), &mut map);

                let mut new_argument_types = vec![];
                for (argument, _) in value_types {
                    new_argument_types.push(argument.replace_type_vars(&map));
                }

                let new_return = return_type.clone().replace_type_vars(&map);

                Ok(new_return)
            }
            _ => {
                return self.error(
                    TypeCheckError::ExpectedAProcedure { encountered: Type::Mono(t) },
                    function.location()
                );
            }
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> ReportableResult<MonoType> {
        let ProjectionExpression { expression, name } = projection;

        let t = self.infer(expression)?;

        let (path, arguments) = match &t {
            MonoType::Variant(path, arguments) => (path, arguments),
            MonoType::Constant(type_var) | MonoType::Var(type_var) => {
                let TypeVar { interfaces, .. } = type_var;

                let mut var_procedure = None;
                'interface: for path in interfaces {
                    for (method_name, procedure) in &self.interfaces[path].methods {
                        if name.data() == method_name {
                            var_procedure = Some(procedure);
                            break 'interface;
                        }
                    }
                }
                let Some(var_procedure) = var_procedure.take() else {
                    return self.error(
                        TypeCheckError::HasNoMethod {
                            ty: Type::Mono(t),
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                let map = HashMap::from([(INTERFACE_CONSTANT_IDX, t.clone())]);

                return Ok(MonoType::Procedure(var_procedure.clone()).replace_type_constants(&map));
            }
            _ => {
                return self.error(
                    TypeCheckError::HasNoMethod {
                        ty: Type::Mono(t),
                        name: *name.data()
                    },
                    name.location()
                );
            }
        };

        let Some(method_ty) = self.variants[path].methods.get(name.data()) else {
            return self.error(
                TypeCheckError::HasNoMethod {
                    ty: Type::Mono(t),
                    name: *name.data()
                },
                name.location()
            );
        };

        if let Type::Forall(type_vars, _) = &self.variants[&path].ty {
            let type_var_map = type_vars
                .iter()
                .cloned()
                .map(|var| var.idx)
                .zip(arguments.clone())
                .collect();

            Ok(MonoType::Procedure(method_ty.clone()).replace_type_constants(&type_var_map))
        } else {
            Ok(MonoType::Procedure(method_ty.clone()))
        }
    }

    fn unify(&mut self, a: MonoType, b: MonoType, map: &mut HashMap<usize, MonoType>) -> bool {
        let result = match (a, b) {
            (MonoType::Variant(p1, args1), MonoType::Variant(p2, args2)) => {
                if p1 != p2 {
                    return false;
                }

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !self.unify(arg1, arg2, map) {
                        return false;
                    }
                }

                true
            },
            (MonoType::Procedure(p1), MonoType::Procedure(p2)) => {
                let ProcedureType { arguments: args1, return_type: r1 } = p1;
                let ProcedureType { arguments: args2, return_type: r2 } = p2;

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !self.unify(arg1, arg2, map) {
                        return false;
                    }
                }

                self.unify(*r1, *r2, map)
            },

            (MonoType::Constant(idx1), MonoType::Constant(idx2)) => idx1 == idx2,

            (MonoType::Var(var1), MonoType::Var(var2)) => {
                let TypeVar { idx: idx1, interfaces: interfaces1 } = var1;
                let TypeVar { idx: idx2, interfaces: interfaces2 } = var2;

                let mut interfaces = interfaces1.clone();
                if map.contains_key(&idx1) {
                    if let Some(mut v) = map.insert(idx2, map[&idx1].clone()) {
                        while let MonoType::Var(var) = v {
                            // TODO: check colisions here
                            interfaces.extend(var.interfaces);
                            v = map[&var.idx].clone()
                        }

                        if v != map[&idx1].clone() {
                            return false;
                        }

                        map.insert(idx1, MonoType::Var(TypeVar { idx: idx2, interfaces }));
                    }
                } else {
                    // TODO: check colisions here
                    interfaces.extend(interfaces2);
                    map.insert(idx1, MonoType::Var(TypeVar { idx: idx2, interfaces }));
                }

                true
            },

            (t, MonoType::Var(var)) | (MonoType::Var(var), t) => {
                let TypeVar { idx, mut interfaces } = var.clone();

                if !t.occurs(idx) {
                    if let Some(mut v) = map.insert(idx, t.clone()) {
                        while let MonoType::Var(var) = v {
                            // TODO: check confilicts here
                            interfaces.extend(var.interfaces);
                            v = map[&var.idx].clone()
                        }

                        if v != t {
                            return false;
                        }
                    }

                    if !self.is_supertype_of_interface(t.clone(), &interfaces) {
                        return false
                    }

                    map.insert(idx, t);

                    true
                } else {
                    false
                }
            },
            _ => false
        };

        if result {
            for local in self.locals.iter_mut() {
                if let Type::Mono(m) = local.clone() {
                    *local = Type::Mono(m.replace_type_vars(&map));
                }
            }
        }

        self.lastest_unification.extend(map.clone());

        result
    }

    fn eval_to_mono(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<MonoType> {
        let t = self.eval_type_expression(type_expression)?;
        let Type::Mono(m) = t else {
            return self.error(
                TypeCheckError::ExpectedMonoType { encountered: t },
                type_expression.location()
            );
        };

        Ok(m)
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
    WrongCaseArity {
        type_path: Path,
        case_name: InternIdx,
        expected: usize,
        encountered: usize,
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
    ExpectedMonoType {
        encountered: Type,
    },
    NotAPolyType {
        encountered: Type,
    },
    TypeArityMismatch {
        expected: usize,
        encountered: usize,
    },
    DontImplementInterfaces {
        t: Type,
        interfaces: HashSet<Path>,
    }
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
            TypeCheckError::WrongCaseArity {
                type_path,
                case_name,
                expected,
                encountered,
            } => {
                format!(
                    "`{}` of variant `{}` takes `{}` values but supplied `{}` values.",
                    interner.get(case_name),
                    type_path.as_string(interner),
                    expected,
                    encountered
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
            TypeCheckError::ExpectedMonoType { encountered } => {
                format!("Type is not mono : `{}`.", encountered.display(interner))
            }
            TypeCheckError::NotAPolyType { encountered } => {
                format!("Type is not poly : `{}`.", encountered.display(interner))
            }
            TypeCheckError::TypeArityMismatch { encountered, expected } => {
                format!("Type is of arity {} but supplied {} arguments.",
                    expected, encountered
                )
            }
            TypeCheckError::DontImplementInterfaces { t, interfaces } => {
                format!("Type `{}` does not implement interfaces: {}.",
                    t.display(interner), interfaces.iter().map(|path| path.as_string(interner)).collect::<Vec<_>>().join(" ")
                )
            }
        }
    }
}
