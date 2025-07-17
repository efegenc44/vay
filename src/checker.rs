use std::collections::HashMap;

use crate::{
    bound::{Bound, Path},
    declaration::{Declaration, MethodDeclaration, Module, ProcedureDeclaration, VariantDeclaration},
    expression::{
        ApplicationExpression, Expression, PathExpression, PathTypeExpression, ProcedureTypeExpression, ProjectionExpression, TypeApplicationExpression, TypeExpression
    },
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    statement::{MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern},
    typ::{ProcedureType, Type},
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
    cases: HashMap<InternIdx, Vec<Type>>,
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

pub struct Checker {
    names: HashMap<Path, Type>,
    variants: HashMap<Path, VariantInformation>,

    // TODO: Seperate type locals and value locals
    locals: Vec<Type>,
    return_type: Option<Type>,

    type_var_counter: usize,

    current_source: String,
}

impl Checker {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            variants: HashMap::new(),
            locals: vec![],
            return_type: None,
            type_var_counter: 0,
            current_source: String::new(),
        }
    }

    #[allow(unused)]
    fn newvar(&mut self) -> Type {
        let idx = self.type_var_counter;
        self.type_var_counter += 1;
        Type::TypeVar(idx)
    }

    fn newvar_idx(&mut self) -> usize {
        let idx = self.type_var_counter;
        self.type_var_counter += 1;
        idx
    }

    // Basically like immediate instance after generalization
    fn rinst(&mut self, t: Type, map: &mut HashMap<usize, Type>) -> Type {
        match t {
            Type::Variant(path, arguments) => {
                let mut rarguments = vec![];
                for argument in arguments {
                    rarguments.push(self.rinst(argument, map));
                }

                Type::Variant(path, rarguments)
            },
            Type::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                let mut rarguments = vec![];
                for argument in arguments {
                    rarguments.push(self.rinst(argument, map));
                }

                let rreturn = Box::new(self.rinst(*return_type, map));
                let procedure = ProcedureType {
                    arguments: rarguments,
                    return_type: rreturn
                };
                Type::Procedure(procedure)
            },

            Type::TypeVar(idx) => {
                if map.contains_key(&idx) {
                    map[&idx].clone()
                } else {
                    let t = self.newvar();
                    map.insert(idx, t.clone());
                    t
                }
            },

            Type::Forall(_, _) => unreachable!(),
        }
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
            argument_types.push(self.eval_type_expression(argument)?);
        }

        let return_type = Box::new(self.eval_type_expression(return_type)?);

        let procedure_type = ProcedureType { arguments: argument_types, return_type };
        Ok(Type::Procedure(procedure_type))
    }

    fn eval_type_application(&mut self, type_application: &TypeApplicationExpression) -> ReportableResult<Type> {
        let TypeApplicationExpression { function, arguments } = &type_application;

        let Type::Forall(vars, mut ty) = self.eval_type_expression(function)? else {
            todo!("not a parameterized type");
        };

        if vars.len() != arguments.len() {
            todo!("mismatched arity for type parameters");
        }

        match *ty {
            Type::Variant(_, ref mut variant_arguments) => {
                variant_arguments.clear();
                for argument in arguments {
                    variant_arguments.push(self.eval_type_expression(argument)?);
                }
            },
            Type::Procedure(_procedure_type) => todo!(),
            Type::Forall(_, _) |
            Type::TypeVar(_) => unreachable!(),
        };

        Ok(*ty)
    }

    fn replace_type_vars(ty: Type, type_var_map: &HashMap<usize, Type>) -> Type {
        match ty {
            Type::Variant(path, arguments) => {
                let mut new_arguments = vec![];
                for argument in arguments {
                    let ntype = Self::replace_type_vars(argument, type_var_map);
                    new_arguments.push(ntype);
                }

                Type::Variant(path, new_arguments)
            },
            Type::Procedure(procedure_type) => {
                let ProcedureType { arguments, return_type } = procedure_type;

                let mut new_arguments = vec![];
                for argument in arguments {
                    let ntype = Self::replace_type_vars(argument, type_var_map);
                    new_arguments.push(ntype);
                }

                let new_return = Self::replace_type_vars(*return_type, type_var_map);
                let new_procdeure = ProcedureType {
                    arguments: new_arguments,
                    return_type: Box::new(new_return),
                };

                Type::Procedure(new_procdeure)
            },
            Type::TypeVar(id) => {
                if type_var_map.contains_key(&id) {
                    type_var_map[&id].clone()
                } else {
                    Type::TypeVar(id)
                }
            },

            Type::Forall(_, _) => unreachable!(),
        }
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
                Declaration::Module(..) => (),
                Declaration::Import(..) => (),
                Declaration::Procedure(..) => (),
                Declaration::Variant(variant) => self.collect_variant_type(variant)?,
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, module: &Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Variant(variant) => self.collect_variant_name(variant)?,
                Declaration::Procedure(procedure) => self.collect_procedure_name(procedure)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn collect_procedure_name(&mut self, procedure: &ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { arguments, return_type, path, .. } = procedure;

        let mut argument_types = vec![];
        for argument in arguments {
            argument_types.push(self.eval_type_expression(argument.data().type_expression())?);
        }

        let return_type = Box::new(self.eval_type_expression(return_type)?);

        let procedure_type = ProcedureType { arguments: argument_types, return_type };
        self.names.insert(
            path.clone(),
            Type::Procedure(procedure_type),
        );

        Ok(())
    }

    fn collect_variant_type(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { path, type_vars, .. } = variant;

        let variant_type = if type_vars.is_empty() {
            Type::Variant(path.clone(), vec![])
        } else {
            let ty = Box::new(Type::Variant(path.clone(), vec![]));
            let mut vars = vec![];
            for _ in type_vars {
                vars.push(self.newvar_idx());
            }

            Type::Forall(vars, ty)
        };
        let variant_data = VariantInformation::with_type(variant_type);
        self.variants.insert(path.clone(), variant_data);

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        scoped!(self, {
            if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                self.locals.extend(vars.iter().map(|var| Type::TypeVar(*var)));
            };

            for method in methods {
                let MethodDeclaration { name, arguments, return_type, .. } = method;

                let mut argument_types = vec![];
                for argument in arguments {
                    let argument = self.eval_type_expression(argument.data().type_expression())?;
                    argument_types.push(argument);
                }

                let return_type = self.eval_type_expression(return_type)?;
                let return_type = Box::new(return_type);
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

            let poly;
            let variant_type = self.variants[path].ty.clone();
            let variant_type = match variant_type {
                Type::Variant(..) => {
                    poly = false;
                    variant_type
                },
                Type::Forall(vars, _) => {
                    poly = true;
                    let type_vars = vars.iter().map(|var| Type::TypeVar(*var));
                    Type::Variant(path.clone(), type_vars.collect())
                },
                _ => unreachable!(),
            };

            for case in cases {
                let case_name = *case.data().identifier().data();
                let case_path = case.data().path().clone();

                if let Some(arguments) = case.data().arguments() {
                    let mut argument_types = vec![];
                    for argument in arguments {
                        let argument_type = self.eval_type_expression(argument)?;
                        argument_types.push(argument_type);
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

                    let ty = if poly {
                        let mut map = HashMap::new();
                        let procedure = self.rinst(Type::Procedure(procedure_type), &mut map);
                        let vars = map.into_values().map(|t| {
                            let Type::TypeVar(id) = t else { unreachable!() };
                            id
                        }).collect();
                        Type::Forall(vars, Box::new(procedure))
                    } else {
                        Type::Procedure(procedure_type)
                    };

                    self.names.insert(case_path, ty);
                } else {
                    self.variants
                        .get_mut(path)
                        .unwrap()
                        .cases
                        .insert(case_name, vec![]);

                    let ty = if poly {
                        let mut map = HashMap::new();
                        let t = self.rinst(variant_type.clone(), &mut map);
                        let vars = map.into_values().map(|t| {
                            let Type::TypeVar(id) = t else { unreachable!() };
                            id
                        }).collect();
                        Type::Forall(vars, Box::new(t))
                    } else {
                        variant_type.clone()
                    };

                    self.names.insert(case_path, ty);
                }
            }
        });

        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> ReportableResult<()> {
        match declaration {
            Declaration::Module(..) | Declaration::Import(..) => Ok(()),
            Declaration::Variant(variant) => self.variant(variant),
            Declaration::Procedure(procedure) => self.procedure(procedure),
        }
    }

    fn variant(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { methods, path, .. } = variant;

        for method in methods {
            let MethodDeclaration { name, body, .. } = method;

            let ProcedureType { arguments, return_type } = self.variants[path].methods[name.data()].clone();

            if !body.iter().all(|statement| statement.data().returns()) {
                return self.error(
                    TypeCheckError::MethodDoesNotReturn {
                        type_path: path.clone(),
                        method: *method.name.data(),
                        expceted: *return_type,
                    },
                    method.name.location(),
                );
            }

            let variant_type = self.variants[path].ty.clone();
            let variant_type = match variant_type {
                Type::Variant(..) => variant_type,
                Type::Forall(vars, _) => {
                    let type_vars = vars.iter().map(|var| Type::TypeVar(*var));
                    Type::Variant(path.clone(), type_vars.collect())
                },
                _ => unreachable!(),
            };

            scoped!(self, {
                self.locals.push(variant_type);
                self.locals.extend(arguments);
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

        let Type::Procedure(procedure) = self.names[path].clone() else {
            unreachable!();
        };
        let ProcedureType { arguments, return_type } = procedure;

        // TODO: When the body is empty .all return true
        if !body.iter().all(|statement| statement.data().returns()) {
            return self.error(
                TypeCheckError::ProcedureDoesNotReturn {
                    procedure: path.clone(),
                    expceted: *return_type,
                },
                name.location(),
            );
        }

        scoped!(self, {
            self.locals.extend(arguments);
            self.return_type = Some(*return_type.clone());
            for statement in body {
                self.statement(statement)?;
            }
            self.return_type = None;
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

        let Some(ty) = self.return_type.clone() else {
            unreachable!();
        };
        self.check(expression, ty)
    }

    fn matc(&mut self, matc: &MatchStatement) -> ReportableResult<()> {
        // TODO: Exhaustiveness check
        let MatchStatement { expression, branches } = matc;

        let ty = self.infer(expression)?;
        for branch in branches {
            scoped!(self, {
                if !self.type_pattern_match(ty.clone(), branch.data().pattern())? {
                    // TODO: Remove push locals by type_pattern_match()
                    return self.error(
                        TypeCheckError::NotAPatternOfType { expected: ty },
                        branch.data().pattern().location(),
                    );
                }

                self.statement(branch.data().statement())?;
            })
        }

        Ok(())
    }

    fn type_pattern_match(&mut self, ty: Type, pattern: &Located<Pattern>) -> ReportableResult<bool> {
        match (ty, pattern.data()) {
            (Type::Variant(path, arguments), Pattern::VariantCase(variant_case)) => {
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
                    let type_var_map = type_vars.to_owned().into_iter().zip(arguments).collect::<HashMap<_, _>>();
                    for ty in case_fields {
                        let ty = Self::replace_type_vars(ty.clone(), &type_var_map);
                        self.locals.push(ty.clone());
                    }
                } else {
                    for ty in case_fields {
                        self.locals.push(ty.clone());
                    }
                }

                Ok(true)
            }
            (Type::Procedure { .. }, _) => Ok(false),
            (Type::Forall { .. }, _) => Ok(false),
            (Type::TypeVar { .. }, _) => Ok(false),
        }
    }

    fn contains_type_var(t: &Type) -> bool {
        match t {
            Type::Variant(_, arguments) => {
                arguments.iter().any(Self::contains_type_var)
            },
            Type::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                arguments.iter().any(Self::contains_type_var) ||
                Self::contains_type_var(return_type)
            },
            Type::TypeVar(_) => true,

            Type::Forall(..) => unreachable!(),
        }
    }

    fn check(&mut self, expression: &Located<Expression>, expected: Type) -> ReportableResult<()> {
        let encountered = match expression.data() {
            Expression::Path(..) |
            Expression::Application(..) |
            Expression::Projection(..) => self.infer(expression)?
        };

        // TODO: use generalization and insantce methods as in algorithm J
        //   what this code essentially is `instance`
        let encountered = if let Type::Forall(_, t) = encountered {
            *t
        } else {
            encountered
        };

        // Not sure about this but seems a bit legit
        //   because if contains a type var, they unify and we dont check equality
        let (encountered, expected) = if !Self::contains_type_var(&expected) {
            let mut map = HashMap::new();
            if !Self::unify(encountered.clone(), expected.clone(), &mut map) {
                return self.error(
                    TypeCheckError::MismatchedTypes {
                        encountered,
                        expected,
                    },
                    expression.location(),
                );
            };

            let encountered = Self::replace_type_vars(encountered, &map);
            let expected = Self::replace_type_vars(expected, &map);

            (encountered, expected)
        } else {
            (encountered, expected)
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
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
        }
    }

    fn path(&mut self, path: &PathExpression) -> ReportableResult<Type> {
        let PathExpression { bound, .. } = path;

        match bound {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                let t = self.locals[index].clone();
                let t = if let Type::Forall(_, t) = t {
                    let mut map = HashMap::new();
                    let t = self.rinst(*t, &mut map);
                    let vars = map.into_values().map(|t| {
                        let Type::TypeVar(id) = t else { unreachable!() };
                        id
                    }).collect();
                    Type::Forall(vars, Box::new(t))
                } else {
                    t
                };

                Ok(t)
            }
            Bound::Absolute(path) => {
                let t = self.names[path].clone();
                let t = if let Type::Forall(_, t) = t {
                    let mut map = HashMap::new();
                    let t = self.rinst(*t, &mut map);
                    let vars = map.into_values().map(|t| {
                        let Type::TypeVar(id) = t else { unreachable!() };
                        id
                    }).collect();
                    Type::Forall(vars, Box::new(t))
                } else {
                    t
                };

                Ok(t)
            },
            Bound::Undetermined => unreachable!(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression) -> ReportableResult<Type> {
        let ApplicationExpression { function, arguments } = application;

        let ty = self.infer(function)?;
        match ty {
            Type::Procedure(procedure) => {
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

                for (argument, ty) in arguments.iter().zip(arguments_type) {
                    self.check(argument, ty)?;
                }

                Ok(*return_type.clone())
            },
            Type::Forall(_, ref t) => {
                let Type::Procedure(procedure) = t.as_ref() else {
                    return self.error(
                        TypeCheckError::ExpectedAProcedure { encountered: ty },
                        function.location()
                    );
                };

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

                let mut value_types = vec![];
                for argument in arguments {
                    let a = self.infer(argument)?;
                    if let Type::Forall(_, t) = a {
                        value_types.push(*t);
                    } else {
                        value_types.push(a);
                    };
                }

                let mut map = HashMap::new();
                for (argument, ty) in value_types.iter().zip(arguments_type) {
                    if !Self::unify(argument.clone(), ty.clone(), &mut map) {
                        todo!()
                    };
                }

                let idx = self.newvar_idx();
                if !Self::unify(*return_type.clone(), Type::TypeVar(idx), &mut map) {
                    todo!()
                }

                let mut new_argument_types = vec![];
                for argument in value_types {
                    new_argument_types.push(Self::replace_type_vars(argument, &map));
                }

                let new_return = Self::replace_type_vars(*return_type.clone(), &map);

                Ok(new_return)
            }
            _ => {
                return self.error(
                    TypeCheckError::ExpectedAProcedure { encountered: ty },
                    function.location()
                );
            }
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> ReportableResult<Type> {
        let ProjectionExpression { expression, name } = projection;

        let ty = self.infer(expression)?;

        let Type::Variant(path, arguments) = &ty else {
            return self.error(
                TypeCheckError::HasNoMethod {
                    ty,
                    name: *name.data()
                },
                name.location()
            );
        };

        let Some(method_ty) = self.variants[path].methods.get(name.data()) else {
            return self.error(
                TypeCheckError::HasNoMethod {
                    ty,
                    name: *name.data()
                },
                name.location()
            );
        };

        if let Type::Forall(type_vars, _) = &self.variants[&path].ty {
            let type_var_map = type_vars.to_owned().into_iter().zip(arguments.clone()).collect::<HashMap<_, _>>();
            Ok(Self::replace_type_vars(Type::Procedure(method_ty.clone()), &type_var_map))
        } else {
            Ok(Type::Procedure(method_ty.clone()))
        }
    }

    fn unify(a: Type, b: Type, map: &mut HashMap<usize, Type>) -> bool {
        match (a, b) {
            (Type::Variant(p1, args1), Type::Variant(p2, args2)) => {
                if p1 != p2 {
                    return false;
                }

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !Self::unify(arg1, arg2, map) {
                        return false;
                    }
                }

                true
            },
            (Type::Procedure(p1), Type::Procedure(p2)) => {
                let ProcedureType { arguments: args1, return_type: r1 } = p1;
                let ProcedureType { arguments: args2, return_type: r2 } = p2;

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !Self::unify(arg1, arg2, map) {
                        return false;
                    }
                }

                Self::unify(*r1, *r2, map)
            },

            (Type::TypeVar(idx1), Type::TypeVar(idx2)) => {
                if map.contains_key(&idx1) {
                    if let Some(v) = map.insert(idx2, map[&idx1].clone()) {
                        if v != map[&idx1].clone() {
                            return false;
                        }
                    }
                } else {
                    map.insert(idx1, Type::TypeVar(idx2));
                }

                true
            },

            (a, Type::Forall(_, b)) => Self::unify(a, *b, map),
            (Type::Forall(_, a), b) => Self::unify(*a, b, map),

            (t, Type::TypeVar(idx)) |
            (Type::TypeVar(idx), t) => {
                if !Self::occurs(idx, &t) {
                    if let Some(v) = map.insert(idx, t.clone()) {
                        if v != t {
                            return false;
                        }
                    }

                    true
                } else {
                    false
                }
            },
            _ => false
        }
    }

    fn occurs(var: usize, t: &Type) -> bool {
        match t {
            Type::Variant(_, arguments) => {
                arguments.iter().any(|t| Self::occurs(var, t))
            },
            Type::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                arguments.iter().any(|t| Self::occurs(var, t)) ||
                Self::occurs(var, return_type)

            },
            Type::TypeVar(idx) => idx == &var,

            Type::Forall(..) => unreachable!(),
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
        }
    }
}
