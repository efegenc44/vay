use std::collections::HashMap;

use crate::{
    bound::{Bound, Path},
    declaration::{Declaration, InterfaceDeclaration, MethodDeclaration, MethodSignature, Module, ProcedureDeclaration, VariantDeclaration},
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

pub struct Checker {
    names: HashMap<Path, Type>,
    variants: HashMap<Path, VariantInformation>,
    interfaces: HashMap<Path, (Interface, usize)>,

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
            type_var_counter: 0,
            current_source: String::new(),
            lastest_unification: HashMap::new(),
        }
    }

    fn newvar(&mut self) -> TypeVar {
        let idx = self.type_var_counter;
        self.type_var_counter += 1;
        TypeVar { idx, instance: 0, methods: HashMap::new() }
    }

    // Basically like immediate instance after generalization
    fn rinst(&mut self, t: MonoType, map: &mut HashMap<usize, MonoType>) -> MonoType {
        match t {
            MonoType::Variant(path, arguments) => {
                let mut rarguments = vec![];
                for argument in arguments {
                    rarguments.push(self.rinst(argument, map));
                }

                MonoType::Variant(path, rarguments)
            },
            MonoType::Procedure(procedure) => {
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
                MonoType::Procedure(procedure)
            },
            MonoType::Var(type_var) => {
                let TypeVar { idx, instance, methods } = type_var;

                if map.contains_key(&idx) {
                    map[&idx].clone()
                } else {
                    let mut t = self.newvar();
                    t.instance = instance;
                    t.methods = methods;
                    let t = MonoType::Var(t);
                    map.insert(idx, t.clone());
                    t
                }
            },
            MonoType::Constant(idx) => MonoType::Constant(idx),
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
            let Type::Mono(t) = self.eval_type_expression(argument)? else {
                todo!("Expected mono type");
            };
            argument_types.push(t);
        }

        let Type::Mono(return_type) = self.eval_type_expression(return_type)? else {
            todo!("Expected mono type");
        };
        let return_type = Box::new(return_type);

        let procedure_type = ProcedureType { arguments: argument_types, return_type };
        Ok(Type::Mono(MonoType::Procedure(procedure_type)))
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
            MonoType::Variant(_, ref mut variant_arguments) => {
                variant_arguments.clear();
                for (argument, var) in arguments.iter().zip(vars) {
                    let Type::Mono(t) = self.eval_type_expression(argument)? else {
                        todo!("Expected mono type");
                    };
                    if !self.is_supertype_of_interface(t.clone(), &var.methods, var.instance) {
                        todo!("Constraint error");
                    }
                    variant_arguments.push(t);
                }
            },
            MonoType::Procedure(_procedure_type) => todo!(),

            MonoType::Constant(_) |
            MonoType::Var(..) => unreachable!(),
        };

        Ok(Type::Mono(*ty))
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
                Declaration::Variant(..) => (),
                Declaration::Interface(interface) => self.collect_interface_type(interface)?,
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Module(..) => (),
                Declaration::Import(..) => (),
                Declaration::Procedure(..) => (),
                Declaration::Interface(..) => (),
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
                Declaration::Interface(interface) => self.interface(interface)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn collect_procedure_name(&mut self, procedure: &ProcedureDeclaration) -> ReportableResult<()> {
        let ProcedureDeclaration { type_vars, arguments, return_type, path, .. } = procedure;

        let mut vars = vec![];
        for var in type_vars {
            let mut first = true;
            let mut newvar = self.newvar();
            for interface in &var.data().interfaces {
                let (i, ins) = &self.interfaces[&interface.1];
                if first {
                    newvar.instance = *ins;
                    newvar.methods.extend(i.methods.clone());
                    first = false;
                } else {
                    let mut map = HashMap::new();
                    map.insert(*ins, MonoType::Constant(TypeVar {
                        idx: newvar.instance,
                        instance: newvar.instance,
                        methods: HashMap::new()
                    }));

                    for (name, method) in &i.methods {
                        let MonoType::Procedure(procedure) =
                            MonoType::Procedure(method.clone()).replace_type_constants(&map) else {
                            unreachable!()
                        };

                        newvar.methods.insert(*name, procedure);
                    }
                }
            }

            vars.push(newvar.clone());
        }

        scoped!(self, {
            self.locals.extend(vars.iter().map(|var| Type::Mono(MonoType::Var(var.clone()))));

            let mut argument_types = vec![];
            for argument in arguments {
                let Type::Mono(t) = self.eval_type_expression(argument.data().type_expression())? else {
                    todo!("Expected mono type");
                };
                argument_types.push(t);
            }

            let Type::Mono(t) = self.eval_type_expression(return_type)? else {
                todo!("Expected mono type");
            };
            let return_type = Box::new(t);

            let procedure_type = ProcedureType { arguments: argument_types, return_type };
            let procedure = MonoType::Procedure(procedure_type);
            let t = if type_vars.is_empty() {
                Type::Mono(procedure)
            } else {
                // rinst is called when the name is retrieved so no need to
                //   rinst here like in algorithm J, I think
                Type::Forall(vars, Box::new(procedure))
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
            let ty = Box::new(MonoType::Variant(path.clone(), vec![]));
            let mut vars = vec![];
            for _ in type_vars {
                vars.push(self.newvar());
            }

            Type::Forall(vars, ty)
        };
        let variant_data = VariantInformation::with_type(variant_type);
        self.variants.insert(path.clone(), variant_data);

        Ok(())
    }

    fn collect_interface_type(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { path, .. } = interface;

        let t = self.newvar();
        self.interfaces.insert(path.clone(), (Interface { methods: HashMap::new() }, t.idx));

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { type_vars, cases, methods, path, .. } = variant;

        if let Type::Forall(vars, _) = &mut self.variants.get_mut(path).unwrap().ty {
            for (i, var) in type_vars.iter().enumerate() {
                let mut first = true;
                let newvar = &mut vars[i];
                for interface in &var.data().interfaces {
                    let (i, ins) = &self.interfaces[&interface.1];
                    if first {
                        newvar.instance = *ins;
                        newvar.methods.extend(i.methods.clone());
                        first = false;
                    } else {
                        let mut map = HashMap::new();
                        map.insert(*ins, MonoType::Constant(TypeVar {
                            idx: newvar.instance,
                            instance: newvar.instance,
                            methods: HashMap::new()
                        }));

                        for (name, method) in &i.methods {
                            let MonoType::Procedure(procedure) =
                            MonoType::Procedure(method.clone()).replace_type_constants(&map) else {
                                unreachable!()
                            };

                            newvar.methods.insert(*name, procedure);
                        }
                    }
                }
            }
        };

        scoped!(self, {
            if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                self.locals.extend(vars.iter().map(|var| Type::Mono(MonoType::Constant(var.clone()))));
            }

            for method in methods {
                let MethodDeclaration { name, arguments, return_type, .. } = method;

                let mut argument_types = vec![];
                for argument in arguments {
                    let Type::Mono(t) = self.eval_type_expression(argument.data().type_expression())? else {
                        todo!("Expected mono type");
                    };
                    argument_types.push(t);
                }

                let Type::Mono(t) = self.eval_type_expression(return_type)? else {
                    todo!("Expected mono type");
                };
                let return_type = Box::new(t);
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

            let poly;
            let variant_type = self.variants[path].ty.clone();
            let variant_type = match variant_type {
                Type::Mono(t) => {
                    poly = false;
                    t
                },
                Type::Forall(vars, _) => {
                    poly = true;
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
                        let Type::Mono(t) = self.eval_type_expression(argument)? else {
                            todo!("Expected mono type");
                        };
                        argument_types.push(t);
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
                        let procedure = self.rinst(MonoType::Procedure(procedure_type), &mut map);
                        let vars = map.into_values().map(|t| {
                            let MonoType::Var(type_var) = t else { unreachable!() };
                            type_var
                        }).collect();
                        Type::Forall(vars, Box::new(procedure))
                    } else {
                        Type::Mono(MonoType::Procedure(procedure_type))
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
                            let MonoType::Var(type_var) = t else { unreachable!() };
                            type_var
                        }).collect();
                        Type::Forall(vars, Box::new(t))
                    } else {
                        Type::Mono(variant_type.clone())
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
            Declaration::Interface(interface) => self.interface(interface),
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
                let MonoType::Procedure(procedure) = t.as_ref() else {
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

    fn interface(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { methods, path, .. } = interface;

        scoped!(self, {
            let t = self.interfaces.get_mut(path).unwrap().1;
            self.locals.push(Type::Mono(MonoType::Constant(TypeVar { idx: t, instance: t, methods: HashMap::new() })));

            for method in methods {
                let MethodSignature { name, arguments, return_type } = method;

                let mut new_arguments = vec![];
                for argument in arguments {
                    let Type::Mono(t) = self.eval_type_expression(argument.data().type_expression())? else {
                        todo!("Expected mono type");
                    };
                    new_arguments.push(t)
                }

                let Type::Mono(t) = self.eval_type_expression(return_type)? else {
                    todo!("Expected mono type");
                };
                let new_return_type = Box::new(t);

                let method_ty = ProcedureType {
                    arguments: new_arguments,
                    return_type: new_return_type
                };
                self.interfaces.get_mut(path).unwrap().0.methods.insert(*name.data(), method_ty);
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

    fn is_supertype_of_interface(&self, t: MonoType, methods: &HashMap<InternIdx, ProcedureType>, instance: usize) -> bool {
        match &t {
            MonoType::Variant(path, arguments) => {
                for method in methods {
                    let (name, procedure) = method;
                    let mut type_var_map = HashMap::new();
                    type_var_map.insert(instance, t.clone());

                    let MonoType::Procedure(procedure) =
                    MonoType::Procedure(procedure.clone()).replace_type_constants(&type_var_map) else {
                        unreachable!()
                    };

                    let Some(method_ty) = self.variants[&path].methods.get(name) else {
                        return false;

                    };

                    let method_ty = if let Type::Forall(type_vars, _) = &self.variants[&path].ty {
                        let type_var_map = type_vars
                            .to_owned()
                            .into_iter()
                            .map(|var| var.idx)
                            .zip(arguments.clone())
                            .collect::<HashMap<_, _>>();

                        let MonoType::Procedure(method_ty) =
                            MonoType::Procedure(method_ty.clone()).replace_type_constants(&type_var_map) else {
                            unreachable!()
                        };
                        method_ty
                    } else {
                        method_ty.clone()
                    };

                    if method_ty != procedure {
                        return false;
                    }
                }

                true
            },
            MonoType::Var(type_var) | MonoType::Constant(type_var) => {
                for method in methods {
                    let (name, procedure) = method;
                    let mut type_var_map = HashMap::new();
                    type_var_map.insert(instance, t.clone());

                    let MonoType::Procedure(procedure) =
                    MonoType::Procedure(procedure.clone()).replace_type_constants(&type_var_map) else {
                        unreachable!()
                    };

                    let Some(method_ty) = type_var.methods.get(name) else {
                        return false;
                    };

                    let mut type_var_map = HashMap::new();
                    type_var_map.insert(type_var.instance, t.clone());

                    let MonoType::Procedure(method_ty) =
                        MonoType::Procedure(method_ty.clone()).replace_type_constants(&type_var_map) else {
                        unreachable!()
                    };


                    if method_ty != procedure {
                        return false;
                    }
                }

                true
            }
            _ => methods.is_empty()
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
                let t = match t {
                    Type::Forall(_, t) => {
                        let mut map = HashMap::new();
                        self.rinst(*t, &mut map)
                    }
                    Type::Mono(m) => m,
                };

                Ok(t)
            }
            Bound::Absolute(path) => {
                let t = self.names[path].clone();
                let t = match t {
                    Type::Forall(_, t) => {
                        let mut map = HashMap::new();
                        self.rinst(*t, &mut map)
                    }
                    Type::Mono(m) => m,
                };

                Ok(t)
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
                    value_types.push(self.infer(argument)?);
                }

                let mut map = HashMap::new();
                for (argument, ty) in value_types.iter().zip(arguments_type) {
                    if !self.unify(argument.clone(), ty.clone(), &mut map) {
                        todo!()
                    };
                }

                let idx = self.newvar();
                if !self.unify(*return_type.clone(), MonoType::Var(idx), &mut map) {
                    todo!()
                }

                let mut new_argument_types = vec![];
                for argument in value_types {
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
                let TypeVar { idx: _, instance, methods } = type_var;

                let Some(method_ty) = methods.get(name.data()) else {
                    return self.error(
                        TypeCheckError::HasNoMethod {
                            ty: Type::Mono(t),
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                let mut map = HashMap::new();
                map.insert(*instance, t.clone());

                return Ok(MonoType::Procedure(method_ty.clone()).replace_type_constants(&map));
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
                .to_owned()
                .into_iter()
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
                let TypeVar { idx: idx1, methods: methods1, instance: instance1 } = var1.clone();
                let TypeVar { idx: idx2, methods: methods2, instance: instance2 } = var2;

                let mut methods = methods1.clone();
                if map.contains_key(&idx1) {
                    if let Some(mut v) = map.insert(idx2, map[&idx1].clone()) {
                        while let MonoType::Var(var) = v {
                            let mut map = HashMap::new();
                            map.insert(var.instance, MonoType::Constant(var1.clone()));

                            for (name, method) in var.methods {
                                let MonoType::Procedure(procedure) =
                                MonoType::Procedure(method).replace_type_constants(&map) else {
                                    unreachable!()
                                };

                                // TODO: check colisions here
                                methods.insert(name, procedure);
                            }

                            v = map[&var.idx].clone()
                        }

                        if v != map[&idx1].clone() {
                            return false;
                        }

                        map.insert(idx1, MonoType::Var(TypeVar { idx: idx2, instance: instance1, methods }));
                    }
                } else {
                    let mut map = HashMap::new();
                    map.insert(instance2, MonoType::Constant(var1));

                    for (name, method) in methods2 {
                        let MonoType::Procedure(procedure) =
                            MonoType::Procedure(method).replace_type_constants(&map) else {
                            unreachable!()
                        };

                        // TODO: check colisions here
                        methods.insert(name, procedure);
                    }

                    map.insert(idx1, MonoType::Var(TypeVar { idx: idx2, instance: instance1, methods }));
                }

                true
            },

            (t, MonoType::Var(var)) | (MonoType::Var(var), t) => {
                let TypeVar { idx, mut methods, instance } = var.clone();

                if !t.occurs(idx) {
                    if let Some(mut v) = map.insert(idx, t.clone()) {
                        while let MonoType::Var(var) = v {
                            let mut map = HashMap::new();
                            map.insert(var.instance, MonoType::Constant(var.clone()));

                            for (name, method) in var.methods {
                                let MonoType::Procedure(procedure) =
                                MonoType::Procedure(method).replace_type_constants(&map) else {
                                    unreachable!()
                                };

                                // TODO: check confilicts here
                                methods.insert(name, procedure);
                            }

                            v = map[&var.idx].clone()
                        }

                        if v != t {
                            return false;
                        }
                    }

                    if !self.is_supertype_of_interface(t.clone(), &methods, instance) {
                        todo!()
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
