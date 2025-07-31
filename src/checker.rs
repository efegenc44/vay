use std::collections::{HashMap, HashSet};

use crate::{
    bound::{Bound, Path},
    declaration::{self, Declaration, FunctionDeclaration, InterfaceDeclaration, MethodDeclaration, MethodSignature, Module, StructDeclaration, VariantDeclaration},
    expression::{
        ApplicationExpression, AssignmentExpression, Expression, FunctionTypeExpression, LambdaExpression, LetExpression, MatchExpression, PathExpression, PathTypeExpression, Pattern, ProjectionExpression, ReturnExpression, SequenceExpression, TypeApplicationExpression, TypeExpression, VariantCasePattern
    },
    interner::{InternIdx, Interner},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    typ::{FunctionType, Interface, MonoType, Type, TypeVar},
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
    methods: HashMap<InternIdx, (FunctionType, HashMap<usize, HashSet<Path>>)>,
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

struct StructInformation {
    ty: Type,
    fields: HashMap<InternIdx, MonoType>,
    methods: HashMap<InternIdx, (FunctionType, HashMap<usize, HashSet<Path>>)>,
}

impl StructInformation {
    fn with_type(ty: Type) -> Self {
        Self {
            ty,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }
}

const INTERFACE_CONSTANT_IDX: usize = 0;

pub struct Checker {
    names: HashMap<Path, Type>,
    variants: HashMap<Path, VariantInformation>,
    structs: HashMap<Path, StructInformation>,
    interfaces: HashMap<Path, Interface>,

    // TODO: Seperate type locals and value locals
    locals: Vec<Type>,
    return_type: Vec<MonoType>,

    type_var_counter: usize,

    current_source: String,

    unification_table: HashMap<usize, MonoType>
}

impl Checker {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            variants: HashMap::new(),
            structs: HashMap::new(),
            interfaces: HashMap::new(),
            locals: vec![],
            return_type: vec![],
            // NOTE: 0 is reserved for interfaces' self reference type constant
            type_var_counter: 1,
            current_source: String::new(),
            unification_table: HashMap::new(),
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
                    .map(|var| {
                        let mut newvar = self.newvar();
                        newvar.interfaces = var.interfaces.clone();
                        (var.idx, MonoType::Var(newvar))
                    })
                    .collect();

                m.substitute(&map)
            },
        }
    }

    fn generalize(&mut self, m: MonoType) -> Type {
        if matches!(m, MonoType::Function(_)) {
            Type::Forall(m.occuring_type_vars(), m)
        } else {
            Type::Mono(m)
        }
    }

    fn type_vars(&mut self, type_vars: &[Located<declaration::TypeVar>]) -> Vec<TypeVar> {
        let mut vars = vec![];
        for var in type_vars {
            let mut newvar = self.newvar();
            let interfaces = var
                .data()
                .interfaces
                .iter().map(|interface| interface.1.clone());

            newvar.interfaces.extend(interfaces);
            vars.push(newvar);
        }

        vars
    }

    fn eval_type_expression(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<Type> {
        match type_expression.data() {
            TypeExpression::Path(type_path) => self.eval_path_type(type_path),
            TypeExpression::Function(function) => self.eval_function_type(function),
            // TODO: Inconsistent naming?
            TypeExpression::Application(type_application) => self.eval_type_application(type_application),
            TypeExpression::Unit => Ok(Type::Mono(MonoType::Unit)),
        }
    }

    fn eval_path_type(&mut self, type_path: &PathTypeExpression) -> ReportableResult<Type> {
        let PathTypeExpression { bound, .. } = type_path;

        match bound {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            }
            Bound::Absolute(path) => {
                if let Some(information) = self.variants.get(path) {
                    Ok(information.ty.clone())
                } else {
                    Ok(self.structs[path].ty.clone())
                }
            },
            Bound::Undetermined => unreachable!(),
        }
    }

    fn eval_function_type(&mut self, function_type: &FunctionTypeExpression) -> ReportableResult<Type> {
        let FunctionTypeExpression { arguments, return_type } = function_type;

        let arguments = arguments
            .iter().map(|argument| self.eval_to_mono(argument))
            .collect::<ReportableResult<Vec<_>>>()?;

        let return_type = if let Some(return_type) = return_type {
            Box::new(self.eval_to_mono(return_type)?)
        } else {
            Box::new(MonoType::Unit)
        };

        Ok(Type::Mono(MonoType::Function(FunctionType { arguments, return_type })))
    }

    fn eval_type_application(&mut self, type_application: &TypeApplicationExpression) -> ReportableResult<Type> {
        let TypeApplicationExpression { function, arguments } = &type_application;

        let t = self.eval_type_expression(function)?;
        let Type::Forall(variables, mut m) = t else {
            return self.error(
                TypeCheckError::NotAPolyType {
                    encountered: t
                },
                function.location()
            )
        };

        if variables.len() != arguments.len() {
            return self.error(
                TypeCheckError::TypeArityMismatch {
                    expected: variables.len(),
                    encountered: arguments.len()
                },
                function.location()
            )
        }

        match m {
            MonoType::Variant(_, ref mut variant_arguments) => {
                for (argument, variable) in arguments.iter().zip(variables) {
                    assert!(variable.interfaces.is_empty());
                    variant_arguments.push(self.eval_to_mono(argument)?);
                }
            },
            MonoType::Struct(_, ref mut variant_arguments) => {
                for (argument, variable) in arguments.iter().zip(variables) {
                    assert!(variable.interfaces.is_empty());
                    variant_arguments.push(self.eval_to_mono(argument)?);
                }
            },
            MonoType::Function(_) => todo!(),

            MonoType::Constant(_) |
            MonoType::Var(..) |
            MonoType::Unit |
            MonoType::Bottom => unreachable!(),
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
                Declaration::Struct(strct) => self.collect_struct_type(strct)?,
                _ => ()
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, module: &Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            if let Declaration::Interface(interface) = declaration {
                self.collect_interface_name(interface)?
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Variant(variant) => self.collect_variant_name(variant)?,
                Declaration::Struct(strct) => self.collect_struct_name(strct)?,
                _ => (),
            }
        }

        for declaration in module.declarations() {
            if let Declaration::Function(function) = declaration {
                self.collect_function_name(function)?
            }
        }

        Ok(())
    }

    fn collect_function_name(&mut self, function: &FunctionDeclaration) -> ReportableResult<()> {
        let FunctionDeclaration { type_vars, arguments, return_type, path, .. } = function;

        scoped!(self, {
            let type_vars = self.type_vars(type_vars);
            self.locals.extend(type_vars
                .iter().cloned()
                .map(MonoType::Var)
                .map(Type::Mono)
            );

            let arguments = arguments
                .iter().map(|argument| self.eval_to_mono(argument.data().type_expression()))
                .collect::<ReportableResult<Vec<_>>>()?;

            let return_type = if let Some(return_type) = return_type {
                Box::new(self.eval_to_mono(return_type)?)
            } else {
                Box::new(MonoType::Unit)
            };

            let function = MonoType::Function(FunctionType { arguments, return_type });

            let t = if type_vars.is_empty() {
                Type::Mono(function)
            } else {
                Type::Forall(type_vars, function)
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
            let variables = type_vars.iter().map(|_| self.newvar()).collect();
            Type::Forall(variables, MonoType::Variant(path.clone(), vec![]))
        };
        let variant_information = VariantInformation::with_type(variant_type);
        self.variants.insert(path.clone(), variant_information);

        Ok(())
    }

    fn collect_struct_type(&mut self, strct: &StructDeclaration) -> ReportableResult<()> {
        let StructDeclaration { type_vars, path, .. } = strct;

        let struct_type = if type_vars.is_empty() {
            Type::Mono(MonoType::Struct(path.clone(), vec![]))
        } else {
            let variables = type_vars.iter().map(|_| self.newvar()).collect();
            Type::Forall(variables, MonoType::Struct(path.clone(), vec![]))
        };
        let struct_information = StructInformation::with_type(struct_type);
        self.structs.insert(path.clone(), struct_information);

        Ok(())
    }

    fn collect_interface_type(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { path, .. } = interface;

        self.interfaces.insert(path.clone(), Interface { methods: HashMap::new() });

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        for method in methods {
            let MethodDeclaration { constraints, name, arguments, return_type, .. } = method;

            let constraints = constraints
                .iter().map(|constraint| (
                    constraint.nth,
                    constraint
                        .type_var.data()
                        .interfaces.iter().map(|interface| interface.1.clone())
                        .collect::<HashSet<_>>()
                ))
                .collect::<HashMap<_, _>>();

            scoped!(self, {
                if let Type::Forall(variables, _) = self.variants[path].ty.clone() {
                    self.locals.extend(variables.iter().enumerate().map(|(idx, variable)| {
                        let interfaces = constraints
                            .get(&idx)
                            .cloned()
                            .unwrap_or(HashSet::new());

                        let mut variable = variable.clone();
                        variable.interfaces.extend(interfaces);
                        Type::Mono(MonoType::Constant(variable))
                    }));
                }

                let arguments = arguments
                    .iter().map(|argument| self.eval_to_mono(argument.data().type_expression()))
                    .collect::<ReportableResult<Vec<_>>>()?;

                let return_type = if let Some(return_type) = return_type {
                    Box::new(self.eval_to_mono(return_type)?)
                } else {
                    Box::new(MonoType::Unit)
                };

                let function_type = FunctionType { arguments, return_type };
                if self.variants
                    .get_mut(path)
                    .unwrap()
                    .methods
                    .insert(*name.data(), (function_type, constraints))
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
            });
        }

        scoped!(self, {
            if let Type::Forall(vars, _) = self.variants[path].ty.clone() {
                self.locals.extend(vars
                    .iter().cloned()
                    .map(MonoType::Var)
                    .map(Type::Mono)
                );
            };

            // NOTE: Not using instantiate() here because variables of the variant
            //   must remain unchanged for substitution
            let variant_type = match self.variants[path].ty.clone() {
                Type::Mono(m) => m,
                Type::Forall(variables, _) => {
                    let arguments = variables
                        .iter().cloned()
                        .map(MonoType::Var)
                        .collect();

                    MonoType::Variant(path.clone(), arguments)
                },
            };

            for case in cases {
                let case_name = *case.data().identifier().data();
                let case_path = case.data().path().clone();

                if let Some(arguments) = case.data().arguments() {
                    let arguments = arguments
                        .iter().map(|argument| self.eval_to_mono(argument))
                        .collect::<ReportableResult<Vec<_>>>()?;

                    let return_type = Box::new(variant_type.clone());

                    self.variants
                        .get_mut(path).unwrap()
                        .cases.insert(case_name, arguments.clone());

                    let function_type = FunctionType { arguments, return_type };

                    // Generalization
                    let t = if let Type::Forall(variables, _) = self.variants[path].ty.clone() {
                        Type::Forall(variables, MonoType::Function(function_type))
                    } else {
                        Type::Mono(MonoType::Function(function_type))
                    };

                    self.names.insert(case_path, t);
                } else {
                    self.variants
                        .get_mut(path).unwrap()
                        .cases.insert(case_name, vec![]);

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

    fn collect_struct_name(&mut self, strct: &StructDeclaration) -> ReportableResult<()> {
        let StructDeclaration { fields, methods, path, .. } = strct;

        for method in methods {
            let MethodDeclaration { constraints, name, arguments, return_type, .. } = method;

            let constraints = constraints
                .iter().map(|constraint| (
                    constraint.nth,
                    constraint
                        .type_var.data()
                        .interfaces.iter().map(|interface| interface.1.clone())
                        .collect::<HashSet<_>>()
                ))
                .collect::<HashMap<_, _>>();

            scoped!(self, {
                if let Type::Forall(variables, _) = self.structs[path].ty.clone() {
                    self.locals.extend(variables.iter().enumerate().map(|(idx, variable)| {
                        let interfaces = constraints
                            .get(&idx)
                            .cloned()
                            .unwrap_or(HashSet::new());

                        let mut variable = variable.clone();
                        variable.interfaces.extend(interfaces);
                        Type::Mono(MonoType::Constant(variable))
                    }));
                }

                let arguments = arguments
                    .iter().map(|argument| self.eval_to_mono(argument.data().type_expression()))
                    .collect::<ReportableResult<Vec<_>>>()?;

                let return_type = if let Some(return_type) = return_type {
                    Box::new(self.eval_to_mono(return_type)?)
                } else {
                    Box::new(MonoType::Unit)
                };

                let function_type = FunctionType { arguments, return_type };
                if self.structs
                    .get_mut(path)
                    .unwrap()
                    .methods
                    .insert(*name.data(), (function_type, constraints))
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
            });
        }

        scoped!(self, {
            if let Type::Forall(vars, _) = self.structs[path].ty.clone() {
                self.locals.extend(vars
                    .iter().cloned()
                    .map(MonoType::Var)
                    .map(Type::Mono)
                );
            };

            // NOTE: Not using instantiate() here because variables of the struct
            //   must remain unchanged for substitution
            let struct_type = match self.structs[path].ty.clone() {
                Type::Mono(m) => m,
                Type::Forall(variables, _) => {
                    let arguments = variables
                        .iter().cloned()
                        .map(MonoType::Var)
                        .collect();

                    MonoType::Struct(path.clone(), arguments)
                },
            };

            let arguments = fields
                .iter().map(|field| self.eval_to_mono(field.data().type_expression()))
                .collect::<ReportableResult<Vec<_>>>()?;

            let fields = fields
                .iter().zip(arguments.clone())
                .map(|(field, argument)| (*field.data().indentifier().data(), argument))
                .collect::<HashMap<_, _>>();

            *&mut self.structs.get_mut(path).unwrap().fields = fields;

            let return_type = Box::new(struct_type.clone());
            let function_type = FunctionType { arguments, return_type };

            // Generalization
            let t = if let Type::Forall(variables, _) = self.structs[path].ty.clone() {
                Type::Forall(variables, MonoType::Function(function_type))
            } else {
                Type::Mono(MonoType::Function(function_type))
            };

            self.names.insert(path.clone(), t);
        });

        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> ReportableResult<()> {
        match declaration {
            Declaration::Variant(variant) => self.variant(variant),
            Declaration::Function(function) => self.function(function),
            Declaration::Struct(strct) => self.strct(strct),
            _ => Ok(())
        }
    }

    fn variant(&mut self, variant: &VariantDeclaration) -> ReportableResult<()> {
        let VariantDeclaration { methods, path, .. } = variant;

        for method in methods {
            let MethodDeclaration { name, body, .. } = method;

            let (method_type, constraints) = &self.variants[path].methods[name.data()];
            let FunctionType { arguments, return_type } = method_type.clone();

            let variant_type = match self.variants[path].ty.clone() {
                Type::Mono(m) => m,
                Type::Forall(variables, _) => {
                    let arguments = variables.iter().enumerate().map(|(idx, variable)| {
                        let interfaces = constraints
                            .get(&idx)
                            .cloned()
                            .unwrap_or(HashSet::new());

                        let mut variable = variable.clone();
                        variable.interfaces.extend(interfaces);
                        MonoType::Constant(variable)
                    })
                    .collect();

                    MonoType::Variant(path.clone(), arguments)
                },
            };

            scoped!(self, {
                self.locals.push(Type::Mono(variant_type));
                self.locals.extend(arguments.into_iter().map(Type::Mono));
                self.return_type.push(*return_type.clone());
                self.check(body, *return_type)?;
                self.return_type.pop();
            });
        }

        Ok(())
    }

    fn function(&mut self, function: &FunctionDeclaration) -> ReportableResult<()> {
        let FunctionDeclaration { body, path, .. } = function;

        let function_type = match self.names[path].clone() {
            Type::Mono(m) => m.into_function(),
            Type::Forall(variables, m) => {
                let function_type = m.into_function();
                let map = variables
                    .iter().cloned()
                    .map(|var| (var.idx, MonoType::Constant(var)))
                    .collect();

                MonoType::Function(function_type.clone())
                    .substitute(&map)
                    .into_function()
            }
        };
        let FunctionType { arguments, return_type } = function_type;

        scoped!(self, {
            self.locals.extend(arguments.into_iter().map(Type::Mono));
            self.return_type.push(*return_type.clone());
            self.check(body, *return_type)?;
            self.return_type.pop();
        });

        Ok(())
    }

    fn strct(&mut self, strct: &StructDeclaration) -> ReportableResult<()> {
        let StructDeclaration { methods, path, .. } = strct;

        for method in methods {
            let MethodDeclaration { name, body, .. } = method;

            let (method_type, constraints) = &self.structs[path].methods[name.data()];
            let FunctionType { arguments, return_type } = method_type.clone();

            let struct_type = match self.structs[path].ty.clone() {
                Type::Mono(m) => m,
                Type::Forall(variables, _) => {
                    let arguments = variables.iter().enumerate().map(|(idx, variable)| {
                        let interfaces = constraints
                            .get(&idx)
                            .cloned()
                            .unwrap_or(HashSet::new());

                        let mut variable = variable.clone();
                        variable.interfaces.extend(interfaces);
                        MonoType::Constant(variable)
                    })
                    .collect();

                    MonoType::Struct(path.clone(), arguments)
                },
            };

            scoped!(self, {
                self.locals.push(Type::Mono(struct_type));
                self.locals.extend(arguments.into_iter().map(Type::Mono));
                self.return_type.push(*return_type.clone());
                self.check(body, *return_type)?;
                self.return_type.pop();
            });
        }

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &InterfaceDeclaration) -> ReportableResult<()> {
        let InterfaceDeclaration { methods, path, .. } = interface;

        let instance_type_var = TypeVar {
            idx: INTERFACE_CONSTANT_IDX,
            interfaces: HashSet::from([path.clone()])
        };

        let vars = vec![instance_type_var.clone()];

        scoped!(self, {
            self.locals.extend(vars
                .iter().cloned()
                .map(MonoType::Constant)
                .map(Type::Mono)
            );

            for method in methods {
                let MethodSignature { name, arguments, return_type, .. } = method;

                let arguments = arguments
                    .iter().map(|argument| self.eval_to_mono(argument.data().type_expression()))
                    .collect::<ReportableResult<Vec<_>>>()?;

                let return_type = if let Some(return_type) = return_type {
                    Box::new(self.eval_to_mono(return_type)?)
                } else {
                    Box::new(MonoType::Unit)
                };

                let method_type = FunctionType { arguments, return_type };
                self.interfaces
                    .get_mut(path).unwrap()
                    .methods.insert(*name.data(), method_type);
            }
        });

        scoped!(self, {
            self.locals.extend(vars
                .iter().cloned()
                .map(MonoType::Var)
                .map(Type::Mono)
            );

            for method in methods {
                let MethodSignature { arguments, return_type, path, .. } = method;

                let mut arguments_with_instance = vec![MonoType::Var(instance_type_var.clone())];

                for argument in arguments {
                    arguments_with_instance.push(self.eval_to_mono(argument.data().type_expression())?);
                }

                let return_type = if let Some(return_type) = return_type {
                    Box::new(self.eval_to_mono(return_type)?)
                } else {
                    Box::new(MonoType::Unit)
                };

                let standalone_function = FunctionType { arguments: arguments_with_instance, return_type };
                let function_type = Type::Forall(vars.clone(), MonoType::Function(standalone_function));

                self.names.insert(path.clone(), function_type);
            }
        });


        Ok(())
    }

    fn matc(&mut self, matc: &MatchExpression) -> ReportableResult<MonoType> {
        // TODO: Exhaustiveness check
        let MatchExpression { expression, branches } = matc;

        let mut m = self.infer(expression)?;
        let mut return_type = MonoType::Var(self.newvar());

        match &branches[..] {
            [] => return_type = MonoType::Unit,
            branches => {
                for branch in branches {
                    scoped!(self, {
                        if !self.type_pattern_match(m.clone(), branch.data().pattern())? {
                            // TODO: Remove push locals by type_pattern_match()
                            return self.error(
                                TypeCheckError::NotAPatternOfType { expected: m },
                                branch.data().pattern().location(),
                            );
                        }

                        // NOTE: Bottom type is the subtype of all types
                        // TODO: Maybe self.check should return the most general type
                        if let MonoType::Bottom = return_type {
                            return_type = self.infer(branch.data().expression())?;
                        } else {
                            self.check(branch.data().expression(), return_type.clone())?;
                        }
                        m = m.substitute(&self.unification_table);
                        return_type = return_type.substitute(&self.unification_table);
                    })
                }
            }
        }

        Ok(return_type)
    }

    fn type_pattern_match(&mut self, t: MonoType, pattern: &Located<Pattern>) -> ReportableResult<bool> {
        match (t, pattern.data()) {
            (t, Pattern::Any(_)) => {
                self.locals.push(Type::Mono(t));
                Ok(true)
            },
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

                let case_fields = cases[name.data()].clone();
                let empty_fields = vec![];
                let fields = fields.as_ref().unwrap_or(&empty_fields);

                if case_fields.len() != fields.len() {
                    return self.error(
                        TypeCheckError::WrongCaseArity {
                            type_path: path,
                            case_name: *name.data(),
                            expected: case_fields.len(),
                            encountered: fields.len()
                        },
                        pattern.location(),
                    );
                }

                if let Type::Forall(variables, _) = &self.variants[&path].ty {
                    let map = variables
                        .iter().cloned()
                        .map(|variable| variable.idx)
                        .zip(arguments)
                        .collect();

                    for (t, field_pattern) in case_fields.into_iter().zip(fields) {
                        if !self.type_pattern_match(t.substitute(&map), &field_pattern)? {
                            return Ok(false);
                        };
                    }
                } else {
                    for (t, field_pattern) in case_fields.into_iter().zip(fields) {
                        if !self.type_pattern_match(t, &field_pattern)? {
                            return Ok(false);
                        }
                    }
                }

                Ok(true)
            }
            (MonoType::Unit, Pattern::Unit)  => Ok(true),
            _ => Ok(false)
        }
    }

    fn retrn(&mut self, retrn: &ReturnExpression) -> ReportableResult<MonoType> {
        let ReturnExpression { expression } = retrn;

        let Some(return_type) = self.return_type.last().cloned() else {
            todo!("Return outside of a function");
        };
        self.check(expression, return_type)?;

        Ok(MonoType::Bottom)
    }

    fn check(&mut self, expression: &Located<Expression>, expected: MonoType) -> ReportableResult<()> {
        let encountered = self.infer(expression)?;

        if !self.unify(encountered.clone(), expected.clone()) {
            return self.error(
                TypeCheckError::MismatchedTypes {
                    encountered: encountered.substitute(&self.unification_table),
                    expected: expected.substitute(&self.unification_table),
                },
                expression.location(),
            );
        };

        Ok(())
    }

    fn find_method_in_interfaces(&self, name: &InternIdx, interfaces: &HashSet<Path>) -> Option<FunctionType> {
        for path in interfaces {
            for (method_name, method_function) in &self.interfaces[path].methods {
                if name == method_name {
                    return Some(method_function.clone());
                }
            }
        }

        None
    }

    fn does_satisfy_constraint(&self, m: &MonoType, interfaces: &HashSet<Path>) -> bool {
        match m {
            MonoType::Variant(path, arguments) => {
                for interface_path in interfaces {
                    let interface = &self.interfaces[interface_path];
                    for (name, interface_function) in &interface.methods {
                        let Some((variant_function, constraints)) = self.variants[path].methods.get(name) else {
                            return false;
                        };

                        for (idx, argument) in arguments.iter().enumerate() {
                            let empty_constraint = &HashSet::new();
                            let constraint = constraints
                                .get(&idx)
                                .unwrap_or(empty_constraint);

                            if !self.does_satisfy_constraint(argument, constraint) {
                                return false;
                            }
                        }

                        let variant_function = if let Type::Forall(variables, _) = &self.variants[path].ty {
                            let map = variables
                                .iter()
                                .cloned()
                                .map(|variable| variable.idx)
                                .zip(arguments.clone())
                                .collect();

                            MonoType::Function(variant_function.clone())
                                .replace_type_constants(&map)
                                .into_function()
                        } else {
                            variant_function.clone()
                        };

                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, m.clone())]);
                        let interface_function = MonoType::Function(interface_function.clone())
                            .replace_type_constants(&map)
                            .into_function();

                        if variant_function != interface_function {
                            return false;
                        }
                    }
                }

                true
            },
            MonoType::Struct(path, arguments) => {
                for interface_path in interfaces {
                    let interface = &self.interfaces[interface_path];
                    for (name, interface_function) in &interface.methods {
                        let Some((struct_function, constraints)) = self.structs[path].methods.get(name) else {
                            return false;
                        };

                        for (idx, argument) in arguments.iter().enumerate() {
                            let empty_constraint = &HashSet::new();
                            let constraint = constraints
                                .get(&idx)
                                .unwrap_or(empty_constraint);

                            if !self.does_satisfy_constraint(argument, constraint) {
                                return false;
                            }
                        }

                        let struct_function = if let Type::Forall(variables, _) = &self.structs[path].ty {
                            let map = variables
                                .iter()
                                .cloned()
                                .map(|variable| variable.idx)
                                .zip(arguments.clone())
                                .collect();

                            MonoType::Function(struct_function.clone())
                                .replace_type_constants(&map)
                                .into_function()
                        } else {
                            struct_function.clone()
                        };

                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, m.clone())]);
                        let interface_function = MonoType::Function(interface_function.clone())
                            .replace_type_constants(&map)
                            .into_function();

                        if struct_function != interface_function {
                            return false;
                        }
                    }
                }

                true
            },
            MonoType::Var(type_var) | MonoType::Constant(type_var) => {
                for path in interfaces {
                    let interface = &self.interfaces[path];
                    for (name, interface_function) in &interface.methods {
                        let Some(variable_function) = self.find_method_in_interfaces(name, &type_var.interfaces) else {
                            return false;
                        };

                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, m.clone())]);
                        let variable_function = MonoType::Function(variable_function.clone())
                            .replace_type_constants(&map)
                            .into_function();

                        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, m.clone())]);
                        let interface_function = MonoType::Function(interface_function.clone())
                            .replace_type_constants(&map)
                            .into_function();

                        if variable_function != interface_function {
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
            Expression::Path(path) => self.path(path).map(|p| p.0),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection).map(|p| p.0),
            Expression::Let(lett) => self.lett(lett),
            Expression::Sequence(sequence) => self.sequence(sequence),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Match(matc) => self.matc(matc),
            Expression::Return(retrn) => self.retrn(retrn),
            Expression::Assignment(assignment) => self.assignment(assignment),
        }
    }

    // NOTE: bool indicates if the path is assignable
    fn path(&mut self, path: &PathExpression) -> ReportableResult<(MonoType, bool)> {
        let PathExpression { bound, .. } = path;

        match bound {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                let local = self.locals[index].clone();
                let t = if let Type::Mono(m) = local {
                    let t = Type::Mono(m.substitute(&self.unification_table));
                    *self.locals.get_mut(index).unwrap() = t.clone();
                    t
                } else {
                    local
                };

                Ok((self.instantiate(t), true))
            }
            Bound::Absolute(path) => {
                let t = self.names[path].clone();
                Ok((self.instantiate(t), false))
            },
            Bound::Undetermined => unreachable!(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression) -> ReportableResult<MonoType> {
        let ApplicationExpression { function, arguments } = application;

        let m = self.infer(function)?;
        let MonoType::Function(function_type) = m else {
            return self.error(
                TypeCheckError::ExpectedAFunction { encountered: m },
                function.location()
            );
        };

        let FunctionType { arguments: expected_types, return_type } = function_type;

        if arguments.len() != expected_types.len() {
            return self.error(
                TypeCheckError::ArityMismatch {
                    expected: expected_types.len(),
                    encountered: arguments.len()
                },
                function.location()
            );
        }

        let mut argument_types = vec![];
        for argument in arguments {
            argument_types.push((self.infer(argument)?, argument.location()));
        }

        for ((argument, location), expected) in argument_types.iter().zip(expected_types) {
            if !self.unify(argument.clone(), expected.clone()) {
                return self.error(
                    TypeCheckError::MismatchedTypes {
                        encountered: argument.clone().substitute(&self.unification_table),
                        expected: expected.substitute(&self.unification_table)
                    },
                    *location
                );
            };
        }

        Ok(return_type.substitute(&self.unification_table))
    }

    // NOTE: bool indicates if the projection is assignable
    fn projection(&mut self, projection: &ProjectionExpression) -> ReportableResult<(MonoType, bool)> {
        let ProjectionExpression { expression, name } = projection;

        let m = self.infer(expression)?;
        match &m {
            MonoType::Variant(path, arguments) => {
                let Some((method_type, constraints)) = self.variants[path].methods.get(name.data()) else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                for (idx, argument) in arguments.iter().enumerate() {
                    let empyt_constraint = &HashSet::new();
                    let constraint = constraints
                        .get(&idx)
                        .unwrap_or(empyt_constraint);

                    if !self.does_satisfy_constraint(argument, constraint) {
                        return self.error(
                            TypeCheckError::DontImplementInterfaces {
                                t: argument.clone(),
                                interfaces: constraint.clone()
                            },
                            expression.location(),
                        );
                    }
                }

                if let Type::Forall(variables, _) = &self.variants[path].ty {
                    let map = variables
                        .iter()
                        .cloned()
                        .map(|variable| variable.idx)
                        .zip(arguments.clone())
                        .collect();

                    Ok((MonoType::Function(method_type.clone()).replace_type_constants(&map), false))
                } else {
                    Ok((MonoType::Function(method_type.clone()), false))
                }
            },
            MonoType::Struct(path, arguments) => {
                if let Some(m) = self.structs[path].fields.get(name.data()).cloned() {
                    return if let Type::Forall(variables, _) = &self.structs[path].ty {
                        let map = variables
                            .iter()
                            .cloned()
                            .map(|variable| variable.idx)
                            .zip(arguments.clone())
                            .collect();

                        Ok((m.substitute(&map), true))
                    } else {
                        Ok((m, true))
                    }
                };

                let Some((method_type, constraints)) = self.structs[path].methods.get(name.data()) else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                for (idx, argument) in arguments.iter().enumerate() {
                    let empyt_constraint = &HashSet::new();
                    let constraint = constraints
                        .get(&idx)
                        .unwrap_or(empyt_constraint);

                    if !self.does_satisfy_constraint(argument, constraint) {
                        return self.error(
                            TypeCheckError::DontImplementInterfaces {
                                t: argument.clone(),
                                interfaces: constraint.clone()
                            },
                            expression.location(),
                        );
                    }
                }

                if let Type::Forall(variables, _) = &self.structs[path].ty {
                    let map = variables
                        .iter()
                        .cloned()
                        .map(|variable| variable.idx)
                        .zip(arguments.clone())
                        .collect();

                    Ok((MonoType::Function(method_type.clone()).replace_type_constants(&map), false))
                } else {
                    Ok((MonoType::Function(method_type.clone()), false))
                }
            }
            MonoType::Constant(type_var) | MonoType::Var(type_var) => {
                let Some(variable_function) = self.find_method_in_interfaces(name.data(), &type_var.interfaces) else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *name.data()
                        },
                        name.location()
                    );
                };

                let map = HashMap::from([(INTERFACE_CONSTANT_IDX, m.clone())]);
                Ok((MonoType::Function(variable_function.clone()).replace_type_constants(&map), false))
            }
            _ => {
                self.error(
                    TypeCheckError::NotProjectable {
                        ty: m,
                        name: *name.data()
                    },
                    name.location()
                )
            }
        }
    }

    fn lett(&mut self, lett: &LetExpression) -> ReportableResult<MonoType> {
        let LetExpression { value_expression, body_expression, .. } = lett;

        let m = self.infer(value_expression)?;
        let t = self.generalize(m);

        let return_type;
        scoped!(self, {
            self.locals.push(t);
            return_type = self.infer(body_expression)?;
        });

        Ok(return_type)
    }

    fn sequence(&mut self, sequence: &SequenceExpression) -> ReportableResult<MonoType> {
        let SequenceExpression { expressions } = sequence;

        match &expressions[..] {
            [] => Ok(MonoType::Unit),
            [init@.., last] => {
                init
                    .iter().map(|expression| self.infer(expression))
                    .collect::<ReportableResult<Vec<_>>>()?;

                self.infer(last)
            }
        }
    }

    fn lambda(&mut self, lambda: &LambdaExpression) -> ReportableResult<MonoType> {
        let LambdaExpression { arguments, body } = lambda;

        let return_type = MonoType::Var(self.newvar());
        let variables = arguments
            .iter().map(|_| self.newvar())
            .map(MonoType::Var)
            .collect::<Vec<_>>();

        scoped!(self, {
            self.locals.extend(variables
                .iter().cloned()
                .map(Type::Mono)
            );

            self.return_type.push(return_type.clone());
            self.check(body, return_type.clone())?;
            self.return_type.pop();
        });

        let arguments = variables
            .into_iter()
            .map(|variable| variable.substitute(&self.unification_table))
            .collect::<Vec<_>>();

        let return_type = Box::new(return_type.substitute(&self.unification_table));

        let function_type = FunctionType { arguments, return_type };
        Ok(MonoType::Function(function_type))
    }

    fn assignment(&mut self, assignment: &AssignmentExpression) -> ReportableResult<MonoType> {
        let AssignmentExpression { assignable, expression } = assignment;

        let (assignable_type, is_assignable) = match assignable.data() {
            Expression::Path(path) => self.path(path)?,
            Expression::Projection(projection) => self.projection(projection)?,
            _ => return self.error(TypeCheckError::NotAssignable, assignable.location())
        };

        if !is_assignable {
            return self.error(TypeCheckError::NotAssignable, assignable.location());
        }

        self.check(expression, assignable_type)?;

        Ok(MonoType::Unit)
    }

    // TODO: unify should return Result for better error reporting
    // TODO: How inference is done is much like Algorithm W,
    //   at some point Algorithm J like inference would be better
    fn unify(&mut self, a: MonoType, b: MonoType) -> bool {
        match (a, b) {
            (MonoType::Variant(path1, args1), MonoType::Variant(path2, args2)) => {
                if path1 != path2 {
                    return false;
                }

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !self.unify(arg1, arg2) {
                        return false;
                    }
                }

                true
            },
            (MonoType::Function(function1), MonoType::Function(function2)) => {
                let FunctionType { arguments: args1, return_type: return1 } = function1;
                let FunctionType { arguments: args2, return_type: return2 } = function2;

                for (arg1, arg2) in args1.into_iter().zip(args2) {
                    if !self.unify(arg1, arg2) {
                        return false;
                    }
                }

                self.unify(*return1, *return2)
            },
            (MonoType::Var(var1), MonoType::Var(var2)) => {
                let TypeVar { idx: idx1, interfaces: interfaces1 } = &var1;
                let TypeVar { idx: idx2, interfaces: interfaces2 } = &var2;

                match (self.unification_table.get(idx1).cloned(), self.unification_table.get(idx2).cloned()) {
                    (None, Some(m)) => self.unify(MonoType::Var(var1), m),
                    (Some(m), None) => self.unify(m, MonoType::Var(var2)),
                    (Some(m1), Some(m2)) => self.unify(m1, m2),
                    (None, None) => {
                        let mut newvar = self.newvar();
                        newvar.interfaces = interfaces1
                            .union(interfaces2)
                            .cloned()
                            .collect();

                        self.unification_table.insert(*idx1, MonoType::Var(newvar.clone()));
                        self.unification_table.insert(*idx2, MonoType::Var(newvar));

                        true
                    },
                }
            },
            (t, MonoType::Var(var)) | (MonoType::Var(var), t) => {
                let TypeVar { idx, interfaces } = var;

                if t.occurs(idx) {
                    return false;
                }

                match self.unification_table.get(&idx).cloned() {
                    Some(m) => self.unify(m, t),
                    None => {
                        if !self.does_satisfy_constraint(&t, &interfaces) {
                            return false;
                        }
                        // NOTE: Here t cannot be a MonoType::Var
                        //   so they cannot be equal
                        self.unification_table.insert(idx, t);
                        true
                    }
                }
            },
            // NOTE: Bottom type is the subtype of all types
            // NOTE: Here _ cannot be a MonoType::Var
            (MonoType::Bottom, _) | (_, MonoType::Bottom) => true,
            (a, b) => a == b
        }
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
        encountered: MonoType,
        expected: MonoType,
    },
    DuplicateMethodDeclaration {
        variant_path: Path,
        method_name: InternIdx,
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
        expected: MonoType,
    },
    ExpectedAFunction {
        encountered: MonoType,
    },
    ArityMismatch {
        expected: usize,
        encountered: usize,
    },
    NotProjectable {
        ty: MonoType,
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
        t: MonoType,
        interfaces: HashSet<Path>,
    },
    NotAssignable
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
            TypeCheckError::ExpectedAFunction { encountered } => {
                format!("A function is expected but encountered `{}`.", encountered.display(interner),)
            }
            TypeCheckError::ArityMismatch { encountered, expected } => {
                format!("Function is of arity {} but supplied {} arguments.",
                    expected, encountered
                )
            }
            TypeCheckError::NotProjectable { ty, name } => {
                format!("`{}` has no method or field named `{}`.",
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
            TypeCheckError::NotAssignable => {
                "Expression is not assignable.".into()
            }
        }
    }
}
