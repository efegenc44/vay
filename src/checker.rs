use std::collections::{HashMap, HashSet};

use crate::{
    bound::{Bound, Path},
    declaration::{self, Declaration},
    expression::{self, Expression, pattern::Pattern},
    type_expression::{self, TypeExpression},
    interner::{interner, InternIdx},
    location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    runner,
    typ::{
        BuiltInType, FunctionType, Interface, MethodType,
        MonoType, Type, TypeVar
    }
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

const INTERFACE_CONSTANT_IDX: usize = 0;

pub struct Checker {
    value_types: HashMap<Path, Type>,
    types: HashMap<Path, Type>,

    fields: HashMap<Path, HashMap<InternIdx, MonoType>>,
    cases: HashMap<Path, HashMap<InternIdx, Vec<MonoType>>>,
    methods: HashMap<Path, HashMap<InternIdx, MethodType>>,

    interfaces: HashMap<Path, Interface>,

    defines: HashMap<Path, Located<Expression>>,

    builtin_paths: HashMap<BuiltInType, Path>,

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
            value_types: HashMap::new(),
            types: HashMap::new(),
            fields: HashMap::new(),
            cases: HashMap::new(),
            methods: HashMap::new(),
            interfaces: HashMap::new(),
            defines: HashMap::new(),
            builtin_paths: HashMap::new(),
            locals: vec![],
            return_type: vec![],
            // NOTE: 0 is reserved for interfaces' self reference type constant
            type_var_counter: 1,
            current_source: String::new(),
            unification_table: HashMap::new(),
        }
    }

    pub fn init_interactive_session(&mut self) {
        self.current_source = runner::SESSION_SOURCE.into();
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

    fn declaration_type(&mut self, m: MonoType, type_vars: Vec<TypeVar>) -> Type {
        if type_vars.is_empty() {
            Type::Mono(m)
        } else {
            Type::Forall(type_vars, m)
        }
    }

    fn type_vars(&mut self, type_vars: &[Located<declaration::TypeVar>]) -> Vec<TypeVar> {
        let mut vars = vec![];
        for var in type_vars {
            let mut newvar = self.newvar();
            let interfaces = var
                .data()
                .interfaces()
                .iter()
                .map(|interface| interface.1.clone());

            newvar.interfaces.extend(interfaces);
            vars.push(newvar);
        }

        vars
    }

    fn type_vars_of_type(&self, type_path: &Path) -> Vec<TypeVar> {
        if let Type::Forall(type_vars, _) = self.types[type_path].clone() {
            type_vars
        } else {
            vec![]
        }
    }

    fn get_function_type(
        &mut self,
        arguments: &[Located<declaration::TypedIdentifier>],
        return_type: Option<&Located<TypeExpression>>,
    ) -> ReportableResult<FunctionType> {
        let arguments = arguments
            .iter().map(|argument| self.eval_to_mono(argument.data().type_expression()))
            .collect::<ReportableResult<Vec<_>>>()?;

        let return_type = if let Some(return_type) = return_type {
            Box::new(self.eval_to_mono(return_type)?)
        } else {
            Box::new(MonoType::Unit)
        };

        Ok(FunctionType { arguments, return_type })
    }

    fn define_type_vars(&mut self, type_vars: Vec<TypeVar>) {
        let ms = type_vars
            .into_iter()
            .map(MonoType::Var)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn define_type_constants(&mut self, type_vars: Vec<TypeVar>) {
        let ms = type_vars
            .into_iter()
            .map(MonoType::Constant)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn define_constrained_type_constants(
        &mut self,
        type_vars: Vec<TypeVar>,
        constraints: &HashMap<usize, HashSet<Path>>
    ) {
        let ms = type_vars
            .into_iter().enumerate()
            .map(|(idx, mut type_var)| {
                type_var.interfaces = constraints.get(&idx).cloned().unwrap_or(HashSet::new());
                type_var
            })
            .map(MonoType::Constant)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn argumented_typed(&self, type_path: &Path) -> MonoType {
        match self.types[type_path].clone() {
            Type::Mono(m) => m,
            Type::Forall(variables, m) => {
                let arguments = variables
                    .iter().cloned()
                    .map(MonoType::Var)
                    .collect();

                match m {
                    MonoType::Variant(path, _) => MonoType::Variant(path, arguments),
                    MonoType::Struct(path, _) => MonoType::Struct(path, arguments),
                    MonoType::BuiltIn(path, builtin, _) => MonoType::BuiltIn(path, builtin, arguments),
                    _ => unreachable!()
                }
            },
        }
    }

    fn constantize(&self, t: Type) -> MonoType {
        match t {
            Type::Mono(m) => m,
            Type::Forall(type_vars, m) => {
                let map = type_vars
                    .iter().cloned()
                    .map(|var| (var.idx, MonoType::Constant(var)))
                    .collect();

                m.substitute(&map)
            }
        }
    }

    fn substitute_arguments(&self, type_path: &Path, arguments: Vec<MonoType>, ms: Vec<MonoType>) -> Vec<MonoType> {
        if let Type::Forall(type_variables, _) = &self.types[type_path] {
            let map = type_variables
                .iter().cloned()
                .map(|variable| variable.idx)
                .zip(arguments)
                .collect();

            ms.into_iter().map(|m| m.substitute(&map)).collect()
        } else {
            ms
        }
    }

    fn substitute_arguments_one(&self, type_path: &Path, arguments: Vec<MonoType>, m: MonoType) -> MonoType {
        let mut m = self.substitute_arguments(type_path, arguments, vec![m]);
        m.pop().unwrap()
    }

    fn replace_arguments(&self, type_path: &Path, arguments: Vec<MonoType>, ms: Vec<MonoType>) -> Vec<MonoType> {
        if let Type::Forall(type_variables, _) = &self.types[type_path] {
            let map = type_variables
                .iter().cloned()
                .map(|variable| variable.idx)
                .zip(arguments)
                .collect();

            ms.into_iter().map(|m| m.replace_type_constants(&map)).collect()
        } else {
            ms
        }
    }

    fn replace_arguments_one(&self, type_path: &Path, arguments: Vec<MonoType>, m: MonoType) -> MonoType {
        let mut m = self.replace_arguments(type_path, arguments, vec![m]);
        m.pop().unwrap()
    }

    fn eval_type_expression(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<Type> {
        match type_expression.data() {
            TypeExpression::Path(type_path) => self.eval_path_type(type_path),
            TypeExpression::Function(function) => self.eval_function_type(function),
            TypeExpression::Application(type_application) => self.eval_type_application(type_application),
            TypeExpression::Unit => Ok(Type::Mono(MonoType::Unit)),
        }
    }

    fn eval_path_type(&mut self, type_path: &type_expression::Path) -> ReportableResult<Type> {
        match type_path.bound() {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            }
            Bound::Absolute(path) => {
                Ok(self.types[path].clone())
            },
            Bound::Undetermined => unreachable!()
        }
    }

    fn eval_function_type(&mut self, function_type: &type_expression::Function) -> ReportableResult<Type> {
        let arguments = function_type.arguments()
            .iter().map(|argument| self.eval_to_mono(argument))
            .collect::<ReportableResult<Vec<_>>>()?;

        let return_type = function_type.return_type()
            .as_ref()
            .map(|return_type| self.eval_to_mono(return_type))
            .unwrap_or(Ok(MonoType::Unit))?;

        let return_type = Box::new(return_type);

        Ok(Type::Mono(MonoType::Function(FunctionType { arguments, return_type })))
    }

    fn eval_type_application(&mut self, type_application: &type_expression::Application) -> ReportableResult<Type> {
        let t = self.eval_type_expression(type_application.function())?;
        let Type::Forall(variables, mut m) = t else {
            return self.error(
                TypeCheckError::NotAPolyType {
                    encountered: t
                },
                type_application.function().location()
            )
        };

        if variables.len() != type_application.arguments().len() {
            return self.error(
                TypeCheckError::TypeArityMismatch {
                    expected: variables.len(),
                    encountered: type_application.arguments().len()
                },
                type_application.function().location()
            )
        }

        match &mut m {
            MonoType::Variant(_, variant_arguments) |
            MonoType::Struct(_, variant_arguments) |
            MonoType::BuiltIn(_, _, variant_arguments) => {
                for (argument, variable) in type_application.arguments().iter().zip(variables) {
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

    pub fn type_check(&mut self, modules: &[declaration::Module]) -> ReportableResult<()> {
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

    fn module(&mut self, module: &declaration::Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            self.declaration(declaration)?;
        }

        Ok(())
    }

    fn collect_types(&mut self, module: &declaration::Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Interface(interface) => self.collect_interface_type(interface)?,
                Declaration::Variant(variant) => self.collect_variant_type(variant)?,
                Declaration::Struct(strct) => self.collect_struct_type(strct)?,
                Declaration::BuiltIn(builtin) => self.collect_builtin_type(builtin)?,
                _ => ()
            }
        }

        Ok(())
    }

    fn collect_names(&mut self, module: &declaration::Module) -> ReportableResult<()> {
        for declaration in module.declarations() {
            if let Declaration::Interface(interface) = declaration {
                self.collect_interface_name(interface)?
            }
        }

        for declaration in module.declarations() {
            if let Declaration::Interface(interface) = declaration {
                self.collect_interface_name_extras(interface)?
            }
        }

        for declaration in module.declarations() {
            match declaration {
                Declaration::Variant(variant) => self.collect_variant_name(variant)?,
                Declaration::Struct(strct) => self.collect_struct_name(strct)?,
                Declaration::BuiltIn(builtin) => self.collect_builtin_name(builtin)?,
                Declaration::External(external) => self.collect_external_name(external)?,
                _ => (),
            }
        }

        for declaration in module.declarations() {
            if let Declaration::Function(function) = declaration {
                self.collect_function_name(function)?
            }
        }

        for declaration in module.declarations() {
            if let Declaration::Define(define) = declaration {
                self.collect_define_name(define)?
            }
        }

        Ok(())
    }

    fn collect_function_name(&mut self, function: &declaration::Function) -> ReportableResult<()> {
        let type_vars = self.type_vars(function.type_vars());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            let m = self.get_function_type(function.arguments(), function.return_type())?.into_mono();
            let t = self.declaration_type(m, type_vars);

            self.value_types.insert(function.path().clone(), t);
        });

        Ok(())
    }

    fn collect_define_name(&mut self, define: &declaration::Define) -> ReportableResult<()> {
        let t = self.eval_type_expression(define.type_expression())?;
        self.value_types.insert(define.path().clone(), t);
        self.defines.insert(define.path().clone(), define.expression().clone());

        Ok(())
    }

    fn collect_variant_type(&mut self, variant: &declaration::Variant) -> ReportableResult<()> {
        let type_vars = variant.type_vars().iter().map(|_| self.newvar()).collect();

        let m = MonoType::Variant(variant.path().clone(), vec![]);
        let t = self.declaration_type(m, type_vars);

        self.types.insert(variant.path().clone(), t);

        Ok(())
    }

    fn collect_struct_type(&mut self, strct: &declaration::Struct) -> ReportableResult<()> {
        let type_vars = strct.type_vars().iter().map(|_| self.newvar()).collect();

        let m = MonoType::Struct(strct.path().clone(), vec![]);
        let t = self.declaration_type(m, type_vars);

        self.types.insert(strct.path().clone(), t);

        Ok(())
    }

    fn collect_interface_type(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        self.interfaces.insert(interface.path().clone(), Interface { methods: HashMap::new() });

        Ok(())
    }

    fn collect_builtin_type(&mut self, builtin: &declaration::BuiltIn) -> ReportableResult<()> {
        let builtin_type = match interner().get(builtin.name().data()) {
            "U64" => BuiltInType::U64,
            "F32" => BuiltInType::F32,
            "Char" => BuiltInType::Char,
            "Array" => BuiltInType::Array,
            _ => panic!("Unknown builtin")
        };

        let type_vars = builtin.type_vars().iter().map(|_| self.newvar()).collect();

        let m = MonoType::BuiltIn(builtin.path().clone(), builtin_type, vec![]);
        let t = self.declaration_type(m, type_vars);

        self.builtin_paths.insert(builtin_type, builtin.path().clone());
        self.types.insert(builtin.path().clone(), t);

        Ok(())
    }

    fn collect_external_name(&mut self, external: &declaration::External) -> ReportableResult<()> {
        let type_vars = self.type_vars(external.type_vars());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            let m = self.get_function_type(external.arguments(), external.return_type())?.into_mono();
            let t = self.declaration_type(m, type_vars);

            self.value_types.insert(external.path().clone(), t);
        });

        Ok(())
    }

    fn method_constraints(constraints: &[declaration::MethodConstraint]) -> HashMap<usize, HashSet<Path>> {
        fn interfaces(type_var: &Located<declaration::TypeVar>) -> HashSet<Path> {
            type_var
                .data()
                .interfaces()
                .iter()
                .map(|interface| interface.1.clone())
                .collect()
        }

        constraints
            .iter()
            .map(|constraint| (constraint.nth(), interfaces(constraint.type_var())))
            .collect()
    }

    fn collect_method_signatures(&mut self, path: &Path, signature: &declaration::MethodSignature) -> ReportableResult<()> {
        let constraints = Self::method_constraints(signature.constraints());

        scoped!(self, {
            if let Type::Forall(type_vars, _) = self.types[path].clone() {
                self.define_constrained_type_constants(type_vars, &constraints);
            }

            let type_vars = self.type_vars(signature.type_vars());
            self.define_type_vars(type_vars.clone());

            let function_type = self.get_function_type(signature.arguments(), signature.return_type())?;
            let method_type = MethodType { function_type, constraints, type_vars };

            let methods = self.methods.get_mut(path).unwrap();
            if methods.insert(*signature.name().data(), method_type).is_some() {
                return self.error(
                    TypeCheckError::DuplicateMethodDeclaration {
                        variant_path: path.clone(),
                        method_name: *signature.name().data(),
                    },
                    signature.name().location(),
                );
            }
        });

        Ok(())
    }

    fn collect_variant_name(&mut self, variant: &declaration::Variant) -> ReportableResult<()> {
        self.methods.insert(variant.path().clone(), HashMap::new());
        for method in variant.methods() {
            self.collect_method_signatures(variant.path(), method.signature())?;
        }

        let type_vars = self.type_vars_of_type(variant.path());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());
            let variant_type = self.argumented_typed(variant.path());
            self.cases.insert(variant.path().clone(), HashMap::new());

            let mut variant_cases = HashMap::new();
            for case in variant.cases() {
                let case_name = *case.data().identifier().data();
                let case_path = case.data().path().clone();

                let m = if let Some(arguments) = case.data().arguments() {
                    let arguments = arguments
                        .iter().map(|argument| self.eval_to_mono(argument))
                        .collect::<ReportableResult<Vec<_>>>()?;

                    let return_type = Box::new(variant_type.clone());

                    variant_cases.insert(case_name, arguments.clone());
                    FunctionType { arguments, return_type }.into_mono()
                } else {
                    variant_cases.insert(case_name, vec![]);
                    variant_type.clone()
                };

                let t = self.declaration_type(m, type_vars.clone());
                self.value_types.insert(case_path, t);
            }

            self.cases.insert(variant.path().clone(), variant_cases);
        });

        Ok(())
    }

    fn collect_struct_name(&mut self, strct: &declaration::Struct) -> ReportableResult<()> {
        self.methods.insert(strct.path().clone(), HashMap::new());
        for method in strct.methods() {
            self.collect_method_signatures(strct.path(), method.signature())?;
        }

        let type_vars = self.type_vars_of_type(strct.path());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            let struct_type = self.argumented_typed(strct.path());

            let arguments = strct.fields()
                .iter()
                .map(|field| self.eval_to_mono(field.data().type_expression()))
                .collect::<ReportableResult<Vec<_>>>()?;

            let fields = strct.fields()
                .iter()
                .zip(arguments.clone())
                .map(|(field, argument)| (*field.data().identifier().data(), argument))
                .collect::<HashMap<_, _>>();

            self.fields.insert(strct.path().clone(), fields);

            let return_type = Box::new(struct_type.clone());

            let m = FunctionType { arguments, return_type }.into_mono();
            let t = self.declaration_type(m, type_vars.clone());

            self.value_types.insert(strct.path().clone(), t);
        });

        Ok(())
    }

    fn collect_builtin_name(&mut self, builtin: &declaration::BuiltIn) -> ReportableResult<()> {
        self.methods.insert(builtin.path().clone(), HashMap::new());
        for (signature, _) in builtin.methods() {
            self.collect_method_signatures(builtin.path(), signature)?;
        }

        Ok(())
    }

    fn declaration(&mut self, declaration: &Declaration) -> ReportableResult<()> {
        match declaration {
            Declaration::Variant(variant) => self.variant(variant),
            Declaration::Define(define) => self.define(define),
            Declaration::Function(function) => self.function(function),
            Declaration::Struct(strct) => self.strct(strct),
            Declaration::BuiltIn(builtin) => self.builtin(builtin),
            _ => Ok(())
        }
    }

    fn variant(&mut self, variant: &declaration::Variant) -> ReportableResult<()> {
        for method in variant.methods() {
            let method_type = &self.methods[variant.path()][method.signature().name().data()];
            let FunctionType { arguments, return_type } = self.constantize(
                Type::Forall(
                    method_type.type_vars.clone(),
                    MonoType::Function(method_type.function_type.clone())
                )
            ).into_function();
            let variant_type = self.method_instance_type(variant.path(), &method_type.constraints);

            scoped!(self, {
                self.locals.push(Type::Mono(variant_type));
                self.locals.extend(arguments.into_iter().map(Type::Mono));
                self.return_type.push(*return_type.clone());
                self.check(method.body(), *return_type)?;
                self.return_type.pop();
            });
        }

        Ok(())
    }

    fn builtin(&mut self, builtin: &declaration::BuiltIn) -> ReportableResult<()> {
        for (signature, body) in builtin.methods() {
            let Some(body) = body else {
                continue;
            };

            let method_type = &self.methods[builtin.path()][signature.name().data()];
            let FunctionType { arguments, return_type } = self.constantize(
                Type::Forall(
                    method_type.type_vars.clone(),
                    MonoType::Function(method_type.function_type.clone())
                )
            ).into_function();
            let variant_type = self.method_instance_type(builtin.path(), &method_type.constraints);

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

    fn method_instance_type(&self, type_path: &Path, constraints: &HashMap<usize, HashSet<Path>>) -> MonoType {
        match self.types[type_path].clone() {
            Type::Mono(m) => m,
            Type::Forall(type_vars, m) => {
                let arguments = type_vars
                    .into_iter().enumerate()
                    .map(|(idx, mut type_var)| {
                        type_var.interfaces = constraints.get(&idx).cloned().unwrap_or(HashSet::new());
                        type_var
                    })
                    .map(MonoType::Constant)
                    .collect();

                match m {
                    MonoType::Variant(path, _) => MonoType::Variant(path, arguments),
                    MonoType::Struct(path, _) => MonoType::Struct(path, arguments),
                    MonoType::BuiltIn(path, builtin, _) => MonoType::BuiltIn(path, builtin, arguments),
                    _ => unreachable!()
                }
            },
        }
    }

    fn cyclic_define(&self, expression: &Located<Expression>, current_define: &Path) -> bool {
        match expression.data() {
            Expression::U64(_) |
            Expression::F32(_) |
            Expression::String(_) |
            Expression::Char(_) |
            Expression::Lambda(_) |
            Expression::Continue |
            Expression::Break => false,

            Expression::Path(path) => {
                match path.bound() {
                    Bound::Local(_) => false,
                    Bound::Absolute(path) => {
                        if current_define == path {
                            return true;
                        }

                        let Some(define) = self.defines.get(path) else {
                            return true;
                        };

                        self.cyclic_define(define, current_define)
                    },
                    Bound::Undetermined => unreachable!(),
                }
            },

            Expression::Array(array) => {
                array.expressions()
                    .iter().any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Application(application) => {
                self.cyclic_define(&application.function(), current_define) ||
                application.arguments()
                    .iter().any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Projection(projection) => {
                self.cyclic_define(&projection.expression(), current_define)
            },
            Expression::Let(lett) => {
                self.cyclic_define(&lett.value_expression(), current_define) ||
                self.cyclic_define(&lett.body_expression(), current_define)
            },
            Expression::Sequence(sequence) => {
                sequence.expressions()
                    .iter().any(|expression| self.cyclic_define(expression, current_define))
            }
            Expression::Block(block) => {
                block.expressions()
                    .iter().any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Match(mtch) => {
                mtch.expressions()
                    .iter().any(|expression| self.cyclic_define(expression, current_define)) ||
                mtch.branches()
                    .iter().any(|branch| self.cyclic_define(branch.data().expression(), current_define))
            },
            Expression::Return(retrn) => {
                self.cyclic_define(&retrn.expression(), current_define)
            },
            Expression::Assignment(assignment) => {
                self.cyclic_define(&assignment.expression(), current_define)
            },
            Expression::While(whilee) => {
                self.cyclic_define(&whilee.condition(), current_define) ||
                self.cyclic_define(&whilee.body(), current_define) ||
                whilee.post()
                    .as_ref()
                    .map(|expression| self.cyclic_define(expression, current_define))
                    .unwrap_or(false)
            },
        }
    }

    fn define(&mut self, define: &declaration::Define) -> ReportableResult<()> {
        if self.cyclic_define(define.expression(), define.path()) {
            panic!("Error: Cyclic define");
        }

        let Type::Mono(expected) = self.value_types[define.path()].clone() else {
            unreachable!();
        };
        self.check(define.expression(), expected)
    }

    fn function(&mut self, function: &declaration::Function) -> ReportableResult<()> {
        let t = self.value_types[function.path()].clone();
        let FunctionType { arguments, return_type } = self.constantize(t).into_function();

        scoped!(self, {
            self.locals.extend(arguments.into_iter().map(Type::Mono));
            self.return_type.push(*return_type.clone());
            self.check(function.body(), *return_type)?;
            self.return_type.pop();
        });

        Ok(())
    }

    fn strct(&mut self, strct: &declaration::Struct) -> ReportableResult<()> {
        for method in strct.methods() {
            let method_type = &self.methods[strct.path()][method.signature().name().data()];
            let FunctionType { arguments, return_type } = self.constantize(
                Type::Forall(
                    method_type.type_vars.clone(),
                    MonoType::Function(method_type.function_type.clone())
                )
            ).into_function();
            let struct_type = self.method_instance_type(strct.path(), &method_type.constraints);

            scoped!(self, {
                self.locals.push(Type::Mono(struct_type));
                self.locals.extend(arguments.into_iter().map(Type::Mono));
                self.return_type.push(*return_type.clone());
                self.check(method.body(), *return_type)?;
                self.return_type.pop();
            });
        }

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        let instance_type_var = TypeVar {
            idx: INTERFACE_CONSTANT_IDX,
            interfaces: HashSet::from([interface.path().clone()])
        };

        let type_vars = vec![instance_type_var.clone()];

        scoped!(self, {
            self.define_type_constants(type_vars.clone());

            for method in interface.methods() {
                let function_type = self.get_function_type(method.arguments(), method.return_type())?;

                self.interfaces.get_mut(interface.path()).unwrap().methods
                    .insert(*method.name().data(), function_type);
            }
        });

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            for method in interface.methods() {
                let mut arguments_with_instance = vec![MonoType::Var(instance_type_var.clone())];
                let mut function_type = self.get_function_type(method.arguments(), method.return_type())?;
                arguments_with_instance.extend(function_type.arguments);
                function_type.arguments = arguments_with_instance;

                let t = self.declaration_type(function_type.into_mono(), type_vars.clone());
                self.value_types.insert(method.path().clone(), t);
            }
        });

        Ok(())
    }

    fn collect_interface_name_extras(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        let paths = interface.type_name().data().interfaces().iter().map(|interface| &interface.1);
        let interfaces = paths.map(|path| &self.interfaces[path]);

        let mut extra_methods = HashMap::new();
        for interface in interfaces {
            for method in &interface.methods {
                extra_methods.insert(*method.0, method.1.clone());
            }
        }

        self.interfaces.get_mut(interface.path()).unwrap().methods.extend(extra_methods);

        Ok(())
    }

    fn matc(&mut self, matc: &expression::Match) -> ReportableResult<MonoType> {
        // TODO: Exhaustiveness check
        let mut ms = matc.expressions()
            .iter().map(|expression| self.infer(expression))
            .collect::<ReportableResult<Vec<_>>>()?;
        let mut return_type = MonoType::Var(self.newvar());

        match &matc.branches()[..] {
            [] => return_type = MonoType::Unit,
            branches => {
                for branch in branches {
                    scoped!(self, {
                        if ms.len() != branch.data().patterns().len() {
                            todo!("Patterns dont match");
                        }

                        for (m, pattern) in ms.iter().zip(branch.data().patterns()) {
                            if !self.type_pattern_match(m.clone(), pattern)? {
                                // TODO: Remove push locals by type_pattern_match()
                                return self.error(
                                    TypeCheckError::NotAPatternOfType { expected: m.clone() },
                                    pattern.location(),
                                );
                            }
                        }

                        // NOTE: Bottom type is the subtype of all types
                        // TODO: Maybe self.check should return the most general type
                        if let MonoType::Bottom = return_type {
                            return_type = self.infer(branch.data().expression())?;
                        } else {
                            self.check(branch.data().expression(), return_type.clone())?;
                        }
                        for m in ms.iter_mut() {
                            *m = m.clone().substitute(&self.unification_table);
                        }
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
            (MonoType::BuiltIn(_, BuiltInType::U64, _), Pattern::U64(_)) |
            (MonoType::BuiltIn(_, BuiltInType::F32, _), Pattern::F32(_)) |
            (MonoType::BuiltIn(_, BuiltInType::Char, _), Pattern::Char(_)) => Ok(true),
            (MonoType::BuiltIn(_, BuiltInType::Array, arg), Pattern::String(_)) => {
                if arg.len() != 1 {
                    return Ok(false);
                }

                let Some(MonoType::BuiltIn(_, BuiltInType::Char, _)) = arg.first() else {
                    return Ok(false);
                };

                Ok(true)
            }
            (MonoType::BuiltIn(_, BuiltInType::Array, arguments), Pattern::Array(array)) => {
                let argument = arguments.last().unwrap().clone();

                for pattern in array.before() {
                    self.type_pattern_match(argument.clone(), pattern)?;
                }

                if array.rest().is_some() {
                    let mut type_path = Path::empty();
                    let view_type_path = "Intrinsics::ArrayView";

                    view_type_path
                        .split("::")
                        .map(|part| type_path.push(interner().intern_idx(part)))
                        .for_each(drop);

                    self.locals.push(Type::Mono(MonoType::Struct(type_path, arguments)));
                }

                for pattern in array.after() {
                    self.type_pattern_match(argument.clone(), pattern)?;
                }

                Ok(true)
            },
            (MonoType::Variant(path, arguments), Pattern::VariantCase(variant_case)) => {
                let cases = &self.cases[&path];
                if !cases.contains_key(variant_case.case().data()) {
                    return self.error(
                        TypeCheckError::CaseNotExist {
                            type_path: path,
                            case_name: *variant_case.case().data(),
                        },
                        variant_case.case().location(),
                    );
                }

                let case_fields = cases[variant_case.case().data()].clone();
                let empty_fields = vec![];
                let fields = variant_case.fields().unwrap_or(&empty_fields);

                if case_fields.len() != fields.len() {
                    return self.error(
                        TypeCheckError::WrongCaseArity {
                            type_path: path,
                            case_name: *variant_case.case().data(),
                            expected: case_fields.len(),
                            encountered: fields.len()
                        },
                        pattern.location(),
                    );
                }

                let case_fields = self.substitute_arguments(&path, arguments, case_fields);

                let result = case_fields
                    .into_iter()
                    .zip(fields)
                    .map(|(field, pattern)| self.type_pattern_match(field, pattern))
                    .collect::<ReportableResult<Vec<_>>>()?
                    .iter()
                    .all(|x| *x);

                Ok(result)
            }
            (MonoType::Unit, Pattern::Unit)  => Ok(true),
            _ => Ok(false)
        }
    }

    fn retrn(&mut self, retrn: &expression::Return) -> ReportableResult<MonoType> {
        let Some(return_type) = self.return_type.last().cloned() else {
            todo!("Return outside of a function");
        };
        self.check(retrn.expression(), return_type)?;

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

    fn does_satisfy_constraint(&mut self, m: &MonoType, interfaces: &HashSet<Path>) -> bool {
        match m {
            MonoType::Variant(path, arguments) |
            MonoType::Struct(path, arguments) |
            MonoType::BuiltIn(path, _, arguments) => {
                for interface_path in interfaces {
                    let interface = &self.interfaces[interface_path];
                    for (name, interface_function) in interface.methods.clone() {
                        let Some(method_type) = self.methods[path].get(&name).cloned() else {
                            return false;
                        };

                        for (idx, argument) in arguments.iter().enumerate() {
                            let empty_constraint = &HashSet::new();
                            let constraint = method_type.constraints
                                .get(&idx)
                                .unwrap_or(empty_constraint);

                            if !self.does_satisfy_constraint(argument, constraint) {
                                return false;
                            }
                        }

                        let function = method_type.function_type.clone().into_mono();
                        let function = self.replace_arguments_one(
                            path,
                            arguments.clone(),
                            function
                        );

                        let interface_function = Self::replace_interface_constants(
                            interface_function.clone().into_mono(),
                            m.clone()
                        );

                        if !self.unify(function, interface_function) {
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

                        let variable_function = Self::replace_interface_constants(
                            variable_function.clone().into_mono(),
                            m.clone()
                        ).into_function();

                        let interface_function = Self::replace_interface_constants(
                            interface_function.clone().into_mono(),
                            m.clone()
                        ).into_function();

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

    fn replace_interface_constants(m: MonoType, instance: MonoType) -> MonoType {
        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, instance)]);
        m.replace_type_constants(&map)
    }

    pub fn infer(&mut self, expression: &Located<Expression>) -> ReportableResult<MonoType> {
        match expression.data() {
            Expression::U64(_) => self.u64(),
            Expression::F32(_) => self.f32(),
            Expression::String(_) => self.string(),
            Expression::Char(_) => self.char(),
            Expression::Path(path) => self.path(path).map(|p| p.0),
            Expression::Array(array) => self.array(array),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection).map(|p| p.0),
            Expression::Let(lett) => self.lett(lett),
            Expression::Sequence(sequence) => self.sequence(sequence),
            Expression::Block(block) => self.block(block),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Match(matc) => self.matc(matc),
            Expression::Return(retrn) => self.retrn(retrn),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::While(whilee) => self.whilee(whilee),
            // TODO: Disallow continue and break expression outside of while
            Expression::Continue |
            Expression::Break => Ok(MonoType::Bottom)
        }
    }

    fn u64(&self) -> ReportableResult<MonoType> {
        let m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::U64].clone(),
            BuiltInType::U64,
            vec![]
        );

        Ok(m)
    }

    fn f32(&self) -> ReportableResult<MonoType> {
        let m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::F32].clone(),
            BuiltInType::F32,
            vec![]
        );

        Ok(m)
    }

    fn string(&self) -> ReportableResult<MonoType> {
        let character_m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::Char].clone(),
            BuiltInType::Char,
            vec![]
        );

        let m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::Array].clone(),
            BuiltInType::Array,
            vec![character_m]
        );

        Ok(m)
    }

    fn char(&self) -> ReportableResult<MonoType> {
        let m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::Char].clone(),
            BuiltInType::Char,
            vec![]
        );

        Ok(m)
    }

    // NOTE: bool indicates if the path is assignable
    fn path(&mut self, path: &expression::Path) -> ReportableResult<(MonoType, bool)> {
        match path.bound() {
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
                let t = self.value_types[path].clone();
                Ok((self.instantiate(t), false))
            },
            Bound::Undetermined => unreachable!(),
        }
    }

    fn array(&mut self, array: &expression::Array) -> ReportableResult<MonoType> {
        let mut inner_type = MonoType::Var(self.newvar());

        for expression in array.expressions() {
            self.check(expression, inner_type.clone())?;
            inner_type = inner_type.substitute(&self.unification_table);
        }

        let m = MonoType::BuiltIn(
            self.builtin_paths[&BuiltInType::Array].clone(),
            BuiltInType::Array,
            vec![inner_type]
        );

        Ok(m)
    }

    fn application(&mut self, application: &expression::Application) -> ReportableResult<MonoType> {
        let m = self.infer(application.function())?;
        let MonoType::Function(function_type) = m else {
            return self.error(
                TypeCheckError::ExpectedAFunction { encountered: m },
                application.function().location()
            );
        };

        let FunctionType { arguments: expected_types, return_type } = function_type;

        if application.arguments().len() != expected_types.len() {
            return self.error(
                TypeCheckError::ArityMismatch {
                    expected: expected_types.len(),
                    encountered: application.arguments().len()
                },
                application.function().location()
            );
        }

        let mut argument_types = vec![];
        for argument in application.arguments() {
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
    fn projection(&mut self, projection: &expression::Projection) -> ReportableResult<(MonoType, bool)> {
        let m = self.infer(projection.expression())?;

        // Field Projection (Only structs)
        if let MonoType::Struct(path, arguments) = &m {
            if let Some(m) = self.fields[path].get(projection.projected().data()).cloned() {
                let m = self.substitute_arguments_one(path, arguments.clone(), m);

                return Ok((m, true))
            }
        };

        // Method Projection
        match &m {
            MonoType::Variant(path, arguments) |
            MonoType::Struct(path, arguments) |
            MonoType::BuiltIn(path, _, arguments) => {
                let Some(method_type) = self.methods[path].get(projection.projected().data()).cloned() else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *projection.projected().data()
                        },
                        projection.projected().location()
                    );
                };

                for (idx, argument) in arguments.iter().enumerate() {
                    let empyt_constraint = &HashSet::new();
                    let constraint = method_type.constraints
                        .get(&idx)
                        .unwrap_or(empyt_constraint);

                    if !self.does_satisfy_constraint(argument, constraint) {
                        return self.error(
                            TypeCheckError::DontImplementInterfaces {
                                t: argument.clone(),
                                interfaces: constraint.clone()
                            },
                            projection.expression().location(),
                        );
                    }
                }

                let m = self.replace_arguments_one(
                    path,
                    arguments.clone(),
                    method_type.function_type.clone().into_mono()
                );

                let t = Type::Forall(method_type.type_vars.clone(), MonoType::Function(m.into_function()));
                let m = self.instantiate(t);

                Ok((m, false))
            },
            MonoType::Constant(type_var) | MonoType::Var(type_var) => {
                let Some(variable_function) = self.find_method_in_interfaces(projection.projected().data(), &type_var.interfaces) else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *projection.projected().data()
                        },
                        projection.projected().location()
                    );
                };

                let variable_function = Self::replace_interface_constants(
                    variable_function.clone().into_mono(), m
                );

                Ok((variable_function, false))
            }
            _ => {
                self.error(
                    TypeCheckError::NotProjectable {
                        ty: m,
                        name: *projection.projected().data()
                    },
                    projection.projected().location()
                )
            }
        }
    }

    fn lett(&mut self, lett: &expression::Let) -> ReportableResult<MonoType> {
        let m = self.infer(lett.value_expression())?;
        let t = self.generalize(m);

        let return_type;
        scoped!(self, {
            self.locals.push(t);
            return_type = self.infer(lett.body_expression())?;
        });

        Ok(return_type)
    }

    fn sequence(&mut self, sequence: &expression::Sequence) -> ReportableResult<MonoType> {
        match &sequence.expressions()[..] {
            [] => Ok(MonoType::Unit),
            [init@.., last] => {
                init
                    .iter().map(|expression| self.infer(expression))
                    .collect::<ReportableResult<Vec<_>>>()?;

                self.infer(last)
            }
        }
    }

    fn block(&mut self, block: &expression::Block) -> ReportableResult<MonoType> {
        block.expressions()
            .iter().map(|expression| self.infer(expression))
            .collect::<ReportableResult<Vec<_>>>()?;

        Ok(MonoType::Unit)
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> ReportableResult<MonoType> {
        let return_type = MonoType::Var(self.newvar());
        let variables = lambda.arguments()
            .iter().map(|_| self.newvar())
            .map(MonoType::Var)
            .collect::<Vec<_>>();

        scoped!(self, {
            self.locals.extend(variables
                .iter().cloned()
                .map(Type::Mono)
            );

            self.return_type.push(return_type.clone());
            self.check(lambda.body(), return_type.clone())?;
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

    fn assignment(&mut self, assignment: &expression::Assignment) -> ReportableResult<MonoType> {
        let (assignable_type, is_assignable) = match assignment.assignable().data() {
            Expression::Path(path) => self.path(path)?,
            Expression::Projection(projection) => self.projection(projection)?,
            _ => return self.error(TypeCheckError::NotAssignable, assignment.assignable().location())
        };

        if !is_assignable {
            return self.error(TypeCheckError::NotAssignable, assignment.assignable().location());
        }

        self.check(assignment.expression(), assignable_type)?;

        Ok(MonoType::Unit)
    }

    fn whilee(&mut self, whilee: &expression::While) -> ReportableResult<MonoType> {
        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let boole = MonoType::Variant(type_path, vec![]);

        self.check(whilee.condition(), boole)?;

        if let Some(post) = whilee.post() {
            self.infer(post)?;
        }

        self.infer(whilee.body())?;

        Ok(MonoType::Unit)
    }

    // TODO: unify should return Result for better error reporting
    // TODO: How inference is done is much like Algorithm W,
    //   at some point Algorithm J like inference would be better
    fn unify(&mut self, a: MonoType, b: MonoType) -> bool {
        match (a, b) {
            (MonoType::Variant(path1, args1), MonoType::Variant(path2, args2)) |
            (MonoType::Struct(path1, args1), MonoType::Struct(path2, args2)) |
            (MonoType::BuiltIn(path1, _, args1), MonoType::BuiltIn(path2, _, args2)) => {
                path1 == path2 && args1.into_iter().zip(args2).all(|(arg1, arg2)| self.unify(arg1, arg2))
            },
            (MonoType::Function(function1), MonoType::Function(function2)) => {
                let FunctionType { arguments: args1, return_type: return1 } = function1;
                let FunctionType { arguments: args2, return_type: return2 } = function2;

                args1.into_iter().zip(args2).all(|(arg1, arg2)| self.unify(arg1, arg2)) && self.unify(*return1, *return2)
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

    fn description(&self) -> String {
        match self.0.data() {
            TypeCheckError::MismatchedTypes {
                encountered,
                expected,
            } => {
                format!(
                    "Expected type `{}` but encountered type `{}`.",
                    expected,
                    encountered
                )
            }
            TypeCheckError::DuplicateMethodDeclaration {
                variant_path,
                method_name,
            } => {
                format!(
                    "Duplicate declaration of method `{}` in variant type `{}`.",
                    interner().get(method_name),
                    variant_path
                )
            }
            TypeCheckError::CaseNotExist {
                type_path,
                case_name,
            } => {
                format!(
                    "Variant `{}` does not have a case named `{}`.",
                    type_path,
                    interner().get(case_name),
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
                    interner().get(case_name),
                    type_path,
                    expected,
                    encountered
                )
            }
            TypeCheckError::NotAPatternOfType { expected } => {
                format!("Pattern is not of type `{}`.", expected)
            }
            TypeCheckError::ExpectedAFunction { encountered } => {
                format!("A function is expected but encountered `{}`.", encountered)
            }
            TypeCheckError::ArityMismatch { encountered, expected } => {
                format!("Function is of arity {} but supplied {} arguments.",
                    expected, encountered
                )
            }
            TypeCheckError::NotProjectable { ty, name } => {
                format!("`{}` has no method or field named `{}`.",
                    ty, interner().get(name)
                )
            }
            TypeCheckError::ExpectedMonoType { encountered } => {
                format!("Type is not mono : `{}`.", encountered)
            }
            TypeCheckError::NotAPolyType { encountered } => {
                format!("Type is not poly : `{}`.", encountered)
            }
            TypeCheckError::TypeArityMismatch { encountered, expected } => {
                format!("Type is of arity {} but supplied {} arguments.",
                    expected, encountered
                )
            }
            TypeCheckError::DontImplementInterfaces { t, interfaces } => {
                format!("Type `{}` does not implement interfaces: {}.",
                    t, interfaces.iter().map(|path| path.to_string()).collect::<Vec<_>>().join(" ")
                )
            }
            TypeCheckError::NotAssignable => {
                "Expression is not assignable.".into()
            }
        }
    }
}
