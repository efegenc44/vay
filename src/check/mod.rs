pub mod typ;

use std::collections::{HashMap, HashSet};

use crate::{
    resolution::bound::{Bound, Path},
    ast::{
        declaration::{self, Declaration},
        expression::{self, Expression},
        type_expression::{self, TypeExpression},
        pattern::Pattern
    },
    interner::{interner, InternIdx},
    lex::location::{Located, SourceLocation},
    reportable::{Reportable, ReportableResult},
    runner,
    check::typ::Type,
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
    interfaces: HashMap<Path, typ::Interface>,

    fields: HashMap<Path, HashMap<InternIdx, typ::Mono>>,
    cases: HashMap<Path, HashMap<InternIdx, Vec<typ::Mono>>>,
    methods: HashMap<Path, HashMap<InternIdx, typ::Method>>,

    defines: HashMap<Path, Located<Expression>>,

    builtin_paths: HashMap<typ::BuiltIn, Path>,

    locals: Vec<Type>,
    return_type: Vec<typ::Mono>,
    in_while: bool,

    type_var_counter: usize,

    current_source: String,

    unification_table: HashMap<usize, typ::Mono>
}

impl Checker {
    pub fn new() -> Self {
        Self {
            value_types: HashMap::new(),
            types: HashMap::new(),
            interfaces: HashMap::new(),

            fields: HashMap::new(),
            cases: HashMap::new(),
            methods: HashMap::new(),
            defines: HashMap::new(),
            builtin_paths: HashMap::new(),
            locals: vec![],
            return_type: vec![],
            in_while: false,

            // NOTE: 0 is reserved for interfaces' self reference type constant
            type_var_counter: 1,
            current_source: String::new(),
            unification_table: HashMap::new(),
        }
    }

    pub fn init_interactive_session(&mut self) {
        self.current_source = runner::SESSION_SOURCE.into();
    }

    fn newvar(&mut self) -> typ::Var {
        let idx = self.type_var_counter;
        self.type_var_counter += 1;
        typ::Var::new(idx)
    }

    fn instantiate(&mut self, t: Type) -> typ::Mono {
        match t {
            Type::Mono(m) => m,
            Type::Forall(vars, m) => {
                let map = vars
                    .iter()
                    .map(|var| {
                        let mut newvar = self.newvar();
                        *newvar.interfaces_mut() = var.interfaces().clone();
                        (var.idx(), typ::Mono::Var(newvar))
                    })
                    .collect();

                m.substitute(&map)
            },
        }
    }

    fn generalize(m: typ::Mono) -> Type {
        if matches!(m, typ::Mono::Function(_)) {
            Type::Forall(m.occuring_type_vars(), m)
        } else {
            Type::Mono(m)
        }
    }

    fn declaration_type(&mut self, m: typ::Mono, type_vars: Vec<typ::Var>) -> Type {
        if type_vars.is_empty() {
            Type::Mono(m)
        } else {
            Type::Forall(type_vars, m)
        }
    }

    fn function_type_vars(&mut self, type_vars: &[Located<declaration::TypeVar>]) -> Vec<typ::Var> {
        let mut vars = vec![];
        for var in type_vars {
            let mut newvar = self.newvar();
            let interfaces = var
                .data()
                .interfaces()
                .iter()
                .map(|interface| interface.1.clone());

            newvar.interfaces_mut().extend(interfaces);
            vars.push(newvar);
        }

        vars
    }

    fn type_type_vars(&mut self, type_vars: &[Located<InternIdx>]) -> Vec<typ::Var> {
        type_vars
            .iter()
            .map(|_| self.newvar())
            .collect()
    }

    fn type_vars_of_type(&self, type_path: &Path) -> Vec<typ::Var> {
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
    ) -> ReportableResult<typ::Function> {
        let arguments = arguments
            .iter()
            .map(|argument| self.eval_to_mono(argument.data().type_expression()))
            .collect::<ReportableResult<Vec<_>>>()?;

        let return_type = return_type
            .map(|return_type| self.eval_to_mono(return_type))
            .unwrap_or(Ok(typ::Mono::Unit))
            .map(Box::new)?;

        Ok(typ::Function::new(arguments, return_type))
    }

    fn define_type_vars(&mut self, type_vars: Vec<typ::Var>) {
        let ms = type_vars
            .into_iter()
            .map(typ::Mono::Var)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn define_type_constants(&mut self, type_vars: Vec<typ::Var>) {
        let ms = type_vars
            .into_iter()
            .map(typ::Mono::Constant)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn define_constrained_type_constants(
        &mut self,
        type_vars: Vec<typ::Var>,
        constraints: &HashMap<usize, HashSet<Path>>
    ) {
        let ms = type_vars
            .into_iter().enumerate()
            .map(|(idx, mut type_var)| {
                *type_var.interfaces_mut() = constraints
                    .get(&idx)
                    .cloned()
                    .unwrap_or(HashSet::new());

                type_var
            })
            .map(typ::Mono::Constant)
            .map(Type::Mono);

        self.locals.extend(ms);
    }

    fn declaration_instantiate(&self, type_path: &Path) -> typ::Mono {
        match self.types[type_path].clone() {
            Type::Mono(m) => m,
            Type::Forall(variables, m) => {
                let arguments = variables
                    .iter()
                    .cloned()
                    .map(typ::Mono::Var)
                    .collect();

                match m {
                    typ::Mono::Variant(path, _) => typ::Mono::Variant(path, arguments),
                    typ::Mono::Struct(path, _) => typ::Mono::Struct(path, arguments),
                    typ::Mono::BuiltIn(path, builtin, _) => typ::Mono::BuiltIn(path, builtin, arguments),
                    _ => unreachable!()
                }
            },
        }
    }

    fn constantize(t: Type) -> typ::Mono {
        match t {
            Type::Mono(m) => m,
            Type::Forall(type_vars, m) => {
                let map = type_vars
                    .iter()
                    .cloned()
                    .map(|var| (var.idx(), typ::Mono::Constant(var)))
                    .collect();

                m.substitute(&map)
            }
        }
    }

    fn replace_arguments_in_function(&self, type_path: &Path, arguments: Vec<typ::Mono>, f: typ::Function) -> typ::Mono {
        if let Type::Forall(type_variables, _) = &self.types[type_path] {
            let map = type_variables
                .iter()
                .cloned()
                .map(|variable| variable.idx())
                .zip(arguments)
                .collect();

            typ::Mono::Function(f).replace_type_constants(&map)
        } else {
            typ::Mono::Function(f)
        }
    }

    fn eval_type_expression(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<Type> {
        match type_expression.data() {
            TypeExpression::Path(type_path) => self.eval_type_path(type_path),
            TypeExpression::Function(function) => self.eval_function_type(function),
            TypeExpression::Application(type_application) => self.eval_type_application(type_application),
            TypeExpression::Unit => Ok(Type::Mono(typ::Mono::Unit)),
        }
    }

    fn eval_type_path(&mut self, type_path: &type_expression::Path) -> ReportableResult<Type> {
        match type_path.bound() {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            }
            Bound::Absolute(path) => {
                Ok(self.types[path].clone())
            }
        }
    }

    fn eval_function_type(&mut self, function_type: &type_expression::Function) -> ReportableResult<Type> {
        let arguments = function_type
            .arguments()
            .iter()
            .map(|argument| self.eval_to_mono(argument))
            .collect::<ReportableResult<Vec<_>>>()?;

        let return_type = function_type
            .return_type()
            .as_ref()
            .map(|return_type| self.eval_to_mono(return_type))
            .unwrap_or(Ok(typ::Mono::Unit))
            .map(Box::new)?;

        Ok(Type::Mono(typ::Mono::Function(typ::Function::new(arguments, return_type))))
    }

    fn eval_type_application(&mut self, type_application: &type_expression::Application) -> ReportableResult<Type> {
        let t = self.eval_type_expression(type_application.function())?;

        let Type::Forall(variables, mut m) = t else {
            return self.error(
                TypeCheckError::NotAPolyType { encountered: t },
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
            typ::Mono::Variant(_, variant_arguments) |
            typ::Mono::Struct(_, variant_arguments) |
            typ::Mono::BuiltIn(_, _, variant_arguments) => {
                for (argument, variable) in type_application.arguments().iter().zip(variables) {
                    assert!(variable.interfaces().is_empty());
                    variant_arguments.push(self.eval_to_mono(argument)?);
                }
            },

            typ::Mono::Function(_) => todo!("Explicit type application for functions"),

            typ::Mono::Constant(_) |
            typ::Mono::Var(..) |
            typ::Mono::Unit |
            typ::Mono::Bottom => unreachable!(),
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

    fn collect_variant_type(&mut self, variant: &declaration::Variant) -> ReportableResult<()> {
        let type_vars = self.type_type_vars(variant.type_vars());

        let m = typ::Mono::Variant(variant.path().clone(), vec![]);
        let t = self.declaration_type(m, type_vars);

        self.types.insert(variant.path().clone(), t);

        Ok(())
    }

    fn collect_struct_type(&mut self, strct: &declaration::Struct) -> ReportableResult<()> {
        let type_vars = self.type_type_vars(strct.type_vars());

        let m = typ::Mono::Struct(strct.path().clone(), vec![]);
        let t = self.declaration_type(m, type_vars);

        self.types.insert(strct.path().clone(), t);

        Ok(())
    }

    fn collect_interface_type(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        self.interfaces.insert(interface.path().clone(), typ::Interface::new());

        Ok(())
    }

    fn collect_builtin_type(&mut self, builtin: &declaration::BuiltIn) -> ReportableResult<()> {
        let builtin_type = match interner().get(builtin.name().data()) {
            "U64" => typ::BuiltIn::U64,
            "F32" => typ::BuiltIn::F32,
            "Char" => typ::BuiltIn::Char,
            "Array" => typ::BuiltIn::Array,
            _ => panic!("Unknown builtin")
        };

        let type_vars = self.type_type_vars(builtin.type_vars());

        let m = typ::Mono::BuiltIn(builtin.path().clone(), builtin_type, vec![]);
        let t = self.declaration_type(m, type_vars);

        self.builtin_paths.insert(builtin_type, builtin.path().clone());
        self.types.insert(builtin.path().clone(), t);

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
                Declaration::Function(function) => self.collect_function_name(function)?,
                Declaration::Define(define) => self.collect_define_name(define)?,
                _ => (),
            }
        }

        Ok(())
    }

    fn collect_function_name(&mut self, function: &declaration::Function) -> ReportableResult<()> {
        let type_vars = self.function_type_vars(function.type_vars());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            let m = typ::Mono::Function(self.get_function_type(function.arguments(), function.return_type())?);
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

    fn collect_external_name(&mut self, external: &declaration::External) -> ReportableResult<()> {
        let type_vars = self.function_type_vars(external.type_vars());

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            let m = typ::Mono::Function(self.get_function_type(external.arguments(), external.return_type())?);
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

            let type_vars = self.function_type_vars(signature.type_vars());
            self.define_type_vars(type_vars.clone());

            let function_type = self.get_function_type(signature.arguments(), signature.return_type())?;
            let method_type = typ::Method::new(function_type, constraints, type_vars);

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

            let variant_type = self.declaration_instantiate(variant.path());

            let mut variant_cases = HashMap::new();
            for case in variant.cases() {
                let case_name = *case.data().identifier().data();
                let case_path = case.data().path().clone();

                let m = if let Some(arguments) = case.data().arguments() {
                    let arguments = arguments
                        .iter()
                        .map(|argument| self.eval_to_mono(argument))
                        .collect::<ReportableResult<Vec<_>>>()?;

                    let return_type = Box::new(variant_type.clone());

                    variant_cases.insert(case_name, arguments.clone());
                    typ::Mono::Function(typ::Function::new(arguments, return_type))
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

            let struct_type = self.declaration_instantiate(strct.path());

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

            let m = typ::Mono::Function(typ::Function::new(arguments, return_type));
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
            self.method(variant.path(), method.signature().name().data(), method.body())?;
        }

        Ok(())
    }

    fn builtin(&mut self, builtin: &declaration::BuiltIn) -> ReportableResult<()> {
        for (signature, body) in builtin.methods() {
            let Some(body) = body else {
                continue;
            };

            self.method(builtin.path(), signature.name().data(), body)?;
        }

        Ok(())
    }

    fn method_instance_type(&self, type_path: &Path, constraints: &HashMap<usize, HashSet<Path>>) -> typ::Mono {
        match self.types[type_path].clone() {
            Type::Mono(m) => m,
            Type::Forall(type_vars, m) => {
                let arguments = type_vars
                    .into_iter()
                    .enumerate()
                    .map(|(idx, mut type_var)| {
                        *type_var.interfaces_mut() = constraints
                            .get(&idx)
                            .cloned()
                            .unwrap_or(HashSet::new());

                        type_var
                    })
                    .map(typ::Mono::Constant)
                    .collect();

                match m {
                    typ::Mono::Variant(path, _) => typ::Mono::Variant(path, arguments),
                    typ::Mono::Struct(path, _) => typ::Mono::Struct(path, arguments),
                    typ::Mono::BuiltIn(path, builtin, _) => typ::Mono::BuiltIn(path, builtin, arguments),
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

                        if let Some(define) = self.defines.get(path) {
                            if self.cyclic_define(define, current_define) {
                                return true;
                            }
                        }

                        false
                    }
                }
            },

            Expression::Array(array) => {
                array
                    .expressions()
                    .iter()
                    .any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Application(application) => {
                self.cyclic_define(&application.function(), current_define) ||
                application
                    .arguments()
                    .iter()
                    .any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Projection(projection) => {
                self.cyclic_define(&projection.expression(), current_define)
            },
            Expression::Let(lett) => {
                self.cyclic_define(&lett.value_expression(), current_define) ||
                self.cyclic_define(&lett.body_expression(), current_define)
            },
            Expression::Sequence(sequence) => {
                sequence
                    .expressions()
                    .iter()
                    .any(|expression| self.cyclic_define(expression, current_define))
            }
            Expression::Block(block) => {
                block
                    .expressions()
                    .iter()
                    .any(|expression| self.cyclic_define(expression, current_define))
            },
            Expression::Match(mtch) => {
                mtch
                    .expressions()
                    .iter()
                    .any(|expression| self.cyclic_define(expression, current_define)) ||
                mtch
                    .branches()
                    .iter()
                    .any(|branch| self.cyclic_define(branch.data().expression(), current_define))
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
                whilee
                    .post()
                    .as_ref()
                    .map(|expression| self.cyclic_define(expression, current_define))
                    .unwrap_or(false)
            },
        }
    }

    fn define(&mut self, define: &declaration::Define) -> ReportableResult<()> {
        if self.cyclic_define(define.expression(), define.path()) {
            return self.error(
                TypeCheckError::CyclicDefine(define.path().clone()),
                define.name().location()
            );
        }

        let Type::Mono(expected) = self.value_types[define.path()].clone() else {
            unreachable!();
        };
        self.check(define.expression(), expected)
    }

    fn function(&mut self, function: &declaration::Function) -> ReportableResult<()> {
        let t = self.value_types[function.path()].clone();
        let (arguments, return_type) = Self::constantize(t)
            .into_function()
            .destruct();

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
            self.method(strct.path(), method.signature().name().data(), method.body())?;
        }

        Ok(())
    }

    fn method(&mut self, type_path: &Path, method_name: &InternIdx, body: &Located<Expression>) -> ReportableResult<()> {
        let method_type = &self.methods[type_path][method_name];
        let (arguments, return_type) = Self::constantize(
            Type::Forall(
                method_type.type_vars().to_vec(),
                typ::Mono::Function(method_type.function_type().clone())
            )
        )
        .into_function()
        .destruct();

        let struct_type = self.method_instance_type(&type_path, method_type.constraints());

        scoped!(self, {
            self.locals.push(Type::Mono(struct_type));
            self.locals.extend(arguments.into_iter().map(Type::Mono));
            self.return_type.push(*return_type.clone());
            self.check(body, *return_type)?;
            self.return_type.pop();
        });

        Ok(())
    }

    fn collect_interface_name(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        let mut instance_type_var = typ::Var::new(INTERFACE_CONSTANT_IDX);
        instance_type_var.interfaces_mut().insert(interface.path().clone());

        let type_vars = vec![instance_type_var.clone()];

        scoped!(self, {
            self.define_type_constants(type_vars.clone());

            for method in interface.methods() {
                let function_type = self.get_function_type(method.arguments(), method.return_type())?;

                self.interfaces
                    .get_mut(interface.path())
                    .unwrap()
                    .methods_mut()
                    .insert(*method.name().data(), function_type);
            }
        });

        scoped!(self, {
            self.define_type_vars(type_vars.clone());

            for method in interface.methods() {
                let mut arguments_with_instance = vec![typ::Mono::Var(instance_type_var.clone())];
                let function_type = self.get_function_type(method.arguments(), method.return_type())?;
                arguments_with_instance.extend(function_type.arguments().to_vec());
                let function_type = typ::Function::new(
                    arguments_with_instance,
                    Box::new(function_type.return_type().clone())
                );

                let t = self.declaration_type(typ::Mono::Function(function_type), type_vars.clone());
                self.value_types.insert(method.path().clone(), t);
            }
        });

        Ok(())
    }

    fn collect_interface_name_extras(&mut self, interface: &declaration::Interface) -> ReportableResult<()> {
        let interfaces = interface
            .type_name()
            .data()
            .interfaces()
            .iter()
            .map(|interface| &interface.1)
            .map(|path| &self.interfaces[path]);

        let mut extra_methods = HashMap::new();
        for interface in interfaces {
            extra_methods.extend(interface.methods().clone());
        }

        self.interfaces
            .get_mut(interface.path())
            .unwrap()
            .methods_mut()
            .extend(extra_methods);

        Ok(())
    }

    fn matc(&mut self, matc: &expression::Match) -> ReportableResult<typ::Mono> {
        // TODO: Exhaustiveness check
        let mut ms = matc
            .expressions()
            .iter()
            .map(|expression| self.infer(expression))
            .collect::<ReportableResult<Vec<_>>>()?;

        let mut return_type = typ::Mono::Var(self.newvar());

        match &matc.branches()[..] {
            [] => return_type = typ::Mono::Unit,
            branches => {
                for branch in branches {
                    scoped!(self, {
                        if ms.len() != branch.data().patterns().len() {
                            return self.error(TypeCheckError::MatchPatternLengthMismatch {
                                    expected: ms.len(),
                                    encountered: branch.data().patterns().len()
                                },
                                branch.location()
                            );
                        }

                        for (m, pattern) in ms.iter().zip(branch.data().patterns()) {
                            if !self.match_and_define_pattern_locals(m.clone(), pattern)? {
                                // TODO: Remove push locals by type_pattern_match()
                                return self.error(
                                    TypeCheckError::NotAPatternOfType { expected: m.clone() },
                                    pattern.location(),
                                );
                            }
                        }

                        // NOTE: Bottom type is the subtype of all types
                        // TODO: Maybe self.check should return the most general type
                        if let typ::Mono::Bottom = return_type {
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

    fn match_and_define_pattern_locals(&mut self, t: typ::Mono, pattern: &Located<Pattern>) -> ReportableResult<bool> {
        match (t, pattern.data()) {
            (t, Pattern::Any(_)) => {
                self.locals.push(Type::Mono(t));
                Ok(true)
            },
            (typ::Mono::BuiltIn(_, typ::BuiltIn::U64, _), Pattern::U64(_)) |
            (typ::Mono::BuiltIn(_, typ::BuiltIn::F32, _), Pattern::F32(_)) |
            (typ::Mono::BuiltIn(_, typ::BuiltIn::Char, _), Pattern::Char(_)) => Ok(true),
            (typ::Mono::BuiltIn(_, typ::BuiltIn::Array, arg), Pattern::String(_)) => {
                Ok(matches!(arg.first().unwrap(), typ::Mono::BuiltIn(_, typ::BuiltIn::Char, _)))
            }
            (typ::Mono::BuiltIn(_, typ::BuiltIn::Array, arguments), Pattern::Array(array)) => {
                let argument = arguments.last().unwrap().clone();

                for pattern in array.before() {
                    self.match_and_define_pattern_locals(argument.clone(), pattern)?;
                }

                if array.rest().is_some() {
                    let mut type_path = Path::empty();
                    let view_type_path = "Intrinsics::ArrayView";

                    view_type_path
                        .split("::")
                        .map(|part| type_path.push(interner().intern_idx(part)))
                        .for_each(drop);

                    self.locals.push(Type::Mono(typ::Mono::Struct(type_path, arguments)));
                }

                for pattern in array.after() {
                    self.match_and_define_pattern_locals(argument.clone(), pattern)?;
                }

                Ok(true)
            },
            (typ::Mono::Variant(path, arguments), Pattern::VariantCase(variant_case)) => {
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

                // Substitute arguments
                let case_fields = if let Type::Forall(type_variables, _) = &self.types[&path] {
                    let map = type_variables
                        .iter()
                        .cloned()
                        .map(|variable| variable.idx())
                        .zip(arguments)
                        .collect();

                    case_fields.into_iter().map(|m| m.substitute(&map)).collect()
                } else {
                    case_fields
                };

                let result = case_fields
                    .into_iter()
                    .zip(fields)
                    .map(|(field, pattern)| self.match_and_define_pattern_locals(field, pattern))
                    .collect::<ReportableResult<Vec<_>>>()?
                    .iter()
                    .all(|x| *x);

                Ok(result)
            }
            (typ::Mono::Unit, Pattern::Unit)  => Ok(true),
            _ => Ok(false)
        }
    }

    fn retrn(&mut self, retrn: &expression::Return) -> ReportableResult<typ::Mono> {
        let Some(return_type) = self.return_type.last().cloned() else {
            return self.error(TypeCheckError::ReturnOutsideFunction, retrn.expression().location());
        };

        self.check(retrn.expression(), return_type)?;

        Ok(typ::Mono::Bottom)
    }

    fn check(&mut self, expression: &Located<Expression>, expected: typ::Mono) -> ReportableResult<()> {
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

    fn find_method_in_interfaces(&self, expected: &InternIdx, interfaces: &HashSet<Path>) -> Option<typ::Function> {
        for path in interfaces {
            for (name, method) in self.interfaces[path].methods() {
                if expected == name {
                    return Some(method.clone());
                }
            }
        }

        None
    }

    fn does_satisfy_constraint(&mut self, m: &typ::Mono, interfaces: &HashSet<Path>) -> bool {
        match m {
            typ::Mono::Variant(path, arguments) |
            typ::Mono::Struct(path, arguments) |
            typ::Mono::BuiltIn(path, _, arguments) => {
                for interface_path in interfaces {
                    let interface = &self.interfaces[interface_path];

                    for (name, interface_function) in interface.methods().clone() {
                        let Some(method_type) = self.methods[path].get(&name).cloned() else {
                            return false;
                        };

                        for (idx, argument) in arguments.iter().enumerate() {
                            let empty_constraint = &HashSet::new();
                            let constraint = method_type
                                .constraints()
                                .get(&idx)
                                .unwrap_or(empty_constraint);

                            if !self.does_satisfy_constraint(argument, constraint) {
                                return false;
                            }
                        }

                        let function = self.replace_arguments_in_function(
                            path,
                            arguments.clone(),
                            method_type.function_type().clone()
                        );

                        let interface_function = Self::replace_interface_constants(
                            typ::Mono::Function(interface_function.clone()),
                            m.clone()
                        );

                        if !self.unify(function, interface_function) {
                            return false;
                        }
                    }
                }

                true
            },
            typ::Mono::Var(type_var) | typ::Mono::Constant(type_var) => {
                for path in interfaces {
                    let interface = &self.interfaces[path];
                    for (name, interface_function) in interface.methods() {
                        let Some(variable_function) = self.find_method_in_interfaces(name, &type_var.interfaces()) else {
                            return false;
                        };

                        let variable_function = Self::replace_interface_constants(
                            typ::Mono::Function(variable_function.clone()),
                            m.clone()
                        ).into_function();

                        let interface_function = Self::replace_interface_constants(
                            typ::Mono::Function(interface_function.clone()),
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

    fn replace_interface_constants(m: typ::Mono, instance: typ::Mono) -> typ::Mono {
        let map = HashMap::from([(INTERFACE_CONSTANT_IDX, instance)]);
        m.replace_type_constants(&map)
    }

    pub fn infer(&mut self, expression: &Located<Expression>) -> ReportableResult<typ::Mono> {
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
            Expression::Continue => self.continuee(expression.location()),
            Expression::Break => self.breakk(expression.location())
        }
    }

    fn u64(&self) -> ReportableResult<typ::Mono> {
        let m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::U64].clone(),
            typ::BuiltIn::U64,
            vec![]
        );

        Ok(m)
    }

    fn f32(&self) -> ReportableResult<typ::Mono> {
        let m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::F32].clone(),
            typ::BuiltIn::F32,
            vec![]
        );

        Ok(m)
    }

    fn string(&self) -> ReportableResult<typ::Mono> {
        let character_m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::Char].clone(),
            typ::BuiltIn::Char,
            vec![]
        );

        let m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::Array].clone(),
            typ::BuiltIn::Array,
            vec![character_m]
        );

        Ok(m)
    }

    fn char(&self) -> ReportableResult<typ::Mono> {
        let m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::Char].clone(),
            typ::BuiltIn::Char,
            vec![]
        );

        Ok(m)
    }

    // NOTE: bool indicates if the path is assignable
    fn path(&mut self, path: &expression::Path) -> ReportableResult<(typ::Mono, bool)> {
        match path.bound() {
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                let local = self.locals[index].clone();
                // NOTE: Lazily substitute locals
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
        }
    }

    fn array(&mut self, array: &expression::Array) -> ReportableResult<typ::Mono> {
        let mut inner_type = typ::Mono::Var(self.newvar());

        for expression in array.expressions() {
            self.check(expression, inner_type.clone())?;
            inner_type = inner_type.substitute(&self.unification_table);
        }

        let m = typ::Mono::BuiltIn(
            self.builtin_paths[&typ::BuiltIn::Array].clone(),
            typ::BuiltIn::Array,
            vec![inner_type]
        );

        Ok(m)
    }

    fn application(&mut self, application: &expression::Application) -> ReportableResult<typ::Mono> {
        let m = self.infer(application.function())?;
        let typ::Mono::Function(function_type) = m else {
            return self.error(
                TypeCheckError::ExpectedAFunction { encountered: m },
                application.function().location()
            );
        };

        let (expected_types, return_type) = function_type.destruct();

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
    fn projection(&mut self, projection: &expression::Projection) -> ReportableResult<(typ::Mono, bool)> {
        let m = self.infer(projection.expression())?;

        // Field Projection (Only structs)
        // TODO: Notice that fields have priority over methods,
        //   maybe a field and a method with same name should be an error
        if let typ::Mono::Struct(path, arguments) = &m {
            if let Some(m) = self.fields[path].get(projection.projected().data()).cloned() {
                // Substitute arguments
                let m = if let Type::Forall(type_variables, _) = &self.types[path] {
                    let map = type_variables
                        .iter()
                        .cloned()
                        .map(|variable| variable.idx())
                        .zip(arguments.clone())
                        .collect();

                    m.substitute(&map)
                } else {
                    m
                };

                return Ok((m, true))
            }
        };

        // Method Projection
        match &m {
            typ::Mono::Variant(path, arguments) |
            typ::Mono::Struct(path, arguments) |
            typ::Mono::BuiltIn(path, _, arguments) => {
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
                    let constraint = method_type
                        .constraints()
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

                let m = self.replace_arguments_in_function(
                    path,
                    arguments.clone(),
                    method_type.function_type().clone()
                );

                let t = Type::Forall(method_type.type_vars().to_vec(), m);
                let m = self.instantiate(t);

                Ok((m, false))
            },
            typ::Mono::Constant(type_var) | typ::Mono::Var(type_var) => {
                let Some(variable_function) = self.find_method_in_interfaces(projection.projected().data(), &type_var.interfaces()) else {
                    return self.error(
                        TypeCheckError::NotProjectable {
                            ty: m,
                            name: *projection.projected().data()
                        },
                        projection.projected().location()
                    );
                };

                let variable_function = Self::replace_interface_constants(
                    typ::Mono::Function(variable_function.clone()), m
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

    fn lett(&mut self, lett: &expression::Let) -> ReportableResult<typ::Mono> {
        let m = self.infer(lett.value_expression())?;
        let t = Self::generalize(m);

        let return_type;
        scoped!(self, {
            self.locals.push(t);
            return_type = self.infer(lett.body_expression())?;
        });

        Ok(return_type)
    }

    fn sequence(&mut self, sequence: &expression::Sequence) -> ReportableResult<typ::Mono> {
        match &sequence.expressions()[..] {
            [] => Ok(typ::Mono::Unit),
            [init@.., last] => {
                for expression in init {
                    self.infer(expression)?;
                }

                self.infer(last)
            }
        }
    }

    fn block(&mut self, block: &expression::Block) -> ReportableResult<typ::Mono> {
        for expression in block.expressions() {
            self.infer(expression)?;
        }

        Ok(typ::Mono::Unit)
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> ReportableResult<typ::Mono> {
        let return_type = typ::Mono::Var(self.newvar());
        let variables = lambda
            .arguments()
            .iter()
            .map(|_| self.newvar())
            .collect::<Vec<_>>();

        scoped!(self, {
            self.define_type_vars(variables.clone());

            self.return_type.push(return_type.clone());
            self.check(lambda.body(), return_type.clone())?;
            self.return_type.pop();
        });

        let arguments = variables
            .into_iter()
            .map(typ::Mono::Var)
            .map(|variable| variable.substitute(&self.unification_table))
            .collect::<Vec<_>>();

        let return_type = Box::new(return_type.substitute(&self.unification_table));

        let function_type = typ::Function::new(arguments, return_type);
        Ok(typ::Mono::Function(function_type))
    }

    fn assignment(&mut self, assignment: &expression::Assignment) -> ReportableResult<typ::Mono> {
        let (assignable_type, is_assignable) = match assignment.assignable().data() {
            Expression::Path(path) => self.path(path)?,
            Expression::Projection(projection) => self.projection(projection)?,
            _ => return self.error(TypeCheckError::NotAssignable, assignment.assignable().location())
        };

        if !is_assignable {
            return self.error(TypeCheckError::NotAssignable, assignment.assignable().location());
        }

        self.check(assignment.expression(), assignable_type)?;

        Ok(typ::Mono::Unit)
    }

    fn whilee(&mut self, whilee: &expression::While) -> ReportableResult<typ::Mono> {
        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let boole = typ::Mono::Variant(type_path, vec![]);

        self.check(whilee.condition(), boole)?;

        if let Some(post) = whilee.post() {
            self.infer(post)?;
        }

        self.in_while = true;
        self.infer(whilee.body())?;
        self.in_while = false;

        Ok(typ::Mono::Unit)
    }

    fn continuee(&self, location: SourceLocation) -> ReportableResult<typ::Mono> {
        if self.in_while {
            Ok(typ::Mono::Bottom)
        } else {
            self.error(TypeCheckError::ContinueOutsideFunction, location)
        }
    }

    fn breakk(&self, location: SourceLocation) -> ReportableResult<typ::Mono> {
        if self.in_while {
            Ok(typ::Mono::Bottom)
        } else {
            self.error(TypeCheckError::BreakOutsideFunction, location)
        }
    }

    // TODO: unify should return Result for better error reporting
    // TODO: How inference is done is much like Algorithm W,
    //   at some point Algorithm J like inference would be better
    fn unify(&mut self, a: typ::Mono, b: typ::Mono) -> bool {
        match (a, b) {
            (typ::Mono::Variant(path1, args1), typ::Mono::Variant(path2, args2)) |
            (typ::Mono::Struct(path1, args1), typ::Mono::Struct(path2, args2)) |
            (typ::Mono::BuiltIn(path1, _, args1), typ::Mono::BuiltIn(path2, _, args2)) => {
                path1 == path2 && args1
                    .into_iter()
                    .zip(args2)
                    .all(|(arg1, arg2)| self.unify(arg1, arg2))
            },
            (typ::Mono::Function(function1), typ::Mono::Function(function2)) => {
                let (args1, return1) = function1.destruct();
                let (args2, return2) = function2.destruct();

                args1
                    .into_iter()
                    .zip(args2)
                    .all(|(arg1, arg2)| self.unify(arg1, arg2))

                && self.unify(*return1, *return2)
            },
            (typ::Mono::Var(var1), typ::Mono::Var(var2)) => {
                match (self.unification_table.get(&var1.idx()).cloned(), self.unification_table.get(&var2.idx()).cloned()) {
                    (None, Some(m)) => self.unify(typ::Mono::Var(var1), m),
                    (Some(m), None) => self.unify(m, typ::Mono::Var(var2)),
                    (Some(m1), Some(m2)) => self.unify(m1, m2),
                    (None, None) => {
                        let mut newvar = self.newvar();
                        *newvar.interfaces_mut() = var1.interfaces()
                            .union(var2.interfaces())
                            .cloned()
                            .collect();

                        self.unification_table.insert(var1.idx(), typ::Mono::Var(newvar.clone()));
                        self.unification_table.insert(var2.idx(), typ::Mono::Var(newvar));

                        true
                    },
                }
            },
            (t, typ::Mono::Var(var)) | (typ::Mono::Var(var), t) => {
                if t.occurs(var.idx()) {
                    return false;
                }

                match self.unification_table.get(&var.idx()).cloned() {
                    Some(m) => self.unify(m, t),
                    None => {
                        if !self.does_satisfy_constraint(&t, var.interfaces()) {
                            return false;
                        }
                        // NOTE: Here t cannot be a MonoType::Var
                        //   so they cannot be equal
                        self.unification_table.insert(var.idx(), t);
                        true
                    }
                }
            },
            // NOTE: Bottom type is the subtype of all types
            // NOTE: Here _ cannot be a MonoType::Var
            (typ::Mono::Bottom, _) | (_, typ::Mono::Bottom) => true,
            (a, b) => a == b
        }
    }

    fn eval_to_mono(&mut self, type_expression: &Located<TypeExpression>) -> ReportableResult<typ::Mono> {
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
        encountered: typ::Mono,
        expected: typ::Mono,
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
        expected: typ::Mono,
    },
    ExpectedAFunction {
        encountered: typ::Mono,
    },
    ArityMismatch {
        expected: usize,
        encountered: usize,
    },
    NotProjectable {
        ty: typ::Mono,
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
        t: typ::Mono,
        interfaces: HashSet<Path>,
    },
    NotAssignable,
    CyclicDefine(Path),
    MatchPatternLengthMismatch {
        expected: usize,
        encountered: usize,
    },
    ReturnOutsideFunction,
    ContinueOutsideFunction,
    BreakOutsideFunction,
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
            TypeCheckError::CyclicDefine(path) => {
                format!(
                    "`{}` is defined in a cycle. Define declarations evaluated eagerly hence they can not be defined in a cycle.",
                    path
                )
            }
            TypeCheckError::MatchPatternLengthMismatch { expected, encountered } => {
                format!("Expected `{}` patterns in branch but encountered `{}`", expected, encountered)
            }
            TypeCheckError::ReturnOutsideFunction => {
                "Return expression outside of a function is not permitted.".into()
            }
            TypeCheckError::ContinueOutsideFunction => {
                "Continue expression outside of a while expression is not permitted.".into()
            }
            TypeCheckError::BreakOutsideFunction => {
                "Break expression outside of a while expression is not permitted.".into()
            }
        }
    }
}
