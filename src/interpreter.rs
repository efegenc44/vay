use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bound::{Bound, Path},
    declaration::{
        BuiltInDeclaration, Declaration, ExternalDeclaration, FunctionDeclaration,
        InterfaceDeclaration, InterfaceMethodSignature, MethodDeclaration,
        MethodSignature, Module, StructDeclaration, VariantDeclaration,
        DefineDeclaration
    },
    expression,
    expression::{
        Expression,
        pattern::Pattern,
    },
    interner::{interner, InternIdx},
    intrinsics::{EXTERNAL_FUNCTIONS, INTRINSIC_FUNCTIONS},
    location::Located,
    typ::BuiltInType,
    value::{
        ConstructorInstance, FunctionInstance, InstanceInstance, LambdaInstance,
        MethodInstance, StructConstructorInstance, StructInstanceInstance, Value,
        BuiltInMethodKind
    }
};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, Rc<FunctionInstance>>>,
    builtin_methods: HashMap<BuiltInType, HashMap<InternIdx, BuiltInMethodKind>>,
    names: HashMap<Path, Value>,
    locals: Vec<Value>,

    defines: HashMap<Path, Located<Expression>>,
    collecting_define: bool
}

macro_rules! scoped {
    ($self:expr, $body:block) => {
        {
            let locals_len = $self.locals.len();
            $body
            $self.locals.truncate(locals_len);
        }
    };
}

pub enum FlowException {
    Break,
    Continue,
    Return(Value),
}

pub type ControlFlow = Result<Value, FlowException>;

impl Interpreter {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            builtin_methods: HashMap::new(),
            names: HashMap::new(),
            locals: vec![],

            defines: HashMap::new(),
            collecting_define: false,
        }
    }

    pub fn evaluate_main(&mut self, modules: &[Module]) {
        for module in modules {
            self.collect_names(module);
        }

        let mut main_module = None;

        for module in modules {
            if module.path().to_string() == "Main" {
                main_module = Some(module);
                break;
            }
        }
        let Some(main_module) = main_module else {
            todo!("Main module is not declared");
        };

        let mut main_function = None;
        'outer: for declaration in main_module.declarations() {
            if let Declaration::Function(function) = declaration {
                if interner().get(function.name.data()) == "main" {
                    main_function = Some(declaration);
                    break 'outer;
                }
            }
        }
        let Some(main_function) = main_function else {
            todo!("main function is not declared");
        };

        let Declaration::Function(function) = main_function else {
            unreachable!();
        };

        if !function.arguments.is_empty() {
            todo!("main function is not supposed to take any arguments");
        }

        let value = self.expression(&function.body);
        let value = match value {
            Ok(value) | Err(FlowException::Return(value)) => value,
            _ => unreachable!()
        };

        println!("\nResult = {}", value);
    }

    pub fn collect_names(&mut self, module: &Module) {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Module(..) => (),
                Declaration::Import(..) => (),
                Declaration::Define(define) => {
                    self.defines.insert(define.path.clone(), define.expression.clone());
                },
                Declaration::Interface(interface) => self.collect_interface_name(interface),
                Declaration::Function(function) => self.collect_function_name(function),
                Declaration::Variant(variant) => self.collect_variant_name(variant),
                Declaration::Struct(strct) => self.collect_struct_name(strct),
                Declaration::BuiltIn(builtin) => self.collect_builtin_name(builtin),
                Declaration::External(external) => self.collect_external_name(external),
            }
        }

        for declaration in module.declarations() {
            if let Declaration::Define(define) = declaration {
                self.collect_define_name(define);
            }
        }
    }

    fn collect_function_name(&mut self, function: &FunctionDeclaration) {
        let FunctionDeclaration { body, path, .. } = function;

        let function = FunctionInstance { body: body.clone() };
        let value = Value::Function(Rc::new(function));

        self.names.insert(path.clone(), value);
    }

    fn collect_define_name(&mut self, define: &DefineDeclaration) {
        let DefineDeclaration { expression, path, .. } = define;

        self.collecting_define = true;
        let Ok(value) = self.expression(expression) else {
            panic!();
        };
        self.collecting_define = false;

        self.names.insert(path.clone(), value);
    }

    fn collect_method_name(&mut self, path: &Path, method: &MethodDeclaration) {
        let MethodDeclaration { signature, body, .. } = method;

        let function = FunctionInstance { body: body.clone() };
        self.methods.get_mut(path).unwrap().insert(*signature.name.data(), Rc::new(function));
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        for case in cases {
            let constructor = ConstructorInstance {
                type_path: path.clone(),
                case: *case.data().identifier().data(),
            };

            let value = match case.data().arguments() {
                Some(_) => Value::Constructor(Rc::new(constructor)),
                None => {
                    let instance = InstanceInstance {
                        constructor: Rc::new(constructor),
                        values: vec![],
                    };
                    Value::Instance(Rc::new(instance))
                },
            };

            self.names.insert(case.data().path().clone(), value);
        }

        self.methods.insert(path.clone(), HashMap::new());
        for method in methods {
            self.collect_method_name(path, method);
        }
    }

    fn collect_struct_name(&mut self, strct: &StructDeclaration) {
        let StructDeclaration { methods, path, fields, .. } = strct;

        let fields = fields
            .iter().map(|field| *field.data().indentifier().data())
            .collect();
        let constructor = StructConstructorInstance { type_path: path.clone(), fields };
        self.names.insert(path.clone(), Value::StructConstructor(Rc::new(constructor)));

        self.methods.insert(path.clone(), HashMap::new());
        for method in methods {
            self.collect_method_name(path, method);
        }
    }

    fn collect_interface_name(&mut self, interface: &InterfaceDeclaration) {
        let InterfaceDeclaration { methods, .. } = interface;

        for method in methods {
            let InterfaceMethodSignature { path, name, .. } = method;

            let function = Value::InterfaceFunction(*name.data());
            self.names.insert(path.clone(), function);
        }
    }

    fn collect_builtin_name(&mut self, builtin: &BuiltInDeclaration) {
        let BuiltInDeclaration { name, methods, path, ..  } = builtin;

        let t = match interner().get(name.data()) {
            "U64" => BuiltInType::U64,
            "F32" => BuiltInType::F32,
            "Char" => BuiltInType::Char,
            "Array" => BuiltInType::Array,
            _ => unreachable!()
        };

        self.builtin_methods.insert(t, HashMap::new());
        for (signature, body) in methods {
            let MethodSignature { name, .. } = signature;

            if let Some(body) = body {
                let f = Rc::new(FunctionInstance { body: body.clone() });
                self.builtin_methods.get_mut(&t).unwrap().insert(*name.data(), BuiltInMethodKind::Normal(f));
            } else {
                let mpath = path.append(*name.data());

                // TODO: Better error reporting here
                let f = INTRINSIC_FUNCTIONS
                    .iter().find(|(ppath, _)| ppath == &mpath.to_string())
                    .unwrap().1;

                self.builtin_methods.get_mut(&t).unwrap().insert(*name.data(), BuiltInMethodKind::Intrinsic(f));
            }
        }
    }

    fn collect_external_name(&mut self, external: &ExternalDeclaration) {
        let ExternalDeclaration { path, .. } = external;

        // TODO: Better error reporting here
        // TODO: External functions via dynamic loading?
        let f = EXTERNAL_FUNCTIONS
            .iter().find(|(ppath, _)| ppath == &path.to_string())
            .unwrap().1;

        let function = Value::ExternalFunction(f);
        self.names.insert(path.clone(), function);
    }

    fn matc(&mut self, matc: &expression::Match) -> ControlFlow {
        let mut values = vec![];
        for expression in matc.expressions() {
            values.push(self.expression(expression)?)
        }

        for branch in matc.branches() {
            if values.iter().zip(branch.data().patterns()).all(|(v, p)| v.matches(p.data())) {
                let return_value;
                scoped!(self, {
                    for (value, pattern) in values.iter().zip(branch.data().patterns()) {
                        self.value_pattern_match(value, pattern);
                    }
                    return_value = self.expression(branch.data().expression());
                });
                return return_value;
            }
        }

        todo!("Unexhaustive pattern matching")
    }

    fn value_pattern_match(&mut self, value: &Value, pattern: &Located<Pattern>) {
        match (value, pattern.data()) {
            (value, Pattern::Any(_)) => {
                self.locals.push(value.clone());
            }
            (Value::U64(_), Pattern::U64(_)) |
            (Value::F32(_), Pattern::F32(_)) |
            (Value::Array(_), Pattern::String(_)) |
            (Value::Char(_), Pattern::Char(_)) => (),
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let InstanceInstance { values, .. } = instance.as_ref();

                let empty_field = vec![];
                let fields = variant_case.fields().unwrap_or(&empty_field);

                for (value, field) in values.iter().zip(fields) {
                    self.value_pattern_match(value, field);
                }
            }
            (Value::Array(array), Pattern::Array(pattern)) => {
                for (value, pattern) in array.borrow().iter().zip(pattern.before()) {
                    self.value_pattern_match(value, pattern);
                }

                if pattern.rest().is_some() {
                    let mut type_path = Path::empty();
                    let view_type_path = "Intrinsics::ArrayView";

                    view_type_path
                        .split("::")
                        .map(|part| type_path.push(interner().intern_idx(part)))
                        .for_each(drop);

                    let fields = HashMap::from([
                        (interner().intern_idx("array"), Value::Array(array.clone())),
                        (interner().intern_idx("start"), Value::U64(pattern.before().len() as u64)),
                        (interner().intern_idx("length"), Value::U64((array.borrow().len() - pattern.after().len() - pattern.before().len()) as u64)),
                    ]);
                    let fields = RefCell::new(fields);

                    let instance = StructInstanceInstance { type_path, fields };

                    let rest_view = Value::StructInstance(Rc::new(instance));
                    self.locals.push(rest_view);
                }

                for (value, pattern) in array.borrow().iter().rev().zip(pattern.after().iter().rev()) {
                    self.value_pattern_match(value, pattern);
                }
            }
            (Value::Unit, Pattern::Unit) => (),
            _ => unreachable!(),
        }
    }

    fn retrn(&mut self, retrn: &expression::Return) -> ControlFlow {
        let value = self.expression(retrn.expression())?;
        Err(FlowException::Return(value))
    }

    pub fn expression(&mut self, expression: &Located<Expression>) -> ControlFlow {
        match expression.data() {
            Expression::U64(u64) => Ok(Value::U64(*u64)),
            Expression::F32(f32) => Ok(Value::F32(*f32)),
            Expression::String(string_idx) => Ok(Value::Array(
                Rc::new(RefCell::new(interner().get(string_idx).chars().map(|ch| Value::Char(ch)).collect::<Vec<_>>()))
            )),
            Expression::Char(ch) => Ok(Value::Char(*ch)),
            Expression::Path(path) => self.path(path),
            Expression::Array(array) => self.array(array),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
            Expression::Let(lett) => self.lett(lett),
            Expression::Sequence(sequence) => self.sequence(sequence),
            Expression::Block(block) => self.block(block),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Match(matc) => self.matc(matc),
            Expression::Return(retrn) => self.retrn(retrn),
            Expression::Assignment(assignment) => self.assignment(assignment),
            Expression::While(whilee) => self.whilee(whilee),
            Expression::Continue => self.continuee(),
            Expression::Break => self.breakk(),
        }
    }

    fn path(&mut self, path: &expression::Path) -> ControlFlow {
        match path.bound() {
            Bound::Undetermined => unreachable!(),
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            },
            Bound::Absolute(path) => {
                if self.collecting_define {
                    if self.names.contains_key(path) {
                        Ok(self.names[path].clone())
                    } else {
                        let define = &self.defines[path].clone();
                        let value = self.expression(define)?;
                        self.names.insert(path.clone(), value.clone());
                        Ok(value)
                    }
                } else {
                    Ok(self.names[path].clone())
                }
            },
        }
    }

    fn array(&mut self, array: &expression::Array) -> ControlFlow {
        let mut values = vec![];
        for expression in array.expressions() {
            values.push(self.expression(expression)?);
        }

        Ok(Value::Array(Rc::new(RefCell::new(values))))
    }

    // TODO: Abstract application on Value and use it here
    fn application(&mut self, application: &expression::Application) -> ControlFlow {
        match self.expression(application.function())? {
            Value::Function(function) => {
                let FunctionInstance { body } = function.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in application.arguments() {
                        let argument = self.expression(argument)?;
                        argument_values.push(argument);
                    }
                    self.locals.extend(argument_values);

                    return_value = self.expression(body);
                });

                match return_value {
                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                    _ => unreachable!()
                }
            },
            Value::Method(method) => {
                let MethodInstance { instance, function } = method.as_ref();
                let FunctionInstance { body } = function.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in application.arguments() {
                        argument_values.push(self.expression(argument)?);
                    }
                    self.locals.push(instance.clone());
                    self.locals.extend(argument_values);

                    return_value = self.expression(body);
                });

                match return_value {
                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                    _ => unreachable!()
                }
            },
            Value::Constructor(constructor) => {
                let mut values = vec![];
                for argument in application.arguments() {
                    values.push(self.expression(argument)?);
                }

                let instance = InstanceInstance { constructor, values };
                Ok(Value::Instance(Rc::new(instance)))
            },
            Value::Lambda(lambda) => {
                let LambdaInstance { capture, body } = lambda.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in application.arguments() {
                        argument_values.push(self.expression(argument)?);
                    }

                    self.locals.extend(capture.clone());
                    self.locals.extend(argument_values);

                    return_value = self.expression(body);
                });

                match return_value {
                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                    _ => unreachable!()
                }
            }
            Value::InterfaceFunction(name) => {
                let instance = self.expression(&application.arguments()[0])?.clone();
                // TODO: Abstract projection on Value and use it here
                let method = match &instance {
                    Value::Instance(value) => {
                        let InstanceInstance { constructor, .. } = value.as_ref();
                        let ConstructorInstance { type_path, .. } = constructor.as_ref();

                        self.methods[type_path][&name].clone()
                    },
                    Value::StructInstance(value) => {
                        let StructInstanceInstance { type_path, .. } = value.as_ref();

                        self.methods[type_path][&name].clone()
                    },
                    Value::U64(u64) => {
                        let f = self.builtin_methods[&BuiltInType::U64][&name].clone();

                        let mut argument_values = vec![Value::U64(*u64)];
                        for argument in application.arguments() {
                            argument_values.push(self.expression(argument)?);
                        }

                        match f {
                            BuiltInMethodKind::Intrinsic(f) => {
                                return Ok(f(argument_values))
                            },
                            BuiltInMethodKind::Normal(f) => {
                                let return_value;
                                scoped!(self, {
                                    let mut argument_values = vec![];
                                    for argument in application.arguments() {
                                        argument_values.push(self.expression(argument)?);
                                    }
                                    self.locals.push(instance.clone());
                                    self.locals.extend(argument_values);

                                    return_value = self.expression(&f.body);
                                });

                                return match return_value {
                                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                                    _ => unreachable!()
                                }
                            },
                        }
                    },
                    Value::F32(f32) => {
                        let f = self.builtin_methods[&BuiltInType::F32][&name].clone();

                        let mut argument_values = vec![Value::F32(*f32)];
                        for argument in application.arguments() {
                            argument_values.push(self.expression(argument)?);
                        }

                        match f {
                            BuiltInMethodKind::Intrinsic(f) => {
                                return Ok(f(argument_values))
                            },
                            BuiltInMethodKind::Normal(f) => {
                                let return_value;
                                scoped!(self, {
                                    let mut argument_values = vec![];
                                    for argument in application.arguments() {
                                        argument_values.push(self.expression(argument)?);
                                    }
                                    self.locals.push(instance.clone());
                                    self.locals.extend(argument_values);

                                    return_value = self.expression(&f.body);
                                });

                                return match return_value {
                                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                                    _ => unreachable!()
                                }
                            },
                        }
                    },
                    Value::Char(ch) => {
                        let f = self.builtin_methods[&BuiltInType::Char][&name].clone();

                        let mut argument_values = vec![Value::Char(*ch)];
                        for argument in application.arguments() {
                            argument_values.push(self.expression(argument)?);
                        }

                        match f {
                            BuiltInMethodKind::Intrinsic(f) => {
                                return Ok(f(argument_values))
                            },
                            BuiltInMethodKind::Normal(f) => {
                                let return_value;
                                scoped!(self, {
                                    let mut argument_values = vec![];
                                    for argument in application.arguments() {
                                        argument_values.push(self.expression(argument)?);
                                    }
                                    self.locals.push(instance.clone());
                                    self.locals.extend(argument_values);

                                    return_value = self.expression(&f.body);
                                });

                                return match return_value {
                                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                                    _ => unreachable!()
                                }
                            },
                        }
                    },
                    Value::Array(array) => {
                        let f = self.builtin_methods[&BuiltInType::Array][&name].clone();

                        let mut argument_values = vec![Value::Array(array.clone())];
                        for argument in application.arguments() {
                            argument_values.push(self.expression(argument)?);
                        }

                        match f {
                            BuiltInMethodKind::Intrinsic(f) => {
                                return Ok(f(argument_values))
                            },
                            BuiltInMethodKind::Normal(f) => {
                                let return_value;
                                scoped!(self, {
                                    let mut argument_values = vec![];
                                    for argument in application.arguments() {
                                        argument_values.push(self.expression(argument)?);
                                    }
                                    self.locals.push(instance.clone());
                                    self.locals.extend(argument_values);

                                    return_value = self.expression(&f.body);
                                });

                                return match return_value {
                                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                                    _ => unreachable!()
                                }
                            },
                        }
                    },
                    _ => { unreachable!(); }
                };

                let FunctionInstance { body } = method.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in application.arguments() {
                        argument_values.push(self.expression(argument)?);
                    }
                    self.locals.push(instance.clone());
                    self.locals.extend(argument_values);

                    return_value = self.expression(body);
                });

                match return_value {
                    Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                    _ => unreachable!()
                }
            }
            Value::StructConstructor(constructor) => {
                let StructConstructorInstance { fields, type_path } = constructor.as_ref();

                let mut values = HashMap::new();
                for (argument, field_name) in application.arguments().iter().zip(fields.iter()) {
                    values.insert(*field_name, self.expression(argument)?);
                }

                let instance = StructInstanceInstance {
                    type_path: type_path.clone(),
                    fields: RefCell::new(values)
                };
                Ok(Value::StructInstance(Rc::new(instance)))
            },
            Value::BuiltinMethod(v, f) => {
                let mut argument_values = vec![*v];
                for argument in application.arguments() {
                    argument_values.push(self.expression(argument)?);
                }

                match f {
                    BuiltInMethodKind::Intrinsic(f) => {
                        return Ok(f(argument_values))
                    },
                    BuiltInMethodKind::Normal(f) => {
                        let return_value;
                        scoped!(self, {
                            let mut argument_values = vec![];
                            for argument in application.arguments() {
                                argument_values.push(self.expression(argument)?);
                            }
                            self.locals.extend(argument_values);

                            return_value = self.expression(&f.body);
                        });

                        return match return_value {
                            Ok(value) | Err(FlowException::Return(value)) => Ok(value),
                            _ => unreachable!()
                        }
                    },
                }
            }
            Value::ExternalFunction(f) => {
                let mut argument_values = vec![];
                for argument in application.arguments() {
                    argument_values.push(self.expression(argument)?);
                }

                Ok(f(argument_values))
            }

            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &expression::Projection) -> ControlFlow {
        let instance = self.expression(projection.expression())?;

        match &instance {
            Value::Instance(instanceinstance) => {
                let InstanceInstance { constructor, .. } = instanceinstance.as_ref();
                let ConstructorInstance { type_path, .. } = constructor.as_ref();

                let function = self.methods[type_path][projection.projected().data()].clone();
                let method = MethodInstance { instance, function };
                Ok(Value::Method(Rc::new(method)))
            }
            Value::StructInstance(structinstanceinstance) => {
                let StructInstanceInstance { type_path, fields } = structinstanceinstance.as_ref();

                if fields.borrow().contains_key(projection.projected().data()) {
                    Ok(fields.borrow().get(projection.projected().data()).unwrap().clone())
                } else {
                    let function = self.methods[type_path][projection.projected().data()].clone();
                    let method = MethodInstance { instance, function };
                    Ok(Value::Method(Rc::new(method)))
                }
            },
            Value::U64(i64) => {
                let function = self.builtin_methods[&BuiltInType::U64][projection.projected().data()].clone();
                Ok(Value::BuiltinMethod(Box::new(Value::U64(*i64)), function))
            }
            Value::F32(f32) => {
                let function = self.builtin_methods[&BuiltInType::F32][projection.projected().data()].clone();
                Ok(Value::BuiltinMethod(Box::new(Value::F32(*f32)), function))
            }
            Value::Char(ch) => {
                let function = self.builtin_methods[&BuiltInType::Char][projection.projected().data()].clone();
                Ok(Value::BuiltinMethod(Box::new(Value::Char(*ch)), function))
            }
            Value::Array(array) => {
                let function = self.builtin_methods[&BuiltInType::Array][projection.projected().data()].clone();
                Ok(Value::BuiltinMethod(Box::new(Value::Array(array.clone())), function))
            }
            _ => unreachable!(),
        }
    }

    fn lett(&mut self, lett: &expression::Let) -> ControlFlow {
        let value = self.expression(lett.value_expression())?;

        let return_value;
        scoped!(self, {
            self.locals.push(value);
            return_value = self.expression(lett.body_expression())?;
        });

        Ok(return_value)
    }

    fn sequence(&mut self, sequence: &expression::Sequence) -> ControlFlow {
        match &sequence.expressions()[..] {
            [] => Ok(Value::Unit),
            [init@.., last] => {
                for expression in init {
                    self.expression(expression)?;
                }

                self.expression(last)
            }
        }
    }

    fn block(&mut self, block: &expression::Block) -> ControlFlow {
        for expression in block.expressions() {
            self.expression(expression)?;
        }

        Ok(Value::Unit)
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> ControlFlow {
        let capture = self.locals.clone();
        let body = lambda.body().clone();
        let lambda = LambdaInstance { capture, body };

        Ok(Value::Lambda(Rc::new(lambda)))
    }

    fn assignment(&mut self, assignment: &expression::Assignment) -> ControlFlow {
        let value = self.expression(assignment.expression())?;
        match assignment.assignable().data() {
            Expression::Path(path) => {
                match path.bound() {
                    Bound::Local(idx) => {
                        let index = self.locals.len() - 1 - idx;
                        self.locals[index] = value
                    },
                    Bound::Absolute(_) |
                    Bound::Undetermined => unreachable!(),
                }
            },
            Expression::Projection(projection) => {
                let Value::StructInstance(instance) = self.expression(projection.expression())? else {
                    unreachable!()
                };

                let StructInstanceInstance { fields, .. } = instance.as_ref();
                *fields.borrow_mut().get_mut(projection.projected().data()).unwrap() = value;
            },
            _ => unreachable!()
        }

        Ok(Value::Unit)
    }

    fn whilee(&mut self, whilee: &expression::While) -> ControlFlow {
        let mut result = self.expression(whilee.condition())?.into_core_bool();
        while result {
            let body_result = self.expression(whilee.body());

            if let Some(post) = whilee.post() {
                self.expression(post)?;
            }

            match body_result {
                Err(FlowException::Break) => break,
                Err(FlowException::Continue) => {
                    result = self.expression(whilee.condition())?.into_core_bool();
                    continue
                },
                Err(FlowException::Return(value)) => return Err(FlowException::Return(value)),
                Ok(_) => result = self.expression(whilee.condition())?.into_core_bool()
            }
        }

        Ok(Value::Unit)
    }

    fn continuee(&mut self) -> ControlFlow {
        Err(FlowException::Continue)
    }

    fn breakk(&mut self) -> ControlFlow {
        Err(FlowException::Break)
    }
}