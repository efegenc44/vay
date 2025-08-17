use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    bound::{Bound, Path},
    declaration::{
        BuiltInDeclaration, Declaration, ExternalDeclaration, FunctionDeclaration,
        InterfaceDeclaration, InterfaceMethodSignature, MethodDeclaration,
        MethodSignature, Module, StructDeclaration, VariantDeclaration,
        DefineDeclaration
    },
    expression::{
        ApplicationExpression, ArrayExpression, ArrayPattern, AssignmentExpression,
        Expression, LambdaExpression, LetExpression, MatchExpression,
        PathExpression, Pattern, ProjectionExpression, ReturnExpression,
        SequenceExpression, VariantCasePattern, WhileExpression, BlockExpression
    },
    interner::{interner, InternIdx},
    intrinsics::{IntrinsicFunction, EXTERNAL_FUNCTIONS, INTRINSIC_FUNCTIONS},
    location::Located,
    typ::BuiltInType,
    value::{
        ConstructorInstance, FunctionInstance, InstanceInstance, LambdaInstance,
        MethodInstance, StructConstructorInstance, StructInstanceInstance, Value
    }
};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, Rc<FunctionInstance>>>,
    builtin_methods: HashMap<BuiltInType, HashMap<InternIdx, IntrinsicFunction>>,
    names: HashMap<Path, Value>,
    locals: Vec<Value>,
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
                Declaration::Define(..) => (),
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

    // TODO: defines are order sensitive right now
    fn collect_define_name(&mut self, define: &DefineDeclaration) {
        let DefineDeclaration { expression, path, .. } = define;

        let Ok(value) = self.expression(expression) else {
            panic!();
        };
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
            "String" => BuiltInType::String,
            "Array" => BuiltInType::Array,
            _ => unreachable!()
        };

        self.builtin_methods.insert(t, HashMap::new());
        for method in methods {
            let MethodSignature { name, .. } = method;

            let mpath = path.append(*name.data());

            // TODO: Better error reporting here
            let f = INTRINSIC_FUNCTIONS
                .iter().find(|(ppath, _)| ppath == &mpath.to_string())
                .unwrap().1;

            self.builtin_methods.get_mut(&t).unwrap().insert(*name.data(), f);
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

    fn matc(&mut self, matc: &MatchExpression) -> ControlFlow {
        let MatchExpression { expressions, branches } = matc;

        let mut values = vec![];
        for expression in expressions {
            values.push(self.expression(expression)?)
        }

        for branch in branches {
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
            (Value::String(_), Pattern::String(_)) => (),
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let InstanceInstance { values, .. } = instance.as_ref();
                let VariantCasePattern { fields, .. } = variant_case;

                let empty_field = vec![];
                let fields = fields.as_ref().unwrap_or(&empty_field);

                for (value, field) in values.iter().zip(fields) {
                    self.value_pattern_match(value, field);
                }
            }
            (Value::Array(array), Pattern::Array(pattern)) => {
                let ArrayPattern { before, after, rest } = pattern;

                let array = array.borrow();

                for (value, pattern) in array.iter().zip(before) {
                    self.value_pattern_match(value, pattern);
                }

                if rest.is_some() {
                    let rest = &array[before.len()..array.len() - after.len()];
                    let rest_array = Value::Array(Rc::new(RefCell::new(rest.to_vec())));
                    self.locals.push(rest_array);
                }

                for (value, pattern) in array.iter().rev().zip(after.iter().rev()) {
                    self.value_pattern_match(value, pattern);
                }
            }
            (Value::Unit, Pattern::Unit) => (),
            _ => unreachable!(),
        }
    }

    fn retrn(&mut self, retrn: &ReturnExpression) -> ControlFlow {
        let ReturnExpression { expression } = retrn;

        let value = self.expression(expression)?;
        Err(FlowException::Return(value))
    }

    pub fn expression(&mut self, expression: &Located<Expression>) -> ControlFlow {
        match expression.data() {
            Expression::U64(u64) => Ok(Value::U64(*u64)),
            Expression::F32(f32) => Ok(Value::F32(*f32)),
            Expression::String(string_idx) => Ok(Value::String(*string_idx)),
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

    fn path(&mut self, path: &PathExpression) -> ControlFlow {
        let PathExpression { bound, .. } = path;

        match bound {
            Bound::Undetermined => unreachable!(),
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                Ok(self.locals[index].clone())
            },
            Bound::Absolute(path) => {
                println!("{path}");
                Ok(self.names[path].clone())
            },
        }
    }

    fn array(&mut self, array: &ArrayExpression) -> ControlFlow {
        let ArrayExpression { expressions } = array;

        let mut values = vec![];
        for expression in expressions {
            values.push(self.expression(expression)?);
        }

        Ok(Value::Array(Rc::new(RefCell::new(values))))
    }

    // TODO: Abstract application on Value and use it here
    fn application(&mut self, application: &ApplicationExpression) -> ControlFlow {
        let ApplicationExpression { function, arguments } = application;

        match self.expression(function)? {
            Value::Function(function) => {
                let FunctionInstance { body } = function.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in arguments {
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
                    for argument in arguments {
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
                for argument in arguments {
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
                    for argument in arguments {
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
                let instance = self.expression(&arguments[0])?.clone();
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
                        let f = self.builtin_methods[&BuiltInType::U64][&name];

                        let mut argument_values = vec![Value::U64(*u64)];
                        for argument in arguments {
                            argument_values.push(self.expression(argument)?);
                        }

                        return Ok(f(argument_values));
                    },
                    Value::F32(f32) => {
                        let f = self.builtin_methods[&BuiltInType::F32][&name];

                        let mut argument_values = vec![Value::F32(*f32)];
                        for argument in arguments {
                            argument_values.push(self.expression(argument)?);
                        }

                        return Ok(f(argument_values));
                    },
                    Value::String(string_idx) => {
                        let f = self.builtin_methods[&BuiltInType::String][&name];

                        let mut argument_values = vec![Value::String(*string_idx)];
                        for argument in arguments {
                            argument_values.push(self.expression(argument)?);
                        }

                        return Ok(f(argument_values));
                    },
                    Value::Array(array) => {
                        let f = self.builtin_methods[&BuiltInType::Array][&name];

                        let mut argument_values = vec![Value::Array(array.clone())];
                        for argument in arguments {
                            argument_values.push(self.expression(argument)?);
                        }

                        return Ok(f(argument_values));
                    },
                    _ => { unreachable!(); }
                };

                let FunctionInstance { body } = method.as_ref();

                let return_value;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in arguments {
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
                for (argument, field_name) in arguments.iter().zip(fields.iter()) {
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
                for argument in arguments {
                    argument_values.push(self.expression(argument)?);
                }

                Ok(f(argument_values))
            }
            Value::ExternalFunction(f) => {
                let mut argument_values = vec![];
                for argument in arguments {
                    argument_values.push(self.expression(argument)?);
                }

                Ok(f(argument_values))
            }

            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> ControlFlow {
        let ProjectionExpression { expression, name } = projection;

        let instance = self.expression(expression)?;

        match &instance {
            Value::Instance(instanceinstance) => {
                let InstanceInstance { constructor, .. } = instanceinstance.as_ref();
                let ConstructorInstance { type_path, .. } = constructor.as_ref();

                let function = self.methods[type_path][name.data()].clone();
                let method = MethodInstance { instance, function };
                Ok(Value::Method(Rc::new(method)))
            }
            Value::StructInstance(structinstanceinstance) => {
                let StructInstanceInstance { type_path, fields } = structinstanceinstance.as_ref();

                if fields.borrow().contains_key(name.data()) {
                    Ok(fields.borrow().get(name.data()).unwrap().clone())
                } else {
                    let function = self.methods[type_path][name.data()].clone();
                    let method = MethodInstance { instance, function };
                    Ok(Value::Method(Rc::new(method)))
                }
            },
            Value::U64(i64) => {
                let function = self.builtin_methods[&BuiltInType::U64][name.data()];
                Ok(Value::BuiltinMethod(Box::new(Value::U64(*i64)), function))
            }
            Value::F32(f32) => {
                let function = self.builtin_methods[&BuiltInType::F32][name.data()];
                Ok(Value::BuiltinMethod(Box::new(Value::F32(*f32)), function))
            }
            Value::String(string_idx) => {
                let function = self.builtin_methods[&BuiltInType::String][name.data()];
                Ok(Value::BuiltinMethod(Box::new(Value::String(*string_idx)), function))
            }
            Value::Array(array) => {
                let function = self.builtin_methods[&BuiltInType::Array][name.data()];
                Ok(Value::BuiltinMethod(Box::new(Value::Array(array.clone())), function))
            }
            _ => unreachable!(),
        }
    }

    fn lett(&mut self, lett: &LetExpression) -> ControlFlow {
        let LetExpression { value_expression, body_expression, .. } = lett;

        let value = self.expression(value_expression)?;

        let return_value;
        scoped!(self, {
            self.locals.push(value);
            return_value = self.expression(body_expression)?;
        });

        Ok(return_value)
    }

    fn sequence(&mut self, sequence: &SequenceExpression) -> ControlFlow {
        let SequenceExpression { expressions } = sequence;

        match &expressions[..] {
            [] => Ok(Value::Unit),
            [init@.., last] => {
                for expression in init {
                    self.expression(expression)?;
                }

                self.expression(last)
            }
        }
    }

    fn block(&mut self, block: &BlockExpression) -> ControlFlow {
        let BlockExpression { expressions } = block;

        for expression in expressions {
            self.expression(expression)?;
        }

        Ok(Value::Unit)
    }

    fn lambda(&mut self, lambda: &LambdaExpression) -> ControlFlow {
        let LambdaExpression { body, .. } = lambda;

        let capture = self.locals.clone();
        let body = *body.clone();
        let lambda = LambdaInstance { capture, body };

        Ok(Value::Lambda(Rc::new(lambda)))
    }

    fn assignment(&mut self, assignment: &AssignmentExpression) -> ControlFlow {
        let AssignmentExpression { assignable, expression } = assignment;

        let value = self.expression(expression)?;
        match assignable.data() {
            Expression::Path(path) => {
                let PathExpression { bound, .. } = path;

                match bound {
                    Bound::Local(idx) => {
                        let index = self.locals.len() - 1 - idx;
                        self.locals[index] = value
                    },
                    Bound::Absolute(_) |
                    Bound::Undetermined => unreachable!(),
                }
            },
            Expression::Projection(projection) => {
                let ProjectionExpression { expression, name } = projection;
                let Value::StructInstance(instance) = self.expression(expression)? else {
                    unreachable!()
                };

                let StructInstanceInstance { fields, .. } = instance.as_ref();
                *fields.borrow_mut().get_mut(name.data()).unwrap() = value;
            },
            _ => unreachable!()
        }

        Ok(Value::Unit)
    }

    fn whilee(&mut self, whilee: &WhileExpression) -> ControlFlow {
        let WhileExpression { condition, post, body } = whilee;

        let mut result = self.expression(condition)?.into_core_bool();
        while result {
            let body_result = self.expression(body);

            if let Some(post) = post {
                self.expression(post)?;
            }

            match body_result {
                Err(FlowException::Break) => break,
                Err(FlowException::Continue) => {
                    result = self.expression(condition)?.into_core_bool();
                    continue
                },
                Err(FlowException::Return(value)) => return Err(FlowException::Return(value)),
                Ok(_) => result = self.expression(condition)?.into_core_bool()
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