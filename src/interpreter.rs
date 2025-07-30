use std::{collections::HashMap, rc::Rc};

use crate::{bound::{Bound, Path}, declaration::{Declaration, FunctionDeclaration, InterfaceDeclaration, MethodDeclaration, MethodSignature, Module, VariantDeclaration}, expression::{ApplicationExpression, Expression, LambdaExpression, LetExpression, MatchExpression, PathExpression, Pattern, ProjectionExpression, ReturnExpression, SequenceExpression, VariantCasePattern}, interner::{InternIdx, Interner}, location::Located, value::{ConstructorInstance, FunctionInstance, InstanceInstance, LambdaInstance, MethodInstance, Value}};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, Rc<FunctionInstance>>>,
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

// NOTE: Err variant describes an exception
type ControlFlow = Result<Value, Value>;

impl Interpreter {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            names: HashMap::new(),
            locals: vec![],
        }
    }

    pub fn evaluate_main(&mut self, modules: &[Module], interner: &Interner) {
        for module in modules {
            self.collect_names(module);
        }

        let mut main_module = None;
        for module in modules {
            if module.path().as_string(interner) == "Main" {
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
                if interner.get(function.name.data()) == "main" {
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
            Ok(value) | Err(value) => value,
        };

        println!("\nResult = {}", value.as_string(interner));
    }

    fn collect_names(&mut self, module: &Module) {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Module(..) => (),
                Declaration::Import(..) => (),
                Declaration::Interface(interface) => self.collect_interface_name(interface),
                Declaration::Function(function) => self.collect_function_name(function),
                Declaration::Variant(variant) => self.collect_variant_name(variant),
            }
        }
    }

    fn collect_function_name(&mut self, function: &FunctionDeclaration) {
        let FunctionDeclaration { body, path, .. } = function;

        let function = FunctionInstance { body: body.clone() };
        let value = Value::Function(Rc::new(function));

        self.names.insert(path.clone(), value);
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        for case in cases {
            let constructor = ConstructorInstance {
                type_path: path.clone(),
                case: *case.data().identifier().data(),
            };

            let value = match case.data().arguments() {
                Some(..) => {
                    Value::Constructor(Rc::new(constructor))
                },
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
            let MethodDeclaration { name, body, .. } = method;

            let function = FunctionInstance { body: body.clone() };
            self.methods.get_mut(path).unwrap().insert(*name.data(), Rc::new(function));
        }
    }

    fn collect_interface_name(&mut self, interface: &InterfaceDeclaration) {
        let InterfaceDeclaration { methods, .. } = interface;

        for method in methods {
            let MethodSignature { path, name, .. } = method;

            let function = Value::InterfaceFunction(*name.data());
            self.names.insert(path.clone(), function);
        }
    }

    fn matc(&mut self, matc: &MatchExpression) -> ControlFlow {
        let MatchExpression { expression, branches } = matc;

        let value = self.expression(expression)?;
        for branch in branches {
            if self.does_value_pattern_match(&value, branch.data().pattern()) {
                let return_value;
                scoped!(self, {
                    self.value_pattern_match(&value, branch.data().pattern());
                    return_value = self.expression(branch.data().expression());
                });
                return return_value;
            }
        }

        todo!("Unexhaustive pattern matching")
    }

    fn does_value_pattern_match(&mut self, value: &Value, pattern: &Located<Pattern>) -> bool {
        match (value, pattern.data()) {
            (_, Pattern::Any(_)) => true,
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let VariantCasePattern { name, fields } = variant_case;
                let InstanceInstance { constructor, values } = instance.as_ref();

                if &constructor.case != name.data() {
                    return false;
                }

                let empty_field = vec![];
                let fields = fields.as_ref().unwrap_or(&empty_field);

                for (value, field) in values.iter().zip(fields) {
                    if !self.does_value_pattern_match(value, field) {
                        return false;
                    }
                }

                true
            }
            _ => unreachable!(),
        }
    }

    fn value_pattern_match(&mut self, value: &Value, pattern: &Located<Pattern>) {
        match (value, pattern.data()) {
            (value, Pattern::Any(_)) => {
                self.locals.push(value.clone());
            }
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let InstanceInstance { values, .. } = instance.as_ref();
                let VariantCasePattern { fields, .. } = variant_case;

                let empty_field = vec![];
                let fields = fields.as_ref().unwrap_or(&empty_field);

                for (value, field) in values.iter().zip(fields) {
                    self.value_pattern_match(value, field);
                }
            }
            _ => unreachable!(),
        }
    }

    fn retrn(&mut self, retrn: &ReturnExpression) -> ControlFlow {
        let ReturnExpression { expression } = retrn;

        let value = self.expression(expression)?;
        Err(value)
    }

    fn expression(&mut self, expression: &Located<Expression>) -> ControlFlow {
        match expression.data() {
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
            Expression::Let(lett) => self.lett(lett),
            Expression::Sequence(sequence) => self.sequence(sequence),
            Expression::Lambda(lambda) => self.lambda(lambda),
            Expression::Match(matc) => self.matc(matc),
            Expression::Return(retrn) => self.retrn(retrn),
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
            Bound::Absolute(path) => Ok(self.names[path].clone()),
        }
    }

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
                    Ok(value) | Err(value) => Ok(value),
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
                    Ok(value) | Err(value) => Ok(value),
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
                    Ok(value) | Err(value) => Ok(value),
                }
            }
            Value::InterfaceFunction(name) => {
                let instance = self.expression(&arguments[0])?.clone();
                let Value::Instance(value) = &instance else {
                    unreachable!();
                };

                let InstanceInstance { constructor, .. } = value.as_ref();
                let ConstructorInstance { type_path, .. } = constructor.as_ref();

                let method = self.methods[type_path][&name].clone();
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
                    Ok(value) | Err(value) => Ok(value),
                }
            }
            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> ControlFlow {
        let ProjectionExpression { expression, name } = projection;

        let instance = self.expression(expression)?;
        let Value::Instance(instanceinstance) = &instance else {
            unreachable!();
        };

        let InstanceInstance { constructor, .. } = instanceinstance.as_ref();
        let ConstructorInstance { type_path, .. } = constructor.as_ref();

        let function = self.methods[type_path][name.data()].clone();
        let method = MethodInstance { instance, function };
        Ok(Value::Method(Rc::new(method)))
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
            [] => todo!("Return Unit value"),
            [init@.., last] => {
                let _ = init
                    .iter().map(|expression| self.expression(expression))
                    .collect::<Vec<_>>();

                self.expression(last)
            }
        }
    }

    fn lambda(&mut self, lambda: &LambdaExpression) -> ControlFlow {
        let LambdaExpression { body, .. } = lambda;

        let capture = self.locals.clone();
        let body = *body.clone();
        let lambda = LambdaInstance { capture, body };

        Ok(Value::Lambda(Rc::new(lambda)))
    }
}