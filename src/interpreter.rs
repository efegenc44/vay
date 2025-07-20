use std::{collections::HashMap, rc::Rc};

use crate::{bound::{Bound, Path}, declaration::{Declaration, MethodDeclaration, Module, ProcedureDeclaration, VariantDeclaration}, expression::{ApplicationExpression, Expression, PathExpression, ProjectionExpression}, interner::{InternIdx, Interner}, location::Located, statement::{MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern}, value::{ConstructorInstance, InstanceInstance, MethodInstance, ProcedureInstance, Value}};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, Rc<ProcedureInstance>>>,
    names: HashMap<Path, Value>,
    locals: Vec<Value>,

    return_exception: Option<Value>,
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

impl Interpreter {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new(),
            names: HashMap::new(),
            locals: vec![],

            return_exception: None,
        }
    }

    pub fn evaluate_main(&mut self, modules: &[Module], interner: &Interner) {
        for module in modules {
            self.collect_names(module);
        }

        let mut main_module = None;
        for module in modules {
            if interner.get(&module.name()) == "Main" {
                main_module = Some(module);
                break;
            }
        }
        let Some(main_module) = main_module else {
            todo!("Main module is not declared");
        };

        let mut main_procedure = None;
        'outer: for declaration in main_module.declarations() {
            if let Declaration::Procedure(procedure) = declaration {
                if interner.get(procedure.name.data()) == "main" {
                    main_procedure = Some(declaration);
                    break 'outer;
                }
            }
        }
        let Some(main_function) = main_procedure else {
            todo!("main procedure is not declared");
        };

        let Declaration::Procedure(procedure) = main_function else {
            unreachable!();
        };

        if !procedure.arguments.is_empty() {
            todo!("main procedure is not supposed to take any arguments");
        }

        for statement in &procedure.body {
            self.statement(statement);

            if let Some(value) = self.return_exception.take() {
                println!("\nResult = {}", value.as_string(interner));
                break;
            }
        }
    }

    fn collect_names(&mut self, module: &Module) {
        for declaration in module.declarations() {
            match declaration {
                Declaration::Module(..) => (),
                Declaration::Import(..) => (),
                Declaration::Interface(..) => (),
                Declaration::Procedure(procedure) => self.collect_procedure_name(procedure),
                Declaration::Variant(variant) => self.collect_variant_name(variant),
            }
        }
    }

    fn collect_procedure_name(&mut self, procedure: &ProcedureDeclaration) {
        let ProcedureDeclaration { body, path, .. } = procedure;

        let procedure = ProcedureInstance { body: body.clone() };
        let value = Value::Procedure(Rc::new(procedure));

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

            let procedure = ProcedureInstance { body: body.clone() };
            self.methods.get_mut(path).unwrap().insert(*name.data(), Rc::new(procedure));
        }
    }

    fn statement(&mut self, statement: &Located<Statement>) {
        if self.return_exception.clone().is_some() {
            return;
        }

        match statement.data() {
            Statement::Expression(expression) => {
                self.expression(expression);
            },
            Statement::Return(retrn) => self.retrn(retrn),
            Statement::Match(matc) => self.matc(matc),
        }
    }

    fn retrn(&mut self, retrn: &ReturnStatement)  {
        let ReturnStatement { expression } = retrn;

        let value = self.expression(expression);
        self.return_exception = Some(value);
    }

    fn matc(&mut self, matc: &MatchStatement) {
        let MatchStatement { expression, branches } = matc;

        let value = self.expression(expression);
        for branch in branches {
            if self.does_value_pattern_match(&value, branch.data().pattern()) {
                scoped!(self, {
                    self.value_pattern_match(&value, branch.data().pattern());
                    self.statement(branch.data().statement());
                });
                break;
            }
        }
    }

    fn does_value_pattern_match(&mut self, value: &Value, pattern: &Located<Pattern>) -> bool {
        match (value, pattern.data()) {
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let VariantCasePattern { name, .. } = variant_case;
                let InstanceInstance { constructor, .. } = instance.as_ref();

                &constructor.case == name.data()
            }
            _ => unreachable!(),
        }
    }

    fn value_pattern_match(&mut self, value: &Value, pattern: &Located<Pattern>) {
        match (value, pattern.data()) {
            (Value::Instance(instance), Pattern::VariantCase(_)) => {
                let InstanceInstance { values, .. } = instance.as_ref();

                for value in values {
                    self.locals.push(value.clone());
                }
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self, expression: &Located<Expression>) -> Value {
        match expression.data() {
            Expression::Path(path) => self.path(path),
            Expression::Application(application) => self.application(application),
            Expression::Projection(projection) => self.projection(projection),
        }
    }

    fn path(&mut self, path: &PathExpression) -> Value {
        let PathExpression { bound, .. } = path;

        match bound {
            Bound::Undetermined => unreachable!(),
            Bound::Local(bound_idx) => {
                let index = self.locals.len() - 1 - bound_idx;
                self.locals[index].clone()
            },
            Bound::Absolute(path) => self.names[path].clone(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression) -> Value {
        let ApplicationExpression { function, arguments } = application;

        match self.expression(function) {
            Value::Procedure(procedure) => {
                let ProcedureInstance { body } = procedure.as_ref();

                let mut return_value = Value::None;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in arguments {
                        let argument = self.expression(argument);
                        argument_values.push(argument);
                    }
                    self.locals.extend(argument_values);
                    for statement in body.iter() {
                        self.statement(statement);

                        if let Some(value) = self.return_exception.take() {
                            return_value = value;
                            break;
                        }
                    }
                });

                return_value
            },
            Value::Method(method) => {
                let MethodInstance { instance, procedure } = method.as_ref();
                let ProcedureInstance { body } = procedure.as_ref();

                let mut return_value = Value::None;
                scoped!(self, {
                    let mut argument_values = vec![];
                    for argument in arguments {
                        argument_values.push(self.expression(argument));
                    }
                    self.locals.push(instance.clone());
                    self.locals.extend(argument_values);
                    for statement in body {
                        self.statement(statement);

                        if let Some(value) = self.return_exception.take() {
                            return_value = value;
                            break;
                        }
                    }
                });


                return_value
            },
            Value::Constructor(constructor) => {
                let mut values = vec![];
                for argument in arguments {
                    values.push(self.expression(argument));
                }

                let instance = InstanceInstance { constructor, values };
                Value::Instance(Rc::new(instance))
            },
            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> Value {
        let ProjectionExpression { expression, name } = projection;

        let instance = self.expression(expression);
        let Value::Instance(instanceinstance) = &instance else {
            unreachable!();
        };

        let InstanceInstance { constructor, .. } = instanceinstance.as_ref();
        let ConstructorInstance { type_path, .. } = constructor.as_ref();

        let procedure = self.methods[type_path][name.data()].clone();
        let method = MethodInstance { instance, procedure };
        Value::Method(Rc::new(method))
    }
}