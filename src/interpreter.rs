use std::{collections::HashMap, rc::Rc};

use crate::{bound::{Bound, Path}, declaration::{Declaration, Module, ProcedureDeclaration, VariantDeclaration}, expression::{ApplicationExpression, Expression, PathExpression, ProjectionExpression}, interner::{InternIdx, Interner}, location::Located, statement::{MatchStatement, Pattern, ReturnStatement, Statement, VariantCasePattern}, value::Value};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, Value>>,
    names: HashMap<Path, Value>,
    locals: Vec<Value>,

    return_exception: Option<Value>,
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
        'outer: for module in modules {
            for declaration in module.declarations() {
                if let Declaration::Module(moduled) = declaration {
                    if interner.get(moduled.name.data()) == "Main" {
                        main_module = Some(module);
                        break 'outer;
                    }
                }
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

            if let Some(value) = self.return_exception.clone() {
                self.return_exception = None;
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
                Declaration::Procedure(procedure) => self.collect_procedure_name(procedure),
                Declaration::Variant(variant) => self.collect_variant_name(variant),
            }
        }
    }

    fn collect_procedure_name(&mut self, procedure: &ProcedureDeclaration) {
        let ProcedureDeclaration { body, path, .. } = procedure;

        let value = Value::Procedure {
            body: Rc::new(body.clone())
        };

        self.names.insert(path.clone(), value);
    }

    fn collect_variant_name(&mut self, variant: &VariantDeclaration) {
        let VariantDeclaration { cases, methods, path, .. } = variant;

        for case in cases {
            let value = match case.data().arguments() {
                Some(_arguments) => {
                    Value::Constructor {
                        name: *case.data().identifier().data(),
                        type_path: path.clone(),
                    }
                },
                None => {
                    Value::Instance {
                        type_path: path.clone(),
                        case: *case.data().identifier().data(),
                        values: Rc::default()
                    }
                },
            };

            self.names.insert(case.data().path().clone(), value);
        }

        self.methods.insert(path.clone(), HashMap::new());
        for method in methods {
            let value = Value::Procedure {
                body: Rc::new(method.body.clone())
            };

            self.methods.get_mut(path).unwrap().insert(*method.name.data(), value);
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
            let locals_len = self.locals.len();
            if self.value_pattern_match(&value, branch.data().pattern()) {
                self.statement(branch.data().statement());
                self.locals.truncate(locals_len);
                break;
            }
            self.locals.truncate(locals_len);
        }
    }

    fn value_pattern_match(
        &mut self,
        value: &Value,
        pattern: &Located<Pattern>,
    ) -> bool {
        match (value, pattern.data()) {
            (Value::Instance { type_path: _, case, values }, Pattern::VariantCase(variant_case)) => {
                let VariantCasePattern { name, fields } = variant_case;

                if case != name.data() {
                    return false;
                }

                match fields {
                    Some(_) => {
                        for value in values.iter() {
                            self.locals.push(value.clone());
                        }
                    },
                    None => {
                        assert!(values.is_empty());
                    },
                }

                true
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
                self.locals[self.locals.len() - 1 - bound_idx.idx()].clone()
            },
            Bound::Absolute(path) => self.names[path].clone(),
        }
    }

    fn application(&mut self, application: &ApplicationExpression) -> Value {
        let ApplicationExpression { function, arguments } = application;

        match self.expression(function) {
            Value::Procedure { body } => {
                let mut argument_values = vec![];
                for argument in arguments {
                    let argument = self.expression(argument);
                    argument_values.push(argument);
                }
                self.locals.extend(argument_values);

                let mut return_value = Value::None;
                for statement in body.iter() {
                    self.statement(statement);

                    if let Some(value) = self.return_exception.clone() {
                        self.return_exception = None;
                        return_value = value;
                        break;
                    }
                }

                self.locals.truncate(self.locals.len() - arguments.len());

                return_value
            },
            Value::Method { instance, body } => {
                let mut argument_values = vec![];
                for argument in arguments {
                    let argument = self.expression(argument);
                    argument_values.push(argument);
                }

                self.locals.push(*instance.clone());
                self.locals.extend(argument_values);

                let mut return_value = Value::None;
                for statement in body.iter() {
                    self.statement(statement);

                    if let Some(value) = self.return_exception.clone() {
                        self.return_exception = None;
                        return_value = value;
                        break;
                    }
                }

                self.locals.truncate(self.locals.len() - arguments.len());
                self.locals.pop();

                return_value
            },
            Value::Constructor { type_path, name } => {
                let mut values = vec![];
                for argument in arguments {
                    let argument = self.expression(argument);
                    values.push(argument);
                }

                Value::Instance {
                    type_path,
                    case: name,
                    values: Rc::new(values),
                }
            },
            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &ProjectionExpression) -> Value {
        let ProjectionExpression { expression, name } = projection;

        let instance = self.expression(expression);
        let Value::Instance { type_path, case: _, values: _ } = &instance else {
            unreachable!();
        };

        let Value::Procedure { body } = self.methods[type_path][name.data()].clone() else {
            unreachable!();
        };

        Value::Method { instance: Box::new(instance), body }
    }
}