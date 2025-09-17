pub mod value;

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    resolution::bound::{Bound, Path},
    ast::{
        declaration::{self, Declaration},
        expression::{self, Expression},
        pattern::Pattern,
    },
    interner::{interner, InternIdx},
    vay::intrinsics::{EXTERNAL_FUNCTIONS, INTRINSIC_FUNCTIONS},
    lex::location::Located,
    check::typ,
    interpret::value::Value
};

pub struct Interpreter {
    methods: HashMap<Path, HashMap<InternIdx, value::Function>>,
    builtin_methods: HashMap<typ::BuiltIn, HashMap<InternIdx, value::Function>>,
    names: HashMap<Path, Value>,
    locals: Vec<Value>,

    defines: HashMap<Path, Expression>,
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

    pub fn evaluate_main(&mut self, modules: &[declaration::Module]) {
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
        for declaration in main_module.declarations() {
            if let Declaration::Function(function) = declaration {
                if interner().get(function.name().data()) == "main" {
                    main_function = Some(function);
                    break;
                }
            }
        }

        let Some(main_function) = main_function else {
            todo!("main function is not declared");
        };

        if !main_function.arguments().is_empty() {
            todo!("main function is not supposed to take any arguments");
        }

        let value = self.expression(main_function.body().data());
        let value = match value {
            Ok(value) | Err(FlowException::Return(value)) => value,
            _ => unreachable!()
        };

        println!("\nResult = {}", value);
    }

    pub fn collect_names(&mut self, module: &declaration::Module) {
        for declaration in module.declarations() {
            match declaration {
                Declaration::ModulePath(..) => (),
                Declaration::Import(..) => (),
                Declaration::Define(define) => {
                    self.defines.insert(
                        define.path().clone(),
                        define.expression().data().clone()
                    );
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

    fn collect_function_name(&mut self, function: &declaration::Function) {
        let function_inst = value::Function::Normal(Rc::new(function.body().data().clone()));
        let value = Value::Function(function_inst);

        self.names.insert(function.path().clone(), value);
    }

    fn collect_define_name(&mut self, define: &declaration::Define) {
        self.collecting_define = true;
        let Ok(value) = self.expression(define.expression().data()) else {
            unreachable!();
        };
        self.collecting_define = false;

        self.names.insert(define.path().clone(), value);
    }

    fn collect_method_name(&mut self, path: &Path, method: &declaration::Method) {
        let function = value::Function::Normal(Rc::new(method.body().data().clone()));
        self.methods.get_mut(path).unwrap().insert(*method.signature().name().data(), function);
    }

    fn collect_variant_name(&mut self, variant: &declaration::Variant) {
        for case in variant.cases() {
            let constructor = value::VariantConstructor::new(
                variant.path().clone(),
                *case.data().identifier().data()
            );

            let value = match case.data().arguments() {
                Some(_) => Value::VariantConstructor(constructor),
                None => {
                    let instance = value::VariantInstance::new(constructor, vec![]);
                    Value::VariantInstance(instance)
                },
            };

            self.names.insert(case.data().path().clone(), value);
        }

        self.methods.insert(variant.path().clone(), HashMap::new());
        for method in variant.methods() {
            self.collect_method_name(variant.path(), method);
        }
    }

    fn collect_struct_name(&mut self, strct: &declaration::Struct) {
        let fields = strct.fields()
            .iter()
            .map(|field| *field.data().identifier().data())
            .collect();

        let constructor = value::StructConstructor::new(strct.path().clone(), fields);
        self.names.insert(strct.path().clone(), Value::StructConstructor(constructor));

        self.methods.insert(strct.path().clone(), HashMap::new());
        for method in strct.methods() {
            self.collect_method_name(strct.path(), method);
        }
    }

    fn collect_interface_name(&mut self, interface: &declaration::Interface) {
        for method in interface.methods() {
            let function = Value::InterfaceFunction(*method.name().data());
            self.names.insert(method.path().clone(), function);
        }
    }

    fn collect_builtin_name(&mut self, builtin: &declaration::BuiltIn) {
        let t = match interner().get(builtin.name().data()) {
            "U64" => typ::BuiltIn::U64,
            "F32" => typ::BuiltIn::F32,
            "Char" => typ::BuiltIn::Char,
            "Array" => typ::BuiltIn::Array,
            _ => unreachable!()
        };

        self.builtin_methods.insert(t, HashMap::new());
        for (signature, body) in builtin.methods() {
            if let Some(body) = body {
                let f = value::Function::Normal(Rc::new(body.data().clone()));
                self.builtin_methods.get_mut(&t).unwrap().insert(*signature.name().data(), f);
            } else {
                let mpath = builtin.path().append(*signature.name().data());

                // TODO: Better error reporting here
                let f = INTRINSIC_FUNCTIONS
                    .iter()
                    .find(|(ppath, _)| ppath == &mpath.to_string())
                    .map(|x| value::Function::BuiltIn(x.1))
                    .unwrap();

                self.builtin_methods.get_mut(&t).unwrap().insert(*signature.name().data(), f);
            }
        }
    }

    fn collect_external_name(&mut self, external: &declaration::External) {
        // TODO: Better error reporting here
        // TODO: External functions via dynamic loading?
        let f = EXTERNAL_FUNCTIONS
            .iter()
            .find(|(ppath, _)| ppath == &external.path().to_string())
            .map(|x| value::Function::BuiltIn(x.1))
            .unwrap();

        let function = Value::Function(f);
        self.names.insert(external.path().clone(), function);
    }

    fn matc(&mut self, matc: &expression::Match) -> ControlFlow {
        let mut values = vec![];
        for expression in matc.expressions() {
            values.push(self.expression(expression.data())?)
        }

        for branch in matc.branches() {
            if values.iter().zip(branch.data().patterns()).all(|(v, p)| v.matches(p.data())) {
                let return_value;
                scoped!(self, {
                    for (value, pattern) in values.iter().zip(branch.data().patterns()) {
                        self.define_pattern_locals(value, pattern);
                    }
                    return_value = self.expression(branch.data().expression().data());
                });
                return return_value;
            }
        }

        todo!("Unexhaustive pattern matching")
    }

    fn define_pattern_locals(&mut self, value: &Value, pattern: &Located<Pattern>) {
        match (value, pattern.data()) {
            (value, Pattern::Any(_)) => {
                self.locals.push(value.clone());
            }
            (Value::U64(_), Pattern::U64(_)) |
            (Value::F32(_), Pattern::F32(_)) |
            (Value::Array(_), Pattern::String(_)) |
            (Value::Char(_), Pattern::Char(_)) => (),
            (Value::VariantInstance(instance), Pattern::VariantCase(variant_case)) => {
                let empty_field = vec![];
                let fields = variant_case.fields().unwrap_or(&empty_field);

                for (value, field) in instance.values().iter().zip(fields) {
                    self.define_pattern_locals(value, field);
                }
            }
            (Value::Array(array), Pattern::Array(pattern)) => {
                for (value, pattern) in array.values().borrow().iter().zip(pattern.before()) {
                    self.define_pattern_locals(value, pattern);
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
                        (interner().intern_idx("length"), Value::U64((array.values().borrow().len() - pattern.after().len() - pattern.before().len()) as u64)),
                    ]);
                    let fields = Rc::new(RefCell::new(fields));

                    let instance = value::StructInstance::new(type_path, fields);

                    let rest_view = Value::StructInstance(instance);
                    self.locals.push(rest_view);
                }

                for (value, pattern) in array.values().borrow().iter().rev().zip(pattern.after().iter().rev()) {
                    self.define_pattern_locals(value, pattern);
                }
            }
            (Value::Unit, Pattern::Unit) => (),
            _ => unreachable!(),
        }
    }

    fn retrn(&mut self, retrn: &expression::Return) -> ControlFlow {
        let value = self.expression(retrn.expression().data())?;
        Err(FlowException::Return(value))
    }

    pub fn expression(&mut self, expression: &Expression) -> ControlFlow {
        match expression {
            Expression::U64(u64) => Ok(Value::U64(*u64)),
            Expression::F32(f32) => Ok(Value::F32(*f32)),
            Expression::Char(ch) => Ok(Value::Char(*ch)),
            Expression::String(string_idx) => self.string(string_idx),
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

    fn string(&mut self, intern_idx: &InternIdx) -> ControlFlow {
        let string = interner()
            .get(intern_idx)
            .chars()
            .map(Value::Char)
            .collect::<Vec<_>>();

        Ok(Value::Array(value::Array::new(Rc::new(RefCell::new(string)))))
    }

    fn path(&mut self, path: &expression::Path) -> ControlFlow {
        match path.bound() {
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
            values.push(self.expression(expression.data())?);
        }

        Ok(Value::Array(value::Array::new(Rc::new(RefCell::new(values)))))
    }

    fn application(&mut self, application: &expression::Application) -> ControlFlow {
        let function = self.expression(application.function().data())?;
        let mut arguments = vec![];
        for argument in application.arguments() {
            arguments.push(self.expression(argument.data())?);
        }

        self.apply(function, arguments)
    }

    fn apply(&mut self, function: Value, arguments: Vec<Value>) -> ControlFlow {
        if let Value::VariantConstructor(constructor) = function {
            let instance = value::VariantInstance::new(constructor, arguments);
            return Ok(Value::VariantInstance(instance));
        }

        if let Value::StructConstructor(constructor) = function {
            let values = constructor.fields()
                .iter()
                .map(|x| *x)
                .zip(arguments)
                .collect();

            let instance = value::StructInstance::new(
                constructor.type_path().clone(),
                Rc::new(RefCell::new(values))
            );

            return Ok(Value::StructInstance(instance));
        }

        let (function, mut capture) = match function {
            Value::Function(function) => (function, vec![]),
            Value::Method(method) => {
                (method.function().clone(), vec![method.instance().clone()])
            },
            Value::Lambda(lambda) => {
                (value::Function::Normal(Rc::new(lambda.expression().clone())), lambda.capture().to_vec())
            },
            Value::InterfaceFunction(method_name) => {
                let instance = arguments[0].clone();
                let method = match &instance {
                    Value::VariantInstance(instance) => {
                        self.methods[instance.constructor().type_path()][&method_name].clone()
                    },
                    Value::StructInstance(instance) => {
                        self.methods[&instance.type_path()][&method_name].clone()
                    },
                    Value::U64(_) => self.builtin_methods[&typ::BuiltIn::U64][&method_name].clone(),
                    Value::F32(_) => self.builtin_methods[&typ::BuiltIn::F32][&method_name].clone(),
                    Value::Char(_) => self.builtin_methods[&typ::BuiltIn::Char][&method_name].clone(),
                    Value::Array(_) => self.builtin_methods[&typ::BuiltIn::Array][&method_name].clone(),
                    _ => unreachable!()
                };

                (method, vec![instance])
            },
            _ => unreachable!()
        };

        let return_value = match function {
            value::Function::BuiltIn(function) => {
                capture.extend(arguments);
                Ok(function(capture))
            },
            value::Function::Normal(expression) => {
                let return_value;
                scoped!(self, {
                    self.locals.extend(capture);
                    self.locals.extend(arguments);

                    return_value = self.expression(expression.as_ref());
                });
                return_value
            },
        };

        match return_value {
            Ok(value) | Err(FlowException::Return(value)) => Ok(value),
            _ => unreachable!()
        }
    }

    fn projection(&mut self, projection: &expression::Projection) -> ControlFlow {
        let value = self.expression(projection.expression().data())?;

        match &value {
            Value::VariantInstance(instance) => {
                let function = self.methods
                    [instance.constructor().type_path()]
                    [projection.projected().data()]
                    .clone();

                let method = value::Method::new(Box::new(value), function);
                Ok(Value::Method(method))
            }
            Value::StructInstance(instance) => {
                if instance.fields().borrow().contains_key(projection.projected().data()) {
                    Ok(instance.fields().borrow().get(projection.projected().data()).unwrap().clone())
                } else {
                    let function = self.methods
                        [instance.type_path()]
                        [projection.projected().data()]
                        .clone();

                    let method = value::Method::new(Box::new(value), function);
                    Ok(Value::Method(method))
                }
            },
            Value::U64(i64) => {
                let function = self.builtin_methods[&typ::BuiltIn::U64][projection.projected().data()].clone();
                Ok(Value::Method(value::Method::new(Box::new(Value::U64(*i64)), function)))
            }
            Value::F32(f32) => {
                let function = self.builtin_methods[&typ::BuiltIn::F32][projection.projected().data()].clone();
                Ok(Value::Method(value::Method::new(Box::new(Value::F32(*f32)), function)))
            }
            Value::Char(ch) => {
                let function = self.builtin_methods[&typ::BuiltIn::Char][projection.projected().data()].clone();
                Ok(Value::Method(value::Method::new(Box::new(Value::Char(*ch)), function)))
            }
            Value::Array(array) => {
                let function = self.builtin_methods[&typ::BuiltIn::Array][projection.projected().data()].clone();
                Ok(Value::Method(value::Method::new(Box::new(Value::Array(array.clone())), function)))
            }
            _ => unreachable!(),
        }
    }

    fn lett(&mut self, lett: &expression::Let) -> ControlFlow {
        let value = self.expression(lett.value_expression().data())?;

        let return_value;
        scoped!(self, {
            self.locals.push(value);
            return_value = self.expression(lett.body_expression().data())?;
        });

        Ok(return_value)
    }

    fn sequence(&mut self, sequence: &expression::Sequence) -> ControlFlow {
        match &sequence.expressions()[..] {
            [] => Ok(Value::Unit),
            [init@.., last] => {
                for expression in init {
                    self.expression(expression.data())?;
                }

                self.expression(last.data())
            }
        }
    }

    fn block(&mut self, block: &expression::Block) -> ControlFlow {
        for expression in block.expressions() {
            self.expression(expression.data())?;
        }

        Ok(Value::Unit)
    }

    fn lambda(&mut self, lambda: &expression::Lambda) -> ControlFlow {
        let capture = self.locals.clone();
        let expression = lambda.body().data().clone();
        let lambda = value::Lambda::new(capture, Rc::new(expression));

        Ok(Value::Lambda(lambda))
    }

    fn assignment(&mut self, assignment: &expression::Assignment) -> ControlFlow {
        let value = self.expression(assignment.expression().data())?;

        match assignment.assignable().data() {
            Expression::Path(path) => {
                match path.bound() {
                    Bound::Local(idx) => {
                        let index = self.locals.len() - 1 - idx;
                        self.locals[index] = value
                    },
                    Bound::Absolute(_) => panic!("Global values are not assignable. Ensured by the type checker.")
                }
            },
            Expression::Projection(projection) => {
                let Value::StructInstance(instance) = self.expression(projection.expression().data())? else {
                    unreachable!()
                };

                *instance
                    .fields()
                    .borrow_mut()
                    .get_mut(projection.projected().data())
                    .unwrap()
                    = value;
            },
            _ => unreachable!()
        }

        Ok(Value::Unit)
    }

    fn whilee(&mut self, whilee: &expression::While) -> ControlFlow {
        let mut result = self.expression(whilee.condition().data())?.into_core_bool();
        while result {
            let body_result = self.expression(whilee.body().data());

            if let Some(post) = whilee.post() {
                self.expression(post.data())?;
            }

            match body_result {
                Err(FlowException::Break) => {
                    break
                },
                Err(FlowException::Return(value)) => {
                    return Err(FlowException::Return(value))
                },
                Ok(_) | Err(FlowException::Continue) => {
                    result = self.expression(whilee.condition().data())?.into_core_bool()
                }
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