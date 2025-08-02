use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{bound::Path, expression::{Expression, Pattern, VariantCasePattern}, interner::{InternIdx, Interner}, intrinsics::IntrinsicFunction, location::Located};

#[derive(Clone)]
pub enum Value {
    Function(Rc<FunctionInstance>),
    Method(Rc<MethodInstance>),
    Lambda(Rc<LambdaInstance>),
    InterfaceFunction(InternIdx),
    BuiltinMethod(Box<Value>, IntrinsicFunction),
    Constructor(Rc<ConstructorInstance>),
    Instance(Rc<InstanceInstance>),
    StructConstructor(Rc<StructConstructorInstance>),
    StructInstance(Rc<StructInstanceInstance>),
    U64(u64),
    Unit
}

impl Value {
    pub fn as_string(&self, interner: &Interner) -> String {
        match self {
            Value::Function(..) => "<function>".into(),
            Value::Method(..) => "<function>".into(),
            Value::Lambda(..) => "<function>".into(),
            Value::InterfaceFunction(..) => "<function>".into(),
            Value::BuiltinMethod(..) => "<function>".into(),
            Value::Constructor(..) => "<function>".into(),
            Value::StructConstructor(..) => "<function>".into(),
            Value::Instance(instance) => {
                let InstanceInstance { constructor, values, .. } = instance.as_ref();
                let ConstructorInstance { case, .. } = constructor.as_ref();

                if values.is_empty() {
                    return interner.get(case).into();
                }

                let mut string = format!("{}(", interner.get(case));
                let mut first = true;
                for value in values.iter() {
                    if first {
                        first = false;
                    } else {
                        string.push_str(", ");
                    }
                    string.push_str(&value.as_string(interner));
                }
                string.push(')');
                string
            },
            Value::StructInstance(instance) => {
                let StructInstanceInstance { type_path, fields } = instance.as_ref();

                let mut string = format!("{}(", type_path.as_string(interner));
                let mut first = true;
                for (name, value) in fields.borrow().iter() {
                    if first {
                        first = false;
                    } else {
                        string.push_str(", ");
                    }
                    string.push_str(&format!("{}={}", interner.get(name), value.as_string(interner)));
                }
                string.push(')');
                string
            },
            Value::U64(u64) => u64.to_string(),
            Value::Unit => "()".into(),
        }
    }

    pub fn matches(&self, pattern: &Pattern) -> bool {
        match (self, pattern) {
            (_, Pattern::Any(_)) => true,
            (Value::U64(u64_1), Pattern::Natural(u64_2)) => u64_1 == u64_2,
            (Value::Instance(instance), Pattern::VariantCase(variant_case)) => {
                let VariantCasePattern { name, fields } = variant_case;
                let InstanceInstance { constructor, values } = instance.as_ref();

                if &constructor.case != name.data() {
                    return false;
                }

                let empty_field = vec![];
                let fields = fields.as_ref().unwrap_or(&empty_field);

                for (value, field) in values.iter().zip(fields) {
                    if !value.matches(field.data()) {
                        return false;
                    }
                }

                true
            }
            (Value::Unit, Pattern::Unit) => true,
            _ => unreachable!(),
        }
    }

    pub fn into_u64(self) -> u64 {
        let Self::U64(v) = self else {
            panic!();
        };

        v
    }
}

pub struct FunctionInstance {
    pub body: Located<Expression>
}

// TODO: Method probably should take something like Callable
//   instead of FunctionInstance
pub struct MethodInstance {
    pub instance: Value,
    pub function: Rc<FunctionInstance>
}

pub struct LambdaInstance {
    pub capture: Vec<Value>,
    pub body: Located<Expression>
}

pub struct ConstructorInstance {
    pub type_path: Path,
    pub case: InternIdx
}

pub struct InstanceInstance {
    pub constructor: Rc<ConstructorInstance>,
    pub values: Vec<Value>
}

pub struct StructConstructorInstance {
    pub type_path: Path,
    pub fields: Vec<InternIdx>
}

pub struct StructInstanceInstance {
    pub type_path: Path,
    pub fields: RefCell<HashMap<InternIdx, Value>>
}
