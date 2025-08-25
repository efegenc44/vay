use std::{
    cell::RefCell, collections::HashMap, fmt::Display, rc::Rc
};

use crate::{
    bound::Path,
    expression::{
        ArrayPattern, Expression, Pattern, VariantCasePattern
    },
    interner::{interner, InternIdx},
    intrinsics::IntrinsicFunction,
    location::Located,
};

#[derive(Clone)]
pub enum Value {
    Function(Rc<FunctionInstance>),
    Method(Rc<MethodInstance>),
    Lambda(Rc<LambdaInstance>),
    InterfaceFunction(InternIdx),
    BuiltinMethod(Box<Value>, IntrinsicFunction),
    ExternalFunction(IntrinsicFunction),
    Constructor(Rc<ConstructorInstance>),
    Instance(Rc<InstanceInstance>),
    StructConstructor(Rc<StructConstructorInstance>),
    StructInstance(Rc<StructInstanceInstance>),
    U64(u64),
    F32(f32),
    Char(char),
    Array(Rc<RefCell<Vec<Value>>>),
    Unit
}

impl Value {
    pub fn matches(&self, pattern: &Pattern) -> bool {
        match (self, pattern) {
            (_, Pattern::Any(_)) => true,
            (Value::U64(u64_1), Pattern::U64(u64_2)) => u64_1 == u64_2,
            (Value::F32(f32_1), Pattern::F32(f32_2)) => f32_1 == f32_2,
            (Value::Array(array), Pattern::String(s2)) => {
                let string = array
                    .borrow()
                    .iter()
                    .map(|v| v.clone().into_char())
                    .collect::<String>();

                interner().intern_idx(&string) == *s2
            },
            (Value::Char(c1), Pattern::Char(c2)) => c1 == c2,
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
            (Value::Array(array), Pattern::Array(pattern)) => {
                let ArrayPattern { before, after, rest } = pattern;

                let array = array.borrow();

                // TODO: Better error reporting here or prefereably check for an
                //   exhaustive pattern matching should prevent this
                if rest.is_some() {
                    assert!(before.len() + after.len() <= array.len());
                } else {
                    assert!(before.len() + after.len() == array.len());
                }

                array.iter().zip(before)
                    .all(|(value, pattern)| value.matches(pattern.data())) &&
                array.iter().rev().zip(after.iter().rev())
                    .all(|(value, pattern)| value.matches(pattern.data()))

            }
            (Value::Unit, Pattern::Unit) => true,
            _ => unreachable!(),
        }
    }

    pub fn into_core_bool(self) -> bool {
        let Self::Instance(instance) = self else {
            panic!();
        };

        let case = instance.constructor.case;
        if interner().get(&case) == "True" {
            true
        } else if interner().get(&case) == "False" {
            false
        } else {
            unreachable!()
        }
    }

    pub fn into_u64(self) -> u64 {
        let Self::U64(v) = self else {
            panic!();
        };

        v
    }

    pub fn into_f32(self) -> f32 {
        let Self::F32(v) = self else {
            panic!();
        };

        v
    }

    pub fn into_array(self) -> Rc<RefCell<Vec<Value>>> {
        let Self::Array(v) = self else {
            panic!();
        };

        v
    }

    pub fn into_char(self) -> char {
        let Self::Char(v) = self else {
            panic!();
        };

        v
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Function(..) |
            Value::Method(..) |
            Value::Lambda(..) |
            Value::InterfaceFunction(..) |
            Value::BuiltinMethod(..) |
            Value::ExternalFunction(..) |
            Value::Constructor(..) |
            Value::StructConstructor(..) => write!(f, "<function>"),
            Value::Instance(instance) => {
                let InstanceInstance { constructor, values, .. } = instance.as_ref();
                let ConstructorInstance { case, .. } = constructor.as_ref();

                if values.is_empty() {
                    return write!(f, "{}", interner().get(case));
                }

                write!(f,"{}(", interner().get(case))?;
                let mut first = true;
                for value in values.iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, ")")
            },
            Value::StructInstance(instance) => {
                let StructInstanceInstance { type_path, fields } = instance.as_ref();

                write!(f, "{}(", type_path)?;
                let mut first = true;
                for (name, value) in fields.borrow().iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }

                    write!(f, "{}={}", interner().get(name), value)?;
                }
                write!(f, ")")
            },
            Value::U64(u64) => write!(f, "{}", u64),
            Value::F32(f32) => write!(f, "{}", f32),
            Value::Char(ch) => write!(f, "\'{ch}\'"),
            Value::Array(array) => {
                if let Some(Value::Char(_)) = array.borrow().first() {
                    write!(f, "\"")?;
                    for value in array.borrow().iter() {
                        write!(f, "{}", value.clone().into_char())?;
                    }
                    write!(f, "\"")?;

                    return Ok(())
                }

                write!(f, "[")?;
                let mut first = true;
                for value in array.borrow().iter() {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            },
            Value::Unit => write!(f, "()"),
        }
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
