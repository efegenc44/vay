use std::{
    cell::RefCell, collections::HashMap, fmt::Display, rc::Rc
};

use crate::{
    resolution::bound::Path,
    ast::{
        expression::Expression,
        pattern::Pattern,
    },
    interner::{interner, InternIdx},
    vay::intrinsics::IntrinsicFunction,
};

#[derive(Clone)]
pub enum Value {
    Function(Function),
    Method(Method),
    Lambda(Lambda),
    InterfaceFunction(InternIdx),
    VariantConstructor(VariantConstructor),
    VariantInstance(VariantInstance),
    StructConstructor(StructConstructor),
    StructInstance(StructInstance),
    U64(u64),
    F32(f32),
    Char(char),
    Array(Array),
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
                    .values()
                    .borrow()
                    .iter()
                    .map(|v| v.clone().into_char())
                    .collect::<String>();

                interner().intern_idx(&string) == *s2
            },
            (Value::Char(c1), Pattern::Char(c2)) => c1 == c2,
            (Value::VariantInstance(instance), Pattern::VariantCase(variant_case)) => {
                if &instance.constructor().case() != variant_case.case().data() {
                    return false;
                }

                let empty_field = vec![];
                let fields = variant_case.fields().unwrap_or(&empty_field);

                for (value, field) in instance.values().iter().zip(fields) {
                    if !value.matches(field.data()) {
                        return false;
                    }
                }

                true
            }
            (Value::Array(array), Pattern::Array(pattern)) => {
                let array = array.values().borrow();

                // TODO: Better error reporting here or prefereably check for an
                //   exhaustive pattern matching should prevent this
                if pattern.rest().is_some() {
                    assert!(pattern.before().len() + pattern.after().len() <= array.len());
                } else {
                    assert!(pattern.before().len() + pattern.after().len() == array.len());
                }

                array.iter().zip(pattern.before())
                    .all(|(value, pattern)| value.matches(pattern.data())) &&
                array.iter().rev().zip(pattern.after().iter().rev())
                    .all(|(value, pattern)| value.matches(pattern.data()))

            }
            (Value::Unit, Pattern::Unit) => true,
            _ => unreachable!(),
        }
    }

    pub fn into_core_bool(self) -> bool {
        let Self::VariantInstance(instance) = self else {
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

    pub fn into_array(self) -> Array {
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
            Value::VariantConstructor(..) |
            Value::StructConstructor(..) => write!(f, "<function>"),
            Value::VariantInstance(instance) => {
                let case = &instance.constructor.case();
                let values = instance.values();

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
                write!(f, "{}(", instance.type_path())?;
                let mut first = true;
                for (name, value) in instance.fields().borrow().iter() {
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
                if let Some(Value::Char(_)) = array.values().borrow().first() {
                    write!(f, "\"")?;
                    for value in array.values().borrow().iter() {
                        write!(f, "{}", value.clone().into_char())?;
                    }
                    write!(f, "\"")?;

                    return Ok(())
                }

                write!(f, "[")?;
                let mut first = true;
                for value in array.values().borrow().iter() {
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

#[derive(Clone)]
pub enum Function {
    BuiltIn(IntrinsicFunction),
    Normal(Rc<Expression>)
}

#[derive(Clone)]
pub struct Method {
    instance: Box<Value>,
    function: Function
}

impl Method {
    pub fn new(instance: Box<Value>, function: Function) -> Self {
        Self { instance, function }
    }

    pub fn instance(&self) -> &Value {
        &self.instance
    }

    pub fn function(&self) -> &Function {
        &self.function
    }
}

#[derive(Clone)]
pub struct Lambda {
    capture: Vec<Value>,
    expression: Rc<Expression>
}

impl Lambda {
    pub fn new(capture: Vec<Value>, expression: Rc<Expression>) -> Self {
        Self { capture, expression }
    }

    pub fn capture(&self) -> &[Value] {
        &self.capture
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }
}

#[derive(Clone)]
pub struct VariantConstructor {
    type_path: Path,
    case: InternIdx
}

impl VariantConstructor {
    pub fn new(type_path: Path, case: InternIdx) -> Self {
        Self { type_path, case }
    }

    pub fn type_path(&self) -> &Path {
        &self.type_path
    }

    pub fn case(&self) -> InternIdx {
        self.case
    }
}

#[derive(Clone)]
pub struct VariantInstance {
    constructor: VariantConstructor,
    values: Vec<Value>
}

impl VariantInstance {
    pub fn new(constructor: VariantConstructor, values: Vec<Value>) -> Self {
        Self { constructor, values }
    }

    pub fn constructor(&self) -> &VariantConstructor {
        &self.constructor
    }

    pub fn values(&self) -> &[Value] {
        &self.values
    }
}

#[derive(Clone)]
pub struct StructConstructor {
    type_path: Path,
    fields: Vec<InternIdx>
}

impl StructConstructor {
    pub fn new(type_path: Path, fields: Vec<InternIdx>) -> Self {
        Self { type_path, fields }
    }

    pub fn type_path(&self) -> &Path {
        &self.type_path
    }

    pub fn fields(&self) -> &[InternIdx] {
        &self.fields
    }
}

#[derive(Clone)]
pub struct StructInstance {
    type_path: Path,
    fields: Rc<RefCell<HashMap<InternIdx, Value>>>
}

impl StructInstance {
    pub fn new(type_path: Path, fields: Rc<RefCell<HashMap<InternIdx, Value>>>) -> Self {
        Self { type_path, fields }
    }

    pub fn type_path(&self) -> &Path {
        &self.type_path
    }

    pub fn fields(&self) -> &RefCell<HashMap<InternIdx, Value>> {
        &self.fields
    }
}

#[derive(Clone)]
pub struct Array {
    values: Rc<RefCell<Vec<Value>>>
}

impl Array {
    pub fn new(values: Rc<RefCell<Vec<Value>>>) -> Self {
        Self { values }
    }

    pub fn values(&self) -> &RefCell<Vec<Value>> {
        &self.values
    }
}