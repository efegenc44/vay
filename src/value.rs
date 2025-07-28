use std::rc::Rc;

use crate::{bound::Path, expression::Expression, interner::{InternIdx, Interner}, location::Located};

#[derive(Clone)]
pub enum Value {
    Function(Rc<FunctionInstance>),
    Method(Rc<MethodInstance>),
    Lambda(Rc<LambdaInstance>),
    Constructor(Rc<ConstructorInstance>),
    Instance(Rc<InstanceInstance>),
}

impl Value {
    pub fn as_string(&self, interner: &Interner) -> String {
        match self {
            Value::Function(..) => "<function>".into(),
            Value::Method(..) => "<function>".into(),
            Value::Constructor(..) => "<function>".into(),
            Value::Lambda(..) => "<function>".into(),
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
        }
    }
}

pub struct FunctionInstance {
    pub body: Located<Expression>
}

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