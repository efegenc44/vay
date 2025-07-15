use std::rc::Rc;

use crate::{bound::Path, interner::{InternIdx, Interner}, location::Located, statement::Statement};

#[derive(Clone)]
pub enum Value {
    Procedure(Rc<ProcedureInstance>),
    Method(Rc<MethodInstance>),
    Constructor(Rc<ConstructorInstance>),
    Instance(Rc<InstanceInstance>),
    None
}

impl Value {
    pub fn as_string(&self, interner: &Interner) -> String {
        match self {
            Value::Procedure(..) => "<function>".into(),
            Value::Method(..) => "<function>".into(),
            Value::Constructor(..) => "<function>".into(),
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
            Value::None => "None".into(),
        }
    }
}

pub struct ProcedureInstance {
    pub body: Vec<Located<Statement>>
}

pub struct MethodInstance {
    pub instance: Value,
    pub procedure: Rc<ProcedureInstance>
}

pub struct ConstructorInstance {
    pub type_path: Path,
    pub case: InternIdx
}

pub struct InstanceInstance {
    pub constructor: Rc<ConstructorInstance>,
    pub values: Vec<Value>
}