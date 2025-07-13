use std::{collections::HashMap, rc::Rc};

use crate::{bound::Path, interner::{InternIdx, Interner}, location::Located, statement::Statement};

#[derive(Clone)]
pub enum Value {
    Procedure {
        body: Rc<Vec<Located<Statement>>>
    },
    Method {
        instance: Box<Value>,
        body: Rc<Vec<Located<Statement>>>
    },
    Constructor {
        type_path: Path,
        name: InternIdx,
        arguments_in_order: Vec<InternIdx>,
    },
    Instance {
        type_path: Path,
        case: InternIdx,
        values: Rc<HashMap<InternIdx, Value>>
    },
    None
}

impl Value {
    pub fn as_string(&self, interner: &Interner) -> String {
        match self {
            Value::Procedure { .. } => "<function>".into(),
            Value::Method { .. } => "<function>".into(),
            Value::Constructor { .. } => "<function>".into(),
            Value::Instance { type_path: _, case, values } => {
                if values.is_empty() {
                    return interner.get(case).into();
                }

                let mut string = format!("{} {{ ", interner.get(case));
                for (field, value) in values.iter() {
                    string.push_str(&format!("{} = {}; ", interner.get(field), value.as_string(interner)));
                }
                string.push('}');
                string
            },
            Value::None => "None".into(),
        }
    }
}