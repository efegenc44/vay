use std::rc::Rc;

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
    },
    Instance {
        type_path: Path,
        case: InternIdx,
        values: Rc<Vec<Value>>
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

                let mut string = format!("{}(", interner.get(case));
                let mut first = true;
                for value in values.iter() {
                    if first {
                        first = false;
                    } else {
                        string.push_str(", ");
                    }
                    string.push_str(&format!("{}", value.as_string(interner)));
                }
                string.push(')');
                string
            },
            Value::None => "None".into(),
        }
    }
}