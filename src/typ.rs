use crate::{bound::Path, interner::Interner};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Variant(Path),
    Procedure(ProcedureType)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcedureType {
    pub arguments: Vec<Type>,
    pub return_type: Box<Type>,
}

impl Type {
    pub fn display(&self, interner: &Interner) -> String {
        match self {
            Type::Variant(path) => path.as_string(interner),
            Type::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                let mut type_string = String::from("proc(");
                match &arguments[..] {
                    [] => type_string.push(')'),
                    [typ] => {
                        type_string.push_str(&format!("{})", typ.display(interner)));
                    }
                    [init @ .., last] => {
                        for t in init {
                            type_string.push_str(&format!("{}, ", t.display(interner)));
                        }
                        type_string.push_str(&format!("{})", last.display(interner)));
                    }
                };
                type_string.push_str(&format!(" -> {}", return_type.display(interner)));
                type_string
            }
        }
    }
}
