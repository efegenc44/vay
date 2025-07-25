use std::collections::HashMap;

use crate::{bound::Path, interner::{InternIdx, Interner}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Mono(MonoType),
    Forall(Vec<TypeVar>, Box<MonoType>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MonoType {
    Variant(Path, Vec<MonoType>),
    Procedure(ProcedureType),
    Constant(TypeVar),
    Var(TypeVar)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcedureType {
    pub arguments: Vec<MonoType>,
    pub return_type: Box<MonoType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub methods: HashMap<InternIdx, ProcedureType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVar {
    // TODO: Can probably unify idx and instance
    pub idx: usize,
    pub instance: usize,
    pub methods: HashMap<InternIdx, ProcedureType>,
}

impl MonoType {
    pub fn display(&self, interner: &Interner) -> String {
        match self {
            MonoType::Variant(path, arguments) => {
                let mut type_string = path.as_string(interner);
                match &arguments[..] {
                    [] => (),
                    [typ] => {
                        type_string.push_str(&format!("({})", typ.display(interner)));
                    }
                    [init @ .., last] => {
                        type_string.push('(');
                        for t in init {
                            type_string.push_str(&format!("{}, ", t.display(interner)));
                        }
                        type_string.push_str(&format!("{})", last.display(interner)));
                    }
                };
                type_string
            },
            MonoType::Procedure(procedure) => {
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
            MonoType::Var(type_var) => format!("a{}", type_var.idx),
            MonoType::Constant(type_var) => format!("c{}", type_var.idx),
        }
    }
}

impl Type {
    pub fn display(&self, interner: &Interner) -> String {
        match self {
            Type::Mono(t) => t.display(interner),
            Type::Forall(vars, ty) => {
                format!(
                    "forall {}; {}",
                    vars.iter().map(|id| format!("a{}", id.idx)).collect::<Vec<_>>().join(","),
                    ty.display(interner),
                )
            }
        }
    }
}