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
    pub fn replace_type_vars(self, map: &HashMap<usize, MonoType>) -> MonoType {
        match self {
            MonoType::Variant(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_vars(map))
                    .collect();

                MonoType::Variant(path, arguments)
            },
            MonoType::Procedure(procedure_type) => {
                let ProcedureType { arguments, return_type } = procedure_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_vars(map))
                    .collect();

                let return_type = Box::new(return_type.replace_type_vars(map));

                let procedure = ProcedureType { arguments, return_type };
                MonoType::Procedure(procedure)
            },
            MonoType::Var(var) => {
                let TypeVar { idx, .. } = var;

                if let Some(t) = map.get(&idx).cloned() {
                    t.replace_type_vars(map)
                } else {
                    MonoType::Var(var)
                }
            },
            MonoType::Constant(c) => MonoType::Constant(c),
        }
    }

    pub fn replace_type_constants(self, map: &HashMap<usize, MonoType>) -> MonoType {
        match self {
            MonoType::Variant(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                MonoType::Variant(path, arguments)
            },
            MonoType::Procedure(procedure_type) => {
                let ProcedureType { arguments, return_type } = procedure_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                let return_type = Box::new(return_type.replace_type_constants(map));

                let procedure = ProcedureType { arguments, return_type };
                MonoType::Procedure(procedure)
            },
            MonoType::Var(var) => MonoType::Var(var),
            MonoType::Constant(c) => {
                let TypeVar { idx, .. } = c;

                let Some(t) = map.get(&idx).cloned() else {
                    panic!()
                };

                t.replace_type_vars(map)
            },
        }
    }

    pub fn contains_type_var(&self) -> bool {
        match self {
            MonoType::Variant(_, arguments) => {
                arguments.iter().any(Self::contains_type_var)
            },
            MonoType::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                arguments.iter().any(Self::contains_type_var) ||
                return_type.contains_type_var()
            },
            MonoType::Var(_) => true,
            MonoType::Constant(_) => false,
        }
    }

    pub fn occurs(&self, idx: usize) -> bool {
        match self {
            MonoType::Variant(_, arguments) => {
                arguments.iter().any(|t| t.occurs(idx))
            },
            MonoType::Procedure(procedure) => {
                let ProcedureType { arguments, return_type } = procedure;

                arguments.iter().any(|t| t.occurs(idx)) ||
                return_type.occurs(idx)
            },
            MonoType::Var(var) => var.idx == idx,
            MonoType::Constant(_) => false,
        }
    }

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