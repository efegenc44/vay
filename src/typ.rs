use std::{collections::{HashMap, HashSet}, fmt::Display};

use crate::{bound::Path, interner::{InternIdx, Interner}};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Mono(MonoType),
    Forall(Vec<TypeVar>, MonoType),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MonoType {
    Variant(Path, Vec<MonoType>),
    Struct(Path, Vec<MonoType>),
    Function(FunctionType),
    Constant(TypeVar),
    Var(TypeVar),
    BuiltIn(Path, BuiltInType, Vec<MonoType>),
    Unit,
    Bottom,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub arguments: Vec<MonoType>,
    pub return_type: Box<MonoType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    pub methods: HashMap<InternIdx, FunctionType>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeVar {
    pub idx: usize,
    pub interfaces: HashSet<Path>
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltInType {
    U64,
}

impl MonoType {
    pub fn into_function(self) -> FunctionType {
        let Self::Function(function) = self else {
            panic!()
        };
        function
    }

    pub fn substitute(self, map: &HashMap<usize, MonoType>) -> MonoType {
        match self {
            MonoType::Variant(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                MonoType::Variant(path, arguments)
            },
            MonoType::Struct(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                MonoType::Struct(path, arguments)
            },
            MonoType::BuiltIn(path, builtin, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                MonoType::BuiltIn(path, builtin, arguments)
            },
            MonoType::Function(function_type) => {
                let FunctionType { arguments, return_type } = function_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                let return_type = Box::new(return_type.substitute(map));

                let function = FunctionType { arguments, return_type };
                MonoType::Function(function)
            },
            MonoType::Var(var) => {
                let TypeVar { idx, .. } = var;

                if let Some(m) = map.get(&idx).cloned() {
                    m.substitute(map)
                } else {
                    MonoType::Var(var)
                }
            },
            MonoType::Constant(c) => MonoType::Constant(c),
            MonoType::Unit => MonoType::Unit,
            MonoType::Bottom => MonoType::Bottom
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
            MonoType::Struct(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                MonoType::Struct(path, arguments)
            },
            MonoType::BuiltIn(path, builtin, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                MonoType::BuiltIn(path, builtin, arguments)
            },
            MonoType::Function(function_type) => {
                let FunctionType { arguments, return_type } = function_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                let return_type = Box::new(return_type.replace_type_constants(map));

                let function = FunctionType { arguments, return_type };
                MonoType::Function(function)
            },
            MonoType::Var(var) => MonoType::Var(var),
            MonoType::Constant(c) => {
                let TypeVar { idx, .. } = c;

                let Some(t) = map.get(&idx).cloned() else {
                    panic!()
                };

                t.substitute(map)
            },
            MonoType::Unit => MonoType::Unit,
            MonoType::Bottom => MonoType::Bottom
        }
    }

    pub fn occuring_type_vars(&self) -> Vec<TypeVar> {
        fn collect_type_vars(m: &MonoType, vars: &mut Vec<TypeVar>) {
            match m {
                MonoType::Variant(_, arguments) => {
                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }
                },
                MonoType::Struct(_, arguments) => {
                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }
                },
                MonoType::BuiltIn(_, _, arguments) => {
                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }
                },
                MonoType::Function(t) => {
                    let FunctionType { arguments, return_type } = t;

                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }
                    collect_type_vars(return_type, vars);
                },
                MonoType::Constant(_) => (),
                MonoType::Var(var) => vars.push(var.clone()),
                MonoType::Unit => (),
                MonoType::Bottom => ()
            }
        }

        let mut type_vars = vec![];
        collect_type_vars(self, &mut type_vars);
        type_vars
    }

    pub fn occurs(&self, idx: usize) -> bool {
        match self {
            MonoType::Variant(_, arguments) => {
                arguments.iter().any(|t| t.occurs(idx))
            },
            MonoType::Struct(_, arguments) => {
                arguments.iter().any(|t| t.occurs(idx))
            },
            MonoType::BuiltIn(_, _, arguments) => {
                arguments.iter().any(|t| t.occurs(idx))
            },
            MonoType::Function(function) => {
                let FunctionType { arguments, return_type } = function;

                arguments.iter().any(|t| t.occurs(idx)) ||
                return_type.occurs(idx)
            },
            MonoType::Var(var) => var.idx == idx,
            MonoType::Constant(_) => false,
            MonoType::Unit => false,
            MonoType::Bottom => false
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
            MonoType::Struct(path, arguments) => {
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
            MonoType::BuiltIn(_, builtin, arguments) => {
                let mut type_string = builtin.to_string();
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
            MonoType::Function(function) => {
                let FunctionType { arguments, return_type } = function;

                let mut type_string = String::from("fun(");
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
            MonoType::Var(type_var) => format!("a{} ({})", type_var.idx, type_var.interfaces
                .iter().map(|path| path.as_string(interner))
                .collect::<Vec<_>>().join(",")
            ),
            MonoType::Constant(type_var) => format!("c{} ({})", type_var.idx, type_var.interfaces
                .iter().map(|path| path.as_string(interner))
                .collect::<Vec<_>>().join(",")
            ),
            MonoType::Unit => "()".into(),
            MonoType::Bottom => "Bottom".into()
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
                    vars.iter().map(|var| format!("a{} ({})", var.idx,
                        var.interfaces
                            .iter().map(|path| path.as_string(interner))
                            .collect::<Vec<_>>().join(","))
                        )
                        .collect::<Vec<_>>().join(","),
                    ty.display(interner),
                )
            }
        }
    }
}

impl Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltInType::U64 => write!(f, "U64"),
        }
    }
}