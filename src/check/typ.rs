use std::{collections::{HashMap, HashSet}, fmt::Display};

use crate::{resolution::bound::Path, interner::InternIdx};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Mono(Mono),
    Forall(Vec<Var>, Mono),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mono {
    Variant(Path, Vec<Mono>),
    Struct(Path, Vec<Mono>),
    Function(Function),
    Constant(Var),
    Var(Var),
    BuiltIn(Path, BuiltIn, Vec<Mono>),
    Unit,
    Bottom,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Function {
    arguments: Vec<Mono>,
    return_type: Box<Mono>,
}

impl Function {
    pub fn new(arguments: Vec<Mono>, return_type: Box<Mono>) -> Self {
        Self { arguments, return_type }
    }

    pub fn arguments(&self) -> &[Mono] {
        &self.arguments
    }

    pub fn return_type(&self) -> &Mono {
        &self.return_type
    }

    pub fn destruct(self) -> (Vec<Mono>, Box<Mono>) {
        (self.arguments, self.return_type)
    }
}

#[derive(Clone)]
pub struct Method {
    function_type: Function,
    constraints: HashMap<usize, HashSet<Path>>,
    type_vars: Vec<Var>
}

impl Method {
    pub fn new(
        function_type: Function,
        constraints: HashMap<usize, HashSet<Path>>,
        type_vars: Vec<Var>
    ) -> Self {
        Self { function_type, constraints, type_vars }
    }

    pub fn function_type(&self) -> &Function {
        &self.function_type
    }

    pub fn constraints(&self) -> &HashMap<usize, HashSet<Path>> {
        &self.constraints
    }

    pub fn type_vars(&self) -> &[Var] {
        &self.type_vars
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Interface {
    methods: HashMap<InternIdx, Function>,
}

impl Interface {
    pub fn new() -> Self {
        Self {
            methods: HashMap::new()
        }
    }

    pub fn methods(&self) -> &HashMap<InternIdx, Function> {
        &self.methods
    }

    pub fn methods_mut(&mut self) -> &mut HashMap<InternIdx, Function> {
        &mut self.methods
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Var {
    idx: usize,
    interfaces: HashSet<Path>
}

impl Var {
    pub fn new(idx: usize) -> Self {
        Self {
            idx,
            interfaces: HashSet::new()
        }
    }

    pub fn idx(&self) -> usize {
        self.idx
    }

    pub fn interfaces(&self) -> &HashSet<Path> {
        &self.interfaces
    }

    pub fn interfaces_mut(&mut self) -> &mut HashSet<Path> {
        &mut self.interfaces
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BuiltIn {
    U64,
    F32,
    Char,
    Array
}

impl Mono {
    pub fn into_function(self) -> Function {
        let Self::Function(function) = self else {
            panic!()
        };
        function
    }

    pub fn substitute(self, map: &HashMap<usize, Mono>) -> Mono {
        match self {
            Mono::Variant(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                Mono::Variant(path, arguments)
            },
            Mono::Struct(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                Mono::Struct(path, arguments)
            },
            Mono::BuiltIn(path, builtin, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                Mono::BuiltIn(path, builtin, arguments)
            },
            Mono::Function(function_type) => {
                let Function { arguments, return_type } = function_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.substitute(map))
                    .collect();

                let return_type = Box::new(return_type.substitute(map));

                let function = Function { arguments, return_type };
                Mono::Function(function)
            },
            Mono::Var(var) => {
                let Var { idx, .. } = var;

                if let Some(m) = map.get(&idx).cloned() {
                    m.substitute(map)
                } else {
                    Mono::Var(var)
                }
            },
            Mono::Constant(c) => Mono::Constant(c),
            Mono::Unit => Mono::Unit,
            Mono::Bottom => Mono::Bottom
        }
    }

    pub fn replace_type_constants(self, map: &HashMap<usize, Mono>) -> Mono {
        match self {
            Mono::Variant(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                Mono::Variant(path, arguments)
            },
            Mono::Struct(path, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                Mono::Struct(path, arguments)
            },
            Mono::BuiltIn(path, builtin, arguments) => {
                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                Mono::BuiltIn(path, builtin, arguments)
            },
            Mono::Function(function_type) => {
                let Function { arguments, return_type } = function_type;

                let arguments = arguments
                    .into_iter()
                    .map(|arg| arg.replace_type_constants(map))
                    .collect();

                let return_type = Box::new(return_type.replace_type_constants(map));

                let function = Function { arguments, return_type };
                Mono::Function(function)
            },
            Mono::Var(var) => Mono::Var(var),
            Mono::Constant(c) => {
                let Var { idx, .. } = c;

                let Some(t) = map.get(&idx).cloned() else {
                    panic!()
                };

                t.substitute(map)
            },
            Mono::Unit => Mono::Unit,
            Mono::Bottom => Mono::Bottom
        }
    }

    pub fn occuring_type_vars(&self) -> Vec<Var> {
        fn collect_type_vars(m: &Mono, vars: &mut Vec<Var>) {
            match m {
                Mono::Variant(_, arguments) |
                Mono::Struct(_, arguments) |
                Mono::BuiltIn(_, _, arguments) => {
                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }
                },
                Mono::Function(t) => {
                    let Function { arguments, return_type } = t;

                    for argument in arguments {
                        collect_type_vars(argument, vars);
                    }

                    collect_type_vars(return_type, vars);
                },
                Mono::Var(var) => vars.push(var.clone()),

                Mono::Constant(_) |
                Mono::Unit |
                Mono::Bottom => ()
            }
        }

        let mut type_vars = vec![];
        collect_type_vars(self, &mut type_vars);
        type_vars
    }

    pub fn occurs(&self, idx: usize) -> bool {
        match self {
            Mono::Variant(_, arguments) |
            Mono::Struct(_, arguments) |
            Mono::BuiltIn(_, _, arguments) => {
                arguments.iter().any(|t| t.occurs(idx))
            },
            Mono::Function(function) => {
                let Function { arguments, return_type } = function;

                arguments.iter().any(|t| t.occurs(idx)) ||
                return_type.occurs(idx)
            },
            Mono::Var(var) => var.idx == idx,

            Mono::Constant(_) |
            Mono::Unit |
            Mono::Bottom => false
        }
    }
}

impl Display for Mono {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mono::Variant(path, arguments) |
            Mono::Struct(path, arguments) => {
                path.fmt(f)?;
                match &arguments[..] {
                    [] => Ok(()),
                    [typ] => write!(f, "({})", typ),
                    [init @ .., last] => {
                        write!(f, "(")?;
                        for t in init {
                            write!(f, "{}, ", t)?;
                        }
                        write!(f, "{})", last)
                    }
                }
            },
            Mono::BuiltIn(_, builtin, arguments) => {
                write!(f, "{}", builtin)?;
                match &arguments[..] {
                    [] => Ok(()),
                    [typ] => write!(f, "({})", typ),
                    [init @ .., last] => {
                        write!(f, "(")?;
                        for t in init {
                            write!(f, "{}, ", t)?;
                        }
                        write!(f, "{})", last)
                    }
                }
            },
            Mono::Function(function) => {
                let Function { arguments, return_type } = function;

                write!(f, "fun(")?;
                match &arguments[..] {
                    [] => write!(f, ")")?,
                    [typ] => write!(f, "{})", typ)?,
                    [init @ .., last] => {
                        for t in init {
                            write!(f, "{}, ", t)?;
                        }
                        write!(f, "{})", last)?;
                    }
                };
                write!(f, " : {}", return_type)
            }
            Mono::Var(type_var) => write!(f, "a{} ({})", type_var.idx, type_var.interfaces
                .iter()
                .map(|path| path.to_string())
                .collect::<Vec<_>>()
                .join(",")
            ),
            Mono::Constant(type_var) => write!(f, "c{} ({})", type_var.idx, type_var.interfaces
                .iter()
                .map(|path| path.to_string())
                .collect::<Vec<_>>()
                .join(",")
            ),
            Mono::Unit => write!(f, "()"),
            Mono::Bottom => write!(f, "Bottom")
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Mono(t) => t.fmt(f),
            Type::Forall(vars, ty) => {
                write!(f,
                    "forall {}; {}",
                    vars.iter().map(|var| format!("a{} ({})", var.idx,
                        var.interfaces
                            .iter().map(|path| path.to_string())
                            .collect::<Vec<_>>().join(","))
                        )
                        .collect::<Vec<_>>().join(","),
                    ty,
                )
            }
        }
    }
}

impl Display for BuiltIn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltIn::U64 => write!(f, "U64"),
            BuiltIn::F32 => write!(f, "F32"),
            BuiltIn::Char => write!(f, "Char"),
            BuiltIn::Array => write!(f, "Array"),
        }
    }
}