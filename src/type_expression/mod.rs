mod path;
mod function;
mod application;

pub type Path        = path::T;
pub type Application = application::T;
pub type Function    = function::T;

#[derive(Clone)]
pub enum TypeExpression {
    Path(Path),
    Function(Function),
    Application(Application),
    Unit
}

