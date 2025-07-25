use crate::{bound::Bound, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Expression {
    Path(PathExpression),
    Application(ApplicationExpression),
    Projection(ProjectionExpression)
}

#[derive(Clone)]
pub struct PathExpression {
    pub parts: Vec<InternIdx>,
    pub bound: Bound
}

#[derive(Clone)]
pub struct ApplicationExpression {
    pub function: Box<Located<Expression>>,
    pub arguments: Vec<Located<Expression>>,
}

#[derive(Clone)]
pub struct ProjectionExpression {
    pub expression: Box<Located<Expression>>,
    pub name: Located<InternIdx>
}

#[derive(Clone)]
pub enum TypeExpression {
    Path(PathTypeExpression),
    Procedure(ProcedureTypeExpression),
    Application(TypeApplicationExpression)
}

#[derive(Clone)]
pub struct PathTypeExpression {
    pub parts: Vec<InternIdx>,
    pub bound: Bound
}

#[derive(Clone)]
pub struct ProcedureTypeExpression {
    pub arguments: Vec<Located<TypeExpression>>,
    pub return_type: Box<Located<TypeExpression>>
}

#[derive(Clone)]
pub struct TypeApplicationExpression {
    pub function: Box<Located<TypeExpression>>,
    pub arguments: Vec<Located<TypeExpression>>,
}

impl TypeExpression {
    pub fn contains_application(&self) -> bool {
        match self {
            TypeExpression::Path(_) => false,
            TypeExpression::Procedure(procedure) => {
                let ProcedureTypeExpression { arguments, return_type } = procedure;

                arguments.iter().any(|arg| arg.data().contains_application()) ||
                return_type.data().contains_application()
            },
            TypeExpression::Application(_) => true,
        }
    }
}