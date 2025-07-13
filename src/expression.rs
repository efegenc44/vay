use crate::{bound::Bound, interner::InternIdx, location::Located};

#[derive(Clone)]
pub enum Expression {
    Path(Vec<InternIdx>, Bound),
    Application {
        function: Box<Located<Expression>>,
        arguments: Vec<Located<Expression>>,
    },
    Projection {
        expression: Box<Located<Expression>>,
        name: Located<InternIdx>
    },
}

#[derive(Clone)]
pub enum TypeExpression {
    Path(Vec<InternIdx>, Bound),
    Procedure {
        arguments: Vec<Located<TypeExpression>>,
        return_type: Box<Located<TypeExpression>>
    }
}
