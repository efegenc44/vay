use crate::{bound::Bound, interner::InternIdx, location::Located};

pub enum Expression {
    Path(Vec<InternIdx>, Bound),
    Application {
        function: Box<Located<Expression>>,
        arguments: Vec<Located<Expression>>,
    },
}

pub enum TypeExpression {
    Path(Vec<InternIdx>, Bound),
    Procedure {
        arguments: Vec<Located<TypeExpression>>,
        return_type: Box<Located<TypeExpression>>
    }
}
