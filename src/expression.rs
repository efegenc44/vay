use crate::interner::InternIdx;

pub enum Expression {
    Identifier(InternIdx),
}

pub enum TypeExpression {
    Identifier(InternIdx),
}
