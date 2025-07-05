use crate::{bound::Bound, interner::InternIdx};

pub enum Expression {
    Path(Vec<InternIdx>, Bound),
}

pub enum TypeExpression {
    Path(Vec<InternIdx>, Bound),
}
