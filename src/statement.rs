use crate::{expression::Expression, location::Located};

pub enum Statement {
    Expression(Located<Expression>),
    Return(Located<Expression>),
}
