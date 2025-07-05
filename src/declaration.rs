use crate::{
    expression::TypeExpression, interner::InternIdx, location::Located, statement::Statement,
};

pub enum Declaration {
    Procedure {
        name: Located<InternIdx>,
        arguments: Vec<Located<TypedIdentifier>>,
        body: Vec<Located<Statement>>,
    },
    Variant {
        name: Located<InternIdx>,
        cases: Vec<Located<VariantCase>>,
    },
}

pub struct VariantCase {
    identifier: Located<InternIdx>,
    arguments: Option<Vec<Located<TypedIdentifier>>>,
}

impl VariantCase {
    pub fn new(name: Located<InternIdx>, arguments: Option<Vec<Located<TypedIdentifier>>>) -> Self {
        Self {
            identifier: name,
            arguments,
        }
    }
}

pub struct TypedIdentifier {
    identifier: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
}

impl TypedIdentifier {
    pub fn new(identifier: Located<InternIdx>, type_expression: Located<TypeExpression>) -> Self {
        Self {
            identifier,
            type_expression,
        }
    }
}
