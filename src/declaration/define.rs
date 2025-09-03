use crate::{
    bound::Path,
    expression::Expression,
    interner::InternIdx,
    location::Located,
    type_expression::TypeExpression
};

pub struct T {
    name: Located<InternIdx>,
    type_expression: Located<TypeExpression>,
    expression: Located<Expression>,
    path: Path
}

impl T {
    pub fn new(
        name: Located<InternIdx>,
        type_expression: Located<TypeExpression>,
        expression: Located<Expression>
    ) -> Self {
        Self {
            name,
            type_expression,
            expression,
            path: Path::empty()
        }
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn type_expression(&self) -> &Located<TypeExpression> {
        &self.type_expression
    }

    pub fn type_expression_mut(&mut self) -> &mut Located<TypeExpression> {
        &mut self.type_expression
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn set_path(&mut self, path: Path) {
        self.path = path;
    }
}