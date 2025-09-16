use crate::{
    resolution::bound::Bound,
    interner::InternIdx,
    lex::location::Located,
    ast::pattern::Pattern
};

#[derive(Clone)]
pub enum Expression {
    U64(u64),
    F32(f32),
    String(InternIdx),
    Char(char),
    Path(Path),
    Array(Array),
    Application(Application),
    Projection(Projection),
    Let(Let),
    Sequence(Sequence),
    Block(Block),
    Lambda(Lambda),
    Match(Match),
    Return(Return),
    Assignment(Assignment),
    While(While),
    Continue,
    Break,
}

#[derive(Clone)]
pub struct Path {
    parts: Vec<InternIdx>,
    bound: Bound
}

impl Path {
    pub fn new(parts: Vec<InternIdx>) -> Self {
        Self {
            parts,
            bound: Bound::Undetermined
        }
    }

    pub fn parts(&self) -> &[InternIdx] {
        &self.parts
    }

    pub fn bound(&self) -> &Bound {
        &self.bound
    }

    pub fn set_bound(&mut self, bound: Bound) {
        self.bound = bound;
    }
}

#[derive(Clone)]
pub struct Array {
    expressions: Vec<Located<Expression>>
}

impl Array {
    pub fn new(expressions: Vec<Located<Expression>>) -> Self {
        Self { expressions }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }
}

#[derive(Clone)]
pub struct Application {
    function: Box<Located<Expression>>,
    arguments: Vec<Located<Expression>>,
}

impl Application {
    pub fn new(function: Box<Located<Expression>>, arguments: Vec<Located<Expression>>) -> Self {
        Self { function, arguments }
    }

    pub fn function(&self) -> &Located<Expression> {
        &self.function
    }

    pub fn function_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.function
    }

    pub fn arguments(&self) -> &[Located<Expression>] {
        &self.arguments
    }

    pub fn arguments_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.arguments
    }
}

#[derive(Clone)]
pub struct Projection {
    expression: Box<Located<Expression>>,
    projected: Located<InternIdx>
}

impl Projection {
    pub fn new(expression: Box<Located<Expression>>, projected: Located<InternIdx>) -> Self {
        Self { expression, projected }
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }

    pub fn projected(&self) -> Located<InternIdx> {
        self.projected
    }
}

#[derive(Clone)]
pub struct Let {
    identifier: Located<InternIdx>,
    value_expression: Box<Located<Expression>>,
    body_expression: Box<Located<Expression>>,
}

impl Let {
    pub fn new(
        identifier: Located<InternIdx>,
        value_expression: Box<Located<Expression>>,
        body_expression: Box<Located<Expression>>
    ) -> Self {
        Self { identifier, value_expression, body_expression }
    }

    pub fn identifier(&self) -> Located<InternIdx> {
        self.identifier
    }

    pub fn value_expression(&self) -> &Located<Expression> {
        &self.value_expression
    }

    pub fn value_expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.value_expression
    }

    pub fn body_expression(&self) -> &Located<Expression> {
        &self.body_expression
    }

    pub fn body_expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body_expression
    }
}

#[derive(Clone)]
pub struct Sequence {
    expressions: Vec<Located<Expression>>,
}

impl Sequence {
    pub fn new(expressions: Vec<Located<Expression>>) -> Self {
        Self { expressions }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }
}

#[derive(Clone)]
pub struct Block {
    expressions: Vec<Located<Expression>>,
}

impl Block {
    pub fn new(expressions: Vec<Located<Expression>>) -> Self {
        Self { expressions }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }
}

#[derive(Clone)]
pub struct Lambda {
    arguments: Vec<Located<InternIdx>>,
    body: Box<Located<Expression>>,
}

impl Lambda {
    pub fn new(arguments: Vec<Located<InternIdx>>, body: Box<Located<Expression>>) -> Self {
        Self { arguments, body }
    }

    pub fn arguments(&self) -> &[Located<InternIdx>] {
        &self.arguments
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body
    }
}

#[derive(Clone)]
pub struct Match {
    expressions: Vec<Located<Expression>>,
    branches: Vec<Located<MatchBranch>>,
}

impl Match {
    pub fn new(expressions: Vec<Located<Expression>>, branches: Vec<Located<MatchBranch>>) -> Self {
        Self { expressions, branches }
    }

    pub fn expressions(&self) -> &[Located<Expression>] {
        &self.expressions
    }

    pub fn expressions_mut(&mut self) -> &mut Vec<Located<Expression>> {
        &mut self.expressions
    }

    pub fn branches(&self) -> &[Located<MatchBranch>] {
        &self.branches
    }

    pub fn branches_mut(&mut self) -> &mut Vec<Located<MatchBranch>> {
        &mut self.branches
    }
}

#[derive(Clone)]
pub struct MatchBranch {
    patterns: Vec<Located<Pattern>>,
    expression: Located<Expression>,
}

impl MatchBranch {
    pub fn new(patterns: Vec<Located<Pattern>>, expression: Located<Expression>) -> Self {
        Self { patterns, expression }
    }

    pub fn patterns(&self) -> &[Located<Pattern>] {
        &self.patterns
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Located<Expression> {
        &mut self.expression
    }
}

#[derive(Clone)]
pub struct Return {
    expression: Box<Located<Expression>>,
}

impl Return {
    pub fn new(expression: Box<Located<Expression>>) -> Self {
        Self { expression }
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }
}

#[derive(Clone)]
pub struct Assignment {
    assignable: Box<Located<Expression>>,
    expression: Box<Located<Expression>>,
}

impl Assignment {
    pub fn new(assignable: Box<Located<Expression>>, expression: Box<Located<Expression>>) -> Self {
        Self { assignable, expression }
    }

    pub fn assignable(&self) -> &Located<Expression> {
        &self.assignable
    }

    pub fn assignable_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.assignable
    }

    pub fn expression(&self) -> &Located<Expression> {
        &self.expression
    }

    pub fn expression_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.expression
    }
}

#[derive(Clone)]
pub struct While {
    condition: Box<Located<Expression>>,
    post: Option<Box<Located<Expression>>>,
    body: Box<Located<Expression>>
}

impl While {
    pub fn new(
        condition: Box<Located<Expression>>,
        post: Option<Box<Located<Expression>>>,
        body: Box<Located<Expression>>
    ) -> Self {
        Self { condition, post, body }
    }

    pub fn condition(&self) -> &Located<Expression> {
        &self.condition
    }

    pub fn condition_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.condition
    }

    pub fn post(&self) -> Option<&Box<Located<Expression>>> {
        self.post.as_ref()
    }

    pub fn post_mut(&mut self) -> &mut Option<Box<Located<Expression>>> {
        &mut self.post
    }

    pub fn body(&self) -> &Located<Expression> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Box<Located<Expression>> {
        &mut self.body
    }
}
