use crate::{interner::InternIdx, location::Located};

// TODO: default pattern
#[derive(Clone)]
pub enum Pattern {
    Any(InternIdx),
    U64(u64),
    F32(f32),
    String(InternIdx),
    Char(char),
    Array(Array),
    VariantCase(VariantCase),
    Unit
}


#[derive(Clone)]
pub struct Array {
    before: Vec<Located<Pattern>>,
    after: Vec<Located<Pattern>>,
    rest: Option<InternIdx>,
}

impl Array {
    pub fn new(
        before: Vec<Located<Pattern>>,
        after: Vec<Located<Pattern>>,
        rest: Option<InternIdx>
    ) -> Self {
        Self { before, after, rest }
    }

    pub fn before(&self) -> &[Located<Pattern>] {
        &self.before
    }

    pub fn after(&self) -> &[Located<Pattern>] {
        &self.after
    }

    pub fn rest(&self) -> Option<InternIdx> {
        self.rest
    }
}

#[derive(Clone)]
pub struct VariantCase {
    case: Located<InternIdx>,
    fields: Option<Vec<Located<Pattern>>>,
}

impl VariantCase {
    pub fn new(case: Located<InternIdx>, fields: Option<Vec<Located<Pattern>>>) -> Self {
        Self { case, fields }
    }

    pub fn case(&self) -> Located<InternIdx> {
        self.case
    }

    pub fn fields(&self) -> Option<&Vec<Located<Pattern>>> {
        self.fields.as_ref()
    }
}
