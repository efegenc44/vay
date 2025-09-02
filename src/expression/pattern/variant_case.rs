use crate::{expression::pattern::Pattern, interner::InternIdx, location::Located};

#[derive(Clone)]
pub struct T {
    case: Located<InternIdx>,
    fields: Option<Vec<Located<Pattern>>>,
}

impl T {
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