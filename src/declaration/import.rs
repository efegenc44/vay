use crate::{interner::InternIdx, location::Located};

pub struct T {
    import_in: bool,
    name: Located<InternIdx>,
    subnames: Option<Vec<T>>,
    as_name: Option<Located<InternIdx>>
}

impl T {
    pub fn new(
        import_in: bool,
        name: Located<InternIdx>,
        subnames: Option<Vec<T>>,
        as_name: Option<Located<InternIdx>>
    ) -> Self {
        Self { import_in, name, subnames, as_name }
    }

    pub fn import_in(&self) -> bool {
        self.import_in
    }

    pub fn name(&self) -> Located<InternIdx> {
        self.name
    }

    pub fn subnames(&self) -> Option<&Vec<T>> {
        self.subnames.as_ref()
    }

    pub fn as_name(&self) -> Option<Located<InternIdx>> {
        self.as_name
    }
}
