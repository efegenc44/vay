use std::collections::HashMap;

use crate::{
    interner::{interner, InternIdx},
    lex::location::SourceLocation,
    resolution::bound::Path,
    vay::{
        core::CORE_MODULE_NAME,
        intrinsics::INTRINSICS_MODULE_NAME
    }
};

pub struct ModuleInformation {
    imports: HashMap<InternIdx, (Path, SourceLocation)>,
    import_ins: HashMap<Path, SourceLocation>,
    path_location: SourceLocation,
}

impl ModuleInformation {
    pub fn new() -> Self {
        // NOTE : Automatically import Core and Intrinsics for all modules
        //   locations are dummy but they supposed to be present at this point
        //   so no error.
        let instrinsics = interner().intern_idx(INTRINSICS_MODULE_NAME);
        let intrinsics_path = Path::empty().append(instrinsics);

        let core = interner().intern_idx(CORE_MODULE_NAME);
        let core_path = Path::empty().append(core);

        let boole = interner().intern_idx("Bool");
        let boole_path = core_path.append(boole);

        let dummyloc = SourceLocation::dummy();

        let imports = HashMap::from([
            (instrinsics, (intrinsics_path.clone(), dummyloc)),
            (core, (core_path.clone(), dummyloc))
        ]);

        let import_ins = HashMap::from([
            (intrinsics_path.clone(), dummyloc),
            (core_path.clone(), dummyloc),
            (boole_path.clone(), dummyloc)
        ]);

        Self {
            imports,
            import_ins,
            path_location: SourceLocation::dummy(),
        }
    }

    pub fn imports(&self) -> &HashMap<InternIdx, (Path, SourceLocation)> {
        &self.imports
    }

    pub fn imports_mut(&mut self) -> &mut HashMap<InternIdx, (Path, SourceLocation)> {
        &mut self.imports
    }

    pub fn import_ins(&self) -> &HashMap<Path, SourceLocation> {
        &self.import_ins
    }

    pub fn import_ins_mut(&mut self) -> &mut HashMap<Path, SourceLocation> {
        &mut self.import_ins
    }

    pub fn path_location(&self) -> SourceLocation {
        self.path_location
    }

    pub fn set_path_location(&mut self, path_location: SourceLocation) {
        self.path_location = path_location;
    }
}

