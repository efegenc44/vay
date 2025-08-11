use std::rc::Rc;

use crate::{interner::Interner, value::{Value, ConstructorInstance, InstanceInstance}, bound::Path};

pub const INTRINSICS_MODULE_NAME: &str = "Intrinsics";
pub const INTRINSICS_FILE_PATH: &str = "./src/intrinsics.vay";

pub type IntrinsicFunction = fn(Vec<Value>, &mut Interner) -> Value;

macro_rules! intrinsics_functions {
    ($($path:literal = $func:expr);*) => {
        &[$((concat!("Intrinsics", "::", $path), $func)),*]
    };
}

pub const INTRINSIC_FUNCTIONS: &[(&str, IntrinsicFunction)] = intrinsics_functions! {
    "U64::add" = |mut arguments, _| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a + b)
    };
    "U64::subtract" = |mut arguments, _| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a - b)
    };
    "U64::multiply" = |mut arguments, _| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a * b)
    };
    "U64::equals" = |mut arguments, interner| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        let case = if a == b {
            interner.intern_idx("True")
        } else {
            interner.intern_idx("False")
        };

        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner.intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "U64::compare" = |mut arguments, interner| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        let case = match a.cmp(&b) {
            std::cmp::Ordering::Less => interner.intern_idx("Less"),
            std::cmp::Ordering::Equal => interner.intern_idx("Equal"),
            std::cmp::Ordering::Greater => interner.intern_idx("Greater"),
        };

        let mut type_path = Path::empty();
        let ordering_type_path = "Core::Ordering";

        ordering_type_path
            .split("::")
            .map(|part| type_path.push(interner.intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "String::add" = |mut arguments, interner| {
        let b = arguments.pop().unwrap().into_string();
        let a = arguments.pop().unwrap().into_string();

        let a = interner.get(&a);
        let b = interner.get(&b);

        let mut ab = String::from(a);
        ab.push_str(b);

        let index = interner.intern(ab);
        Value::String(index)
    };
    "String::equals" = |mut arguments, interner| {
        let b = arguments.pop().unwrap().into_string();
        let a = arguments.pop().unwrap().into_string();

        let case = if a == b {
            interner.intern_idx("True")
        } else {
            interner.intern_idx("False")
        };

        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner.intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    }
};

pub const EXTERNAL_FUNCTIONS: &[(&str, IntrinsicFunction)] = &[
    ("Core::println", |mut arguments, interner| {
        let a = arguments.pop().unwrap();
        println!("{}", a.as_string(interner));

        Value::Unit
    })
];
