use std::rc::Rc;

use crate::{
    bound::Path,
    interner::{interner, interner_mut},
    value::{ConstructorInstance, InstanceInstance, Value}
};

pub const INTRINSICS_MODULE_NAME: &str = "Intrinsics";
pub const INTRINSICS_FILE_PATH: &str = "./src/intrinsics.vay";

pub type IntrinsicFunction = fn(Vec<Value>) -> Value;

macro_rules! intrinsics_functions {
    ($($path:literal = $func:expr);*) => {
        &[$((concat!("Intrinsics", "::", $path), $func)),*]
    };
}

pub const INTRINSIC_FUNCTIONS: &[(&str, IntrinsicFunction)] = intrinsics_functions! {
    "U64::add" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a + b)
    };
    "U64::subtract" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a - b)
    };
    "U64::multiply" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a * b)
    };
    "U64::equals" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        let case = if a == b {
            interner().intern_idx("True")
        } else {
            interner().intern_idx("False")
        };

        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "U64::compare" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        let case = match a.cmp(&b) {
            std::cmp::Ordering::Less => interner().intern_idx("Less"),
            std::cmp::Ordering::Equal => interner().intern_idx("Equal"),
            std::cmp::Ordering::Greater => interner().intern_idx("Greater"),
        };

        let mut type_path = Path::empty();
        let ordering_type_path = "Core::Ordering";

        ordering_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "F32::add" = |mut arguments| {
        let b = arguments.pop().unwrap().into_f32();
        let a = arguments.pop().unwrap().into_f32();

        Value::F32(a + b)
    };
    "F32::subtract" = |mut arguments| {
        let b = arguments.pop().unwrap().into_f32();
        let a = arguments.pop().unwrap().into_f32();

        Value::F32(a - b)
    };
    "F32::multiply" = |mut arguments| {
        let b = arguments.pop().unwrap().into_f32();
        let a = arguments.pop().unwrap().into_f32();

        Value::F32(a * b)
    };
    "F32::equals" = |mut arguments| {
        let b = arguments.pop().unwrap().into_f32();
        let a = arguments.pop().unwrap().into_f32();

        let case = if a == b {
            interner().intern_idx("True")
        } else {
            interner().intern_idx("False")
        };

        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "F32::compare" = |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        let case = match a.cmp(&b) {
            std::cmp::Ordering::Less => interner().intern_idx("Less"),
            std::cmp::Ordering::Equal => interner().intern_idx("Equal"),
            std::cmp::Ordering::Greater => interner().intern_idx("Greater"),
        };

        let mut type_path = Path::empty();
        let ordering_type_path = "Core::Ordering";

        ordering_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "String::add" = |mut arguments| {
        let b = arguments.pop().unwrap().into_string();
        let a = arguments.pop().unwrap().into_string();

        let ab = {
            let interner = interner();

            let a = interner.get(&a);
            let b = interner.get(&b);

            let mut ab = String::from(a);
            ab.push_str(b);
            ab
        };

        let index = interner_mut().intern(ab);
        Value::String(index)
    };
    "String::equals" = |mut arguments| {
        let b = arguments.pop().unwrap().into_string();
        let a = arguments.pop().unwrap().into_string();

        let case = if a == b {
            interner().intern_idx("True")
        } else {
            interner().intern_idx("False")
        };

        let mut type_path = Path::empty();
        let bool_type_path = "Core::Bool";

        bool_type_path
            .split("::")
            .map(|part| type_path.push(interner().intern_idx(part)))
            .for_each(drop);

        let constructor = Rc::new(ConstructorInstance { type_path, case });
        let instance = InstanceInstance { constructor, values: vec![] };
        Value::Instance(Rc::new(instance))
    };
    "Array::length" = |mut arguments| {
        let array = arguments.pop().unwrap().into_array();

        let borrow = array.borrow();
        Value::U64(borrow.len() as u64)
    };
    "Array::append" = |mut arguments| {
        let value = arguments.pop().unwrap();
        let array = arguments.pop().unwrap().into_array();

        array.borrow_mut().push(value);

        Value::Unit
    };
    "Array::pop" = |mut arguments| {
        let array = arguments.pop().unwrap().into_array();

        let mut borrow = array.borrow_mut();
        borrow.pop().unwrap()
    }
};

pub const EXTERNAL_FUNCTIONS: &[(&str, IntrinsicFunction)] = &[
    ("Core::println", |mut arguments| {
        let a = arguments.pop().unwrap();
        println!("{}", a.as_string());

        Value::Unit
    })
];
