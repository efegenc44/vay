use std::rc::Rc;

use crate::{
    name::bound::Path,
    interner::interner,
    interpret::value::{
        ConstructorInstance,
        InstanceInstance,
        Value
    }
};

pub const INTRINSICS_MODULE_NAME: &str = "Intrinsics";
pub const INTRINSICS_FILE_PATH: &str = "./src/vay/intrinsics.vay";

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
    "U64::toF32" = |mut arguments| {
        let a = arguments.pop().unwrap().into_u64();

        Value::F32(a as f32)
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
    "F32::divide" = |mut arguments| {
        let b = arguments.pop().unwrap().into_f32();
        let a = arguments.pop().unwrap().into_f32();

        Value::F32(a / b)
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
    "Char::equals" = |mut arguments| {
        let b = arguments.pop().unwrap().into_char();
        let a = arguments.pop().unwrap().into_char();

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
    };
    "Array::get" = |mut arguments| {
        let index = arguments.pop().unwrap().into_u64();
        let array = arguments.pop().unwrap().into_array();

        let borrow = array.borrow();
        borrow[index as usize].clone()
    }
};

pub const EXTERNAL_FUNCTIONS: &[(&str, IntrinsicFunction)] = &[
    ("Core::println", |mut arguments| {
        println!("{}", arguments.pop().unwrap());

        Value::Unit
    })
];
