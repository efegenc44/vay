use crate::value::Value;

pub const PRIMITIVE_FUNCTIONS: &[(&str, fn(Vec<Value>) -> Value)] = &[
    (&"Primitive::U64::add", |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a + b)
    })
];