use crate::value::Value;

pub const PRIMITIVE_FUNCTIONS: &[(&str, fn(Vec<Value>) -> Value)] = &[
    (&"Primitive::I64::add", |mut _arguments| {
        println!("Merhaba");

        Value::Unit
    })
];