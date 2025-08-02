use crate::value::Value;

pub const INTRINSICS_MODULE_NAME: &str = "Intrinsics";
pub const INTRINSICS_FILE_PATH: &str = "./src/intrinsics.vay";

pub type IntrinsicFunction = fn(Vec<Value>) -> Value;

pub const INTRINSIC_FUNCTIONS: &[(&str, IntrinsicFunction)] = &[
    (&"Intrinsics::U64::add", |mut arguments| {
        let b = arguments.pop().unwrap().into_u64();
        let a = arguments.pop().unwrap().into_u64();

        Value::U64(a + b)
    })
];