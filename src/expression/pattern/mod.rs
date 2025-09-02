mod array;
mod variant_case;

use crate::interner::InternIdx;

pub type Array       = array::T;
pub type VariantCase = variant_case::T;

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