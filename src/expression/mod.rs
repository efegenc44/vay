mod path;
mod array;
mod application;
mod projection;
mod r#let;
mod sequence;
mod block;
mod lambda;
mod r#match;
mod r#return;
mod assignment;
mod r#while;

pub mod pattern;

use crate::{interner::InternIdx};

pub type Path        = path::T;
pub type Array       = array::T;
pub type Application = application::T;
pub type Projection  = projection::T;
pub type Let         = r#let::T;
pub type Sequence    = sequence::T;
pub type Block       = block::T;
pub type Lambda      = lambda::T;
pub type Match       = r#match::T;
pub type MatchBranch = r#match::Branch; // FIXME
pub type Return      = r#return::T;
pub type Assignment  = assignment::T;
pub type While       = r#while::T;

#[derive(Clone)]
pub enum Expression {
    U64(u64),
    F32(f32),
    String(InternIdx),
    Char(char),
    Path(Path),
    Array(Array),
    Application(Application),
    Projection(Projection),
    Let(Let),
    Sequence(Sequence),
    Block(Block),
    Lambda(Lambda),
    Match(Match),
    Return(Return),
    Assignment(Assignment),
    While(While),
    Continue,
    Break,
}
