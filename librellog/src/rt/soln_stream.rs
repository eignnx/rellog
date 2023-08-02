use std::iter;

use crate::{ast::RcTm, rt::UnifierSet};

use super::{err::Err, Res};

/// Essentially a trait alias.
pub trait SolnStream: Iterator<Item = Res<UnifierSet>> {}

// Impl it for all T that apply.
impl<T> SolnStream for T where T: Iterator<Item = Res<UnifierSet>> {}

pub fn success(u: UnifierSet) -> Box<dyn SolnStream> {
    Box::new(iter::once(Ok(u)))
}
pub fn failure() -> Box<dyn SolnStream> {
    Box::new(iter::empty())
}

pub fn unifying(u: UnifierSet, x: &RcTm, y: &RcTm) -> Box<dyn SolnStream> {
    Box::new(u.unify(x, y).into_iter().map(Ok))
}

pub fn once(res: Res<UnifierSet>) -> Box<dyn SolnStream> {
    Box::new(iter::once(res))
}

pub fn error(err: Err) -> Box<dyn SolnStream> {
    once(Err(err))
}

impl From<Err> for Box<dyn SolnStream> {
    fn from(err: Err) -> Self {
        once(Err(err))
    }
}

impl From<UnifierSet> for Box<dyn SolnStream> {
    fn from(u: UnifierSet) -> Self {
        success(u)
    }
}
