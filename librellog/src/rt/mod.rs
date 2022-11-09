mod rt;
pub mod soln_stream;

pub use rt::*;

use crate::{ast::RcTm, data_structures::Var};

pub type Res<T> = Result<T, Err>;

pub type UnifierSet = unifier_set::UnifierSet<Var, RcTm>;
