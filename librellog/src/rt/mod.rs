//! Defines the rellog runtime.

pub mod breakpoint;
pub mod builtins;
mod err;
pub mod kb;
#[allow(clippy::module_inception)]
mod rt;
pub mod soln_stream;

pub use err::*;
pub use rt::*;

use crate::{ast::RcTm, data_structures::Var};

pub type Res<T> = Result<T, err::Err>;

pub type UnifierSet = unifier_set::UnifierSet<Var, RcTm>;
