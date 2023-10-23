use std::{fmt, num::NonZeroUsize, ops::Deref};

use num::BigInt;
use rpds::RedBlackTreeMap;

use crate::{
    ast::dup::{Dup, TmDuplicator},
    interner::IStr,
    rt::UnifierSet,
};

pub type Sym = IStr;
pub type Int = BigInt;
pub type Map<K, V> = RedBlackTreeMap<K, V>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    istr: IStr,
    gen: Option<NonZeroUsize>,
}

impl Var {
    pub(crate) fn with_gen(&self, gen: NonZeroUsize) -> Self {
        Self {
            istr: self.istr,
            gen: Some(gen),
        }
    }

    pub(crate) fn is_original(&self) -> bool {
        self.gen.is_none()
    }
}

impl Dup for Var {
    fn dup(&self, duper: &mut TmDuplicator, _u: &mut UnifierSet) -> Self {
        duper.dup_var(self)
    }
}

impl From<&str> for Var {
    fn from(s: &str) -> Self {
        IStr::from(s).into()
    }
}

impl From<IStr> for Var {
    fn from(istr: IStr) -> Self {
        Self { istr, gen: None }
    }
}

impl Deref for Var {
    type Target = IStr;

    fn deref(&self) -> &Self::Target {
        &self.istr
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(gen) = self.gen {
            write!(f, "{}_{}", self.istr, gen)
        } else {
            write!(f, "{}", self.istr)
        }
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Var({})", self)
    }
}
