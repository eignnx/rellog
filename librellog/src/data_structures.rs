use std::{fmt, num::NonZeroUsize, ops::Deref};

use rpds::RedBlackTreeMap;

use crate::{dup::Dup, incr::Incr, interner::IStr};

pub type Sym = IStr;
pub type Num = i64;
pub type Map<K, V> = RedBlackTreeMap<K, V>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    istr: IStr,
    gen: Option<NonZeroUsize>,
}

impl Dup for Var {
    fn dup(&self, incr: &mut Incr) -> Self {
        Self {
            istr: self.istr,
            gen: Some(incr.next()),
        }
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
