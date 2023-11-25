use std::{fmt, ops::Deref};

use num::BigInt;
use rpds::RedBlackTreeMap;

use crate::{
    ast::dup::{Dup, TmDuplicator},
    interner::IStr,
};

pub type Sym = IStr;
pub type Int = BigInt;
pub type Map<K, V> = RedBlackTreeMap<K, V>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    istr: IStr,
    gen: Generation,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Generation {
    /// Represents a var parsed from user input at the repl.
    Repl,
    /// Represents a var parsed from a source file.
    Source,
    /// Represents a var that has been duplicated already. An internal variable.
    Internal(u16),
}

impl Generation {
    pub fn bump(&self, new: u16) -> Self {
        match self {
            Self::Repl => Self::Internal(new),
            Self::Source => Self::Internal(new),
            Self::Internal(old) if new > *old => Self::Internal(new),
            Self::Internal(old) => {
                panic!("Attempting to set a generation to an older one! (old={old}, new={new})")
            }
        }
    }
}

impl Var {
    pub fn from_repl(s: impl Into<IStr>) -> Self {
        Self {
            istr: s.into(),
            gen: Generation::Repl,
        }
    }

    pub fn from_source(s: impl Into<IStr>) -> Self {
        Self {
            istr: s.into(),
            gen: Generation::Source,
        }
    }

    pub fn with_gen(&self, gen: u16) -> Self {
        Self {
            istr: self.istr,
            gen: Generation::Internal(gen),
        }
    }

    pub fn gen(&self) -> Generation {
        self.gen
    }

    pub fn gen_mut(&mut self) -> &mut Generation {
        &mut self.gen
    }

    pub fn istr(&self) -> IStr {
        self.istr
    }

    pub fn is_original(&self) -> bool {
        matches!(self.gen, Generation::Repl) // Either rename the method or think about Generation::Source.
    }
}

impl Dup for Var {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        duper.dup_var(self)
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
        match self.gen {
            Generation::Repl => write!(f, "{}", self.istr),
            Generation::Source => write!(f, "{}$", self.istr),
            Generation::Internal(gen) => write!(f, "{}${}", self.istr, gen),
        }
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Var({})", self)
    }
}
