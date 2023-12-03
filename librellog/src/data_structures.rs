use std::{fmt, ops::Deref};

use num::BigInt;
use rpds::RedBlackTreeMap;

use crate::{
    ast::{
        dup::{Dup, TmDuplicator},
        Tm,
    },
    interner::IStr,
    tm,
};

pub type Sym = IStr;
pub type Int = BigInt;
pub type Map<K, V> = RedBlackTreeMap<K, V>;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Var {
    pub name: IStr,
    pub suffix: Option<IStr>,
    pub gen: Generation,
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

impl From<Generation> for Tm {
    fn from(value: Generation) -> Self {
        match value {
            Generation::Repl => Tm::Sym("repl".into()),
            Generation::Source => Tm::Sym("source".into()),
            Generation::Internal(g) => tm!([internal Tm::Int(g.into()).into()]),
        }
    }
}

impl Var {
    pub const SUFFIX_SEPARATOR: &'static str = ".";

    pub fn from_repl(name: impl Into<IStr>, suffix: Option<IStr>) -> Self {
        Self {
            name: name.into(),
            suffix: suffix.map(Into::into),
            gen: Generation::Repl,
        }
    }

    pub fn from_source(name: impl Into<IStr>, suffix: Option<IStr>) -> Self {
        Self {
            name: name.into(),
            suffix: suffix.map(Into::into),
            gen: Generation::Source,
        }
    }

    pub fn with_gen(&self, gen: u16) -> Self {
        Self {
            gen: Generation::Internal(gen),
            ..self.clone()
        }
    }

    pub fn is_original(&self) -> bool {
        matches!(self.gen, Generation::Repl) // Either rename the method or think about Generation::Source.
    }

    pub fn should_display_at_top_level(&self) -> bool {
        self.is_original() && !self.name.to_str().starts_with('_')
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
        &self.name
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)?;
        if let Some(suffix) = self.suffix {
            write!(f, ".{suffix}")?;
        }
        match self.gen {
            Generation::Repl => Ok(()),
            Generation::Source => write!(f, "$"),
            Generation::Internal(gen) => write!(f, "${gen}"),
        }
    }
}

impl fmt::Debug for Var {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Var({})", self)
    }
}
