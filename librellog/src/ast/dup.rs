use std::{collections::HashMap, fmt, num::NonZeroUsize};

use crate::{data_structures::Var, rt::UnifierSet};

/// Represents the action of duplicating a term. The same action that Prolog's `copy_term`
/// predicate does. This means it ought to make *different* copies of variables.
pub trait Dup {
    fn dup(&self, duper: &mut TmDuplicator, u: &mut UnifierSet) -> Self;
}

#[derive(Default)]
pub struct TmDuplicator {
    substs: HashMap<Var, Var>,
    gen: Option<NonZeroUsize>,
}

impl TmDuplicator {
    /// If you want to duplicate a term, use this function. It ensures that the
    /// substitution map is reset and the generation is incremented BEFORE the
    /// duplication starts.
    pub fn duplicate<D: Dup>(&mut self, x: &D, u: &mut UnifierSet) -> D {
        self.reset();
        self.incr_gen();
        x.dup(self, u)
    }

    fn incr_gen(&mut self) {
        if let Some(gen) = self.gen {
            self.gen = Some(NonZeroUsize::new(gen.get() + 1).unwrap());
        } else {
            self.gen = Some(NonZeroUsize::new(1).unwrap());
        }
    }

    fn reset(&mut self) {
        self.substs.clear();
    }

    pub fn dup_var(&mut self, old: &Var) -> Var {
        match self.substs.get(old) {
            Some(new) => new.clone(),
            None => {
                let gen = self.gen.expect("Don't generate Vars with None generation.");
                let new = old.with_gen(gen);
                self.substs.insert(old.clone(), new.clone());
                new
            }
        }
    }
}

impl fmt::Debug for TmDuplicator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "TmDuplicator {{")?;
        for (old, new) in &self.substs {
            writeln!(f, "  {old}->{new},")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
