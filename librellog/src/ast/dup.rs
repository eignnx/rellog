use std::{collections::HashMap, fmt};

use crate::data_structures::Var;

/// Represents the action of duplicating a term. The same action that Prolog's `copy_term`
/// predicate does. This means it ought to make *different* copies of variables.
pub trait Dup {
    fn dup(&self, duper: &mut TmDuplicator) -> Self;
}

#[derive(Default)]
pub struct TmDuplicator {
    substs: HashMap<Var, Var>,
    gen: u16,
    should_rename_var: Option<Box<dyn Fn(Var) -> bool>>,
}

impl TmDuplicator {
    /// If you want to duplicate a term, use this function. It ensures that the
    /// substitution map is reset and the generation is incremented BEFORE the
    /// duplication starts.
    pub fn duplicate<D: Dup>(&mut self, x: &D) -> D {
        self.reset();
        self.incr_gen();
        x.dup(self)
    }

    fn incr_gen(&mut self) {
        self.gen += 1
    }

    pub fn do_in_new_gen<'a>(&'a mut self, action: impl FnOnce(&'a mut Self)) {
        self.incr_gen();
        action(self)
    }

    fn reset(&mut self) {
        self.substs.clear();
    }

    pub fn dup_var(&mut self, old: &Var) -> Var {
        match self.substs.get(old) {
            Some(new) => new.clone(),
            None if self.should_rename_var(old) => {
                let new = old.with_gen(self.gen);
                self.substs.insert(old.clone(), new.clone());
                new
            }
            None => old.clone(),
        }
    }

    fn should_rename_var(&self, var: &Var) -> bool {
        self.should_rename_var
            .as_ref()
            .map(|predicate| predicate(var.clone()))
            .unwrap_or(true)
    }

    pub fn duplicate_conditionally<D: Dup>(
        &mut self,
        x: &D,
        cond: Box<impl Fn(Var) -> bool + 'static>,
    ) -> D {
        let old_cond = self.should_rename_var.replace(cond);
        let new = self.duplicate(x);
        self.should_rename_var = old_cond;
        new
    }

    pub fn substs(&self) -> &HashMap<Var, Var> {
        &self.substs
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
