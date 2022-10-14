use crate::{ast::ast::RcTm, data_structures::Var};
use std::fmt;
use unifier_set::{ClassifyTerm, TermKind, UnifierSet};

pub struct DisplayUnifierSet(pub UnifierSet<Var, RcTm>);

impl fmt::Display for DisplayUnifierSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.0;
        let mut nothing_written = true;
        for (root_term, vars) in u.reified_forest().into_iter() {
            // Skip this row if none of the variables are original (from the top-level).
            if !vars.is_empty() && vars.iter().any(Var::is_original) {
                nothing_written = false;
                write!(f, "    - ")?;
                for (i, var) in vars.into_iter().filter(Var::is_original).enumerate() {
                    if var.is_original() {
                        if i == 0 {
                            write!(f, "{var}")?;
                        } else {
                            write!(f, " = {var}")?;
                        }
                    }
                }
                match root_term.classify_term() {
                    TermKind::NonVar => writeln!(f, " = {root_term}")?,
                    TermKind::Var(v) if v.is_original() => writeln!(f, " = {v}")?,
                    _ => {}
                }
            }
        }

        if nothing_written {
            write!(f, "    - [true]")?;
        }

        Ok(())
    }
}
