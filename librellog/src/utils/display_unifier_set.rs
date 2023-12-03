use crate::{
    ast::{tm_displayer::TmDisplayer, RcTm},
    data_structures::Var,
};
use std::fmt;
use unifier_set::{ClassifyTerm, TermKind, UnifierSet};

pub struct DisplayUnifierSet {
    pub u: UnifierSet<Var, RcTm>,
    pub display_or_bar: bool,
}

impl fmt::Display for DisplayUnifierSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let u = &self.u;
        let mut nothing_written = true;
        for (root_tm, vars) in u.reified_forest().into_iter() {
            // Skip this row if none of the variables are original (from the top-level).
            if !vars.is_empty() && vars.iter().any(Var::should_display_at_top_level) {
                let msg = if nothing_written && self.display_or_bar {
                    "  - "
                } else {
                    "   - "
                };
                f.write_str(msg)?;

                nothing_written = false;
                for (i, var) in vars
                    .into_iter()
                    .filter(Var::should_display_at_top_level)
                    .enumerate()
                {
                    let maybe_eq = if i == 0 { "" } else { " = " };
                    write!(f, "{maybe_eq}{var}")?;
                }
                match root_tm.classify_term() {
                    TermKind::NonVar => {
                        writeln!(f, " = {}", TmDisplayer::default().indented(&root_tm))?
                    }
                    TermKind::Var(v) if v.should_display_at_top_level() => writeln!(f, " = {v}")?,
                    _ => writeln!(f)?,
                }
            }
        }

        if nothing_written {
            if self.display_or_bar {
                write!(f, "   - [true]")?;
            } else {
                write!(f, "    - [true]")?;
            }
        }

        Ok(())
    }
}
