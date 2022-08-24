use crate::data_structures::{Map, Sym, Var};

#[derive(Debug, PartialEq, Eq)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Rel(Map<Sym, Tm>),
}
