use crate::data_structures::{Map, Sym, Var};

pub enum Tm {
    Sym(Sym),
    Var(Var),
    Rel(Map<Sym, Tm>),
}
