use crate::{
    data_structures::{Map, Sym, Var},
    tok::Tok,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Block(Tok, Vec<Tm>),
    Rel(Map<Sym, Tm>),
}
