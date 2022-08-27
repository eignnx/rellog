use std::{collections::BTreeMap, fmt};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Sym(pub(crate) lasso::Spur);

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

pub type Var = Sym;
pub type Num = i64;
pub type Map<K, V> = BTreeMap<K, V>;
