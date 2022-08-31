use rpds::RedBlackTreeMap;

use crate::interner::IStr;

pub type Sym = IStr;
pub type Var = IStr;
pub type Num = i64;
pub type Map<K, V> = RedBlackTreeMap<K, V>;
