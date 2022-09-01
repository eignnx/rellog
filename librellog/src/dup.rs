use crate::incr::Incr;

/// Represents the action of duplicating a term. The same action that Prolog's `copy_term`
/// predicate does. This means it ought to make *different* copies of variables.
pub trait Dup {
    fn dup(&self, incr: &mut Incr) -> Self;
}
