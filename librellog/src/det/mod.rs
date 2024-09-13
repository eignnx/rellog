use crate::ast::{BinOpSymbol, RcTm, Tm};

#[derive(Debug)]
pub enum Error {}

pub type Result<T> = std::result::Result<T, Error>;

/// | Can fail?   |   0             | 1            |  \> 1  |
/// |-------------|-----------------|--------------|--------|
/// | no          |   erroneous     | det          |  multi |
/// | yes         |   failure       | semidet      | nondet |
///
/// ```text
///    erroneous
///     /     \
/// failure   det
///    \     /   \
///    semidet  multi
///        \     /
///         nondet
/// ```
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Determinism {
    /// # Erroneous
    /// No possible calls to a particular mode of a predicate or function can
    /// return to the caller.
    Erroneous,
    /// # Deterministic
    /// Has exactly one solution.
    Det,
    /// # Multisolution
    /// Has at least one solution but may have more.
    Multi,
    /// # Failure
    /// Always fails without producing a solution.
    Failure,
    /// # Semideterministic
    /// Either has no solutions or have one solution.
    Semidet,
    /// # Nondeterministic
    /// May have zero, one, or more solutions.
    NonDet,
}

/// Infers the determinisim of a term.
pub fn infer_det(tm: &RcTm) -> Result<Determinism> {
    match tm.as_ref() {
        // The determinism of a unification is either det, semidet, or failure,
        // depending on its mode.
        //
        // A unification that assigns the value of one variable to another is
        // deterministic. A unification that constructs a structure and assigns
        // it to a variable is also deterministic. A unification that tests
        // whether a variable has a given top function symbol is
        // semideterministic, unless the compiler knows the top function symbol
        // of that variable, in which case its determinism is either det or
        // failure depending on whether the two function symbols are the same
        // or not. A unification that tests two variables for equality is
        // semideterministic, unless the compiler knows that the two variables
        // are aliases for one another, in which case the unification is
        // deterministic, or unless the compiler knows that the two variables
        // have different function symbols in the same position, in which case
        // the unification has a determinism of failure.
        //
        // The compiler knows the top function symbol of a variable if the
        // previous part of the procedure definition contains a unification of
        // the variable with a function symbol, or if the variable’s type has
        // only one function symbol.
        Tm::BinOp(BinOpSymbol::Equal, lhs, rhs) => Ok(Determinism::Semidet),
    }
}
