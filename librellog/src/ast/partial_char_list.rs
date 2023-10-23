use std::{cmp::Ordering, fmt::Display, hash::Hash, ops::Deref};

use char_list::{CharList, CharListTail};
use unifier_set::ClassifyTerm;

use crate::{data_structures::Var, rt::UnifierSet};

use super::{
    dup::{Dup, TmDuplicator},
    RcTm, Tm,
};

#[derive(Debug, Clone)]
pub struct PartialCharList(pub CharList<RcTm>);

impl PartialCharList {
    pub fn cons_partial_char_list(&self, prefix: &PartialCharList) -> Self {
        PartialCharList(self.0.cons_char_list(&prefix.0))
    }

    pub fn from_string_and_tail(s: impl Into<String>, tail: RcTm) -> Self {
        PartialCharList(CharList::from_string_and_tail(s, tail))
    }

    /// Returns the tail of the frontmost segment of the underlying `CharList`.
    pub fn segment_tail(&self) -> &RcTm {
        self.0.tail()
    }

    /// Returns the last segment's tail (an `RcTm`).
    pub fn tail(&self) -> &RcTm {
        let mut cl = self;
        while let Tm::Txt(next_cl) = cl.segment_tail().as_ref() {
            cl = next_cl;
        }
        cl.segment_tail()
    }

    /// Consolidates all the text in this set of linked `Txt` segments. Note:
    /// Stops when it hits a `Nil` or a `Var` (or any other non-`Txt` term).
    pub fn clone_with_new_tail<'a>(
        &'a self,
        mut tail_mapper: impl FnMut(&'a RcTm) -> RcTm,
    ) -> Self {
        let mut content = String::new();

        for seg in self.partial_segments() {
            content.push_str(seg.segment_as_str());
        }

        Self::from_string_and_tail(content, tail_mapper(self.tail()))
    }
}

impl From<String> for PartialCharList {
    fn from(value: String) -> Self {
        PartialCharList(CharList::from(value))
    }
}

impl From<&str> for PartialCharList {
    fn from(value: &str) -> Self {
        PartialCharList(CharList::from(value))
    }
}

impl ClassifyTerm<Var> for PartialCharList {
    fn classify_term(&self) -> unifier_set::TermKind<&Var> {
        unifier_set::TermKind::NonVar
    }

    fn superficially_unifiable(&self, other: &Self) -> bool {
        // We need to check if the text content (without reifying the tail) is
        // the same.
        match cmp_text_content(self.clone(), other.clone()) {
            Ok(ordering) => ordering.is_eq(),
            // If the text content is equal but the tails are non-`Nil`, then
            // ***superficially***, these two are unifiable.
            Err((_tail_a, _tail_b)) => true,
        }
    }
}

impl Dup for PartialCharList {
    fn dup(&self, duper: &mut TmDuplicator, u: &mut UnifierSet) -> Self {
        // We'd PREFER NOT TO clone any of the backing `FrontString`s!
        // Alas...
        self.clone_with_new_tail(|tail| u.reify_term(tail).dup(duper, u))
    }
}

impl CharListTail for RcTm {
    type Err = TailError;

    fn next_char_list(&self) -> Result<Option<&char_list::CharList<Self>>, Self::Err> {
        match self.as_ref() {
            Tm::Nil => Ok(None),
            Tm::Txt(PartialCharList(cl)) => Ok(Some(cl)),
            Tm::Var(v) => Err(TailError::UninstantiatedTail(v.clone())),
            Tm::Sym(..) | Tm::Int(..) | Tm::Block(..) | Tm::Rel(..) | Tm::Cons(..) => {
                Err(TailError::NonListTail(self.clone()))
            }
        }
    }

    fn len(&self) -> Result<usize, Self::Err> {
        match self.as_ref() {
            Tm::Nil => Ok(0),
            Tm::Txt(PartialCharList(cl)) => cl.len(),
            Tm::Var(v) => Err(TailError::UninstantiatedTail(v.clone())),
            Tm::Sym(..) | Tm::Int(..) | Tm::Block(..) | Tm::Rel(..) | Tm::Cons(..) => {
                Err(TailError::NonListTail(self.clone()))
            }
        }
    }
}

impl Display for PartialCharList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut cl = self;
        write!(f, "{}", cl.segment_as_str())?;

        while let Tm::Txt(next_cl) = cl.segment_tail().as_ref() {
            write!(f, "{}", cl.segment_as_str())?;
            cl = next_cl;
        }

        if !cl.segment_tail().is_nil() {
            write!(f, "[..{}]", cl.segment_tail())?;
        }

        Ok(())
    }
}

impl Deref for PartialCharList {
    type Target = CharList<RcTm>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl PartialEq for PartialCharList {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other).unwrap().is_eq()
    }
}

impl PartialOrd for PartialCharList {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

fn remove_common_prefix(a: &mut &str, b: &mut &str) {
    let mut i = 0;
    for (ch_a, ch_b) in a.chars().zip(b.chars()) {
        if ch_a != ch_b {
            break;
        }
        i += 1;
    }
    *a = &a[..i];
    *b = &b[..i];
}

/// If a determination can be made based on the text content alone, returns
/// `Ok(Ordering)`, otherwise returns the two tails and an ordering can be
/// made by the caller.
fn cmp_text_content(
    mut cl_a: PartialCharList,
    mut cl_b: PartialCharList,
) -> Result<Ordering, (RcTm, RcTm)> {
    let mut seg_a = cl_a.segment_as_str();
    let mut seg_b = cl_b.segment_as_str();

    loop {
        remove_common_prefix(&mut seg_a, &mut seg_b);

        let tail_a = cl_a.tail();
        let tail_b = cl_b.tail();

        if (seg_a.is_empty() && seg_b.is_empty()) && (!tail_a.is_nil() || !tail_b.is_nil()) {
            return Err((tail_a.clone(), tail_b.clone()));
        }

        if !seg_a.is_empty() && !seg_b.is_empty() {
            return Ok(seg_a.cmp(seg_b));
        }

        if seg_a.is_empty() {
            cl_a = match tail_a.next_char_list() {
                Ok(Some(cl)) => PartialCharList(cl.clone()),
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Less),
            };
            seg_a = cl_a.segment_as_str();
        }

        if seg_b.is_empty() {
            cl_b = match tail_b.next_char_list() {
                Ok(Some(cl)) => PartialCharList(cl.clone()),
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Greater),
            };
            seg_b = cl_b.segment_as_str();
        }
    }
}

impl Ord for PartialCharList {
    fn cmp(&self, other: &Self) -> Ordering {
        match cmp_text_content(self.clone(), other.clone()) {
            Ok(ordering) => ordering,
            Err((tail_a, tail_b)) => tail_a.cmp(&tail_b),
        }
    }
}

/// This is only for SYNTACTIC equality, not string content equality. If both
/// char lists have tails instantiated to `Tm::Nil`, this is the same as string
/// equality. If the text in both strings matches up until they both end in a
/// non-`Nil` tail, the two `RcTm` tails will be compared.
impl Eq for PartialCharList {}

impl Hash for PartialCharList {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.segment_as_bytes());
        self.tail().hash(state);
    }
}

#[derive(Debug)]
pub enum TailError {
    UninstantiatedTail(Var),
    NonListTail(RcTm),
}

impl Display for TailError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[test]
fn comparison() {
    use assert2::check;

    let a = PartialCharList::from("abc");
    let b = PartialCharList::from("abc");
    check!(a == b);

    let a = PartialCharList::from("a");
    let b = PartialCharList::from("abc");
    check!(a < b);

    let a = PartialCharList::from("abc");
    let b = PartialCharList::from("a");
    check!(a > b);

    let a = PartialCharList::from("");
    let b = PartialCharList::from("");
    check!(a == b);

    let a = PartialCharList::from("");
    let b = PartialCharList::from("a");
    check!(a < b);

    let a = PartialCharList::from("a");
    let b = PartialCharList::from("");
    check!(a > b);
}
