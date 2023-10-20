use std::{cmp::Ordering, fmt::Display, hash::Hash, ops::Deref};

use char_list::{CharList, CharListTail};

use crate::data_structures::Var;

use super::{RcTm, Tm};

#[derive(Debug, Clone)]
pub struct PartialCharList(CharList<RcTm>);

impl CharListTail for RcTm {
    type Err = TailError;

    fn next_char_list(&self) -> Result<Option<char_list::CharList<Self>>, Self::Err> {
        match self.as_ref() {
            Tm::Nil => Ok(None),
            Tm::Txt(PartialCharList(cl)) => Ok(Some(*cl)),
            Tm::Var(v) => Err(TailError::UninstantiatedTail(*v)),
            Tm::Sym(..) | Tm::Int(..) | Tm::Block(..) | Tm::Rel(..) | Tm::Cons(..) => {
                Err(TailError::NonListTail(self.clone()))
            }
        }
    }

    fn len(&self) -> Result<usize, Self::Err> {
        match self.as_ref() {
            Tm::Nil => Ok(0),
            Tm::Txt(PartialCharList(cl)) => cl.len(),
            Tm::Var(v) => Err(TailError::UninstantiatedTail(*v)),
            Tm::Sym(..) | Tm::Int(..) | Tm::Block(..) | Tm::Rel(..) | Tm::Cons(..) => {
                Err(TailError::NonListTail(self.clone()))
            }
        }
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
        let mut cl_a = self.clone();
        let mut cl_b = other.clone();

        let mut seg_a = cl_a.segment_as_str();
        let mut seg_b = cl_b.segment_as_str();

        loop {
            remove_common_prefix(&mut seg_a, &mut seg_b);

            if (seg_a.is_empty() && seg_b.is_empty())
                && (!cl_a.tail().is_nil() || !cl_b.tail().is_nil())
            {
                return cl_a.tail().partial_cmp(cl_b.tail());
            }

            if !seg_a.is_empty() && !seg_b.is_empty() {
                return seg_a.partial_cmp(seg_b);
            }

            if seg_a.is_empty() {
                cl_a = match cl_a.tail().next_char_list() {
                    Ok(Some(cl)) => PartialCharList(cl),
                    // We're doing SYNTACTIC equality, so instantiation of tail
                    // doesn't matter.
                    _ => return Some(Ordering::Less),
                };
                seg_a = cl_a.segment_as_str();
            }

            if seg_b.is_empty() {
                cl_b = match cl_b.tail().next_char_list() {
                    Ok(Some(cl)) => PartialCharList(cl),
                    // We're doing SYNTACTIC equality, so instantiation of tail
                    // doesn't matter.
                    _ => return Some(Ordering::Greater),
                };
                seg_b = cl_b.segment_as_str();
            }
        }
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
    *a = &mut a[..i];
    *b = &mut b[..i];
}

impl Ord for PartialCharList {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
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

impl From<&str> for PartialCharList {
    fn from(value: &str) -> Self {
        PartialCharList(CharList::from(value))
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
