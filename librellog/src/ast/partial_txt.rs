use std::{cmp::Ordering, fmt, hash::Hash};

use char_list::{seg_walker::SegmentWalker, CharList, CharListTail};
use unifier_set::ClassifyTerm;

use crate::{data_structures::Var, rt};

use super::{
    dup::{Dup, TmDuplicator},
    RcTm, Tm,
};

#[derive(Clone)]
pub struct PartialTxt(CharList<RcTm>);

impl PartialTxt {
    pub fn cons_partial_char_list(&self, prefix: &PartialTxt) -> Self {
        PartialTxt(self.0.cons_char_list(&prefix.0))
    }

    pub fn from_string_and_tail(s: impl Into<String>, tail: RcTm) -> Self {
        PartialTxt(CharList::from_string_and_tail(s, tail))
    }

    /// Returns the tail of the frontmost segment of the underlying `CharList`.
    pub fn segment_tail(&self) -> &RcTm {
        self.0.tail()
    }

    /// Returns the last segment's tail (an `RcTm`).
    pub fn final_tail(&self) -> &RcTm {
        let mut cl = self;
        while let Tm::Txt(next_cl) = cl.segment_tail().as_ref() {
            cl = next_cl;
        }
        cl.segment_tail()
    }

    pub fn segment_as_str(&self) -> &str {
        self.0.segment_as_str()
    }

    pub fn segment_as_bytes(&self) -> &[u8] {
        self.0.segment_as_bytes()
    }

    pub fn partial_segments<'a>(&'a self) -> SegmentWalker<'a, RcTm> {
        self.0.partial_segments()
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

        Self::from_string_and_tail(content, tail_mapper(self.segment_tail()))
    }

    /// Returns the length of as much of the text as is known right now.
    pub fn partial_len(&self) -> usize {
        self.0.partial_len()
    }

    pub fn segment_len(&self) -> usize {
        self.0.segment_len()
    }

    pub fn car_cdr(&self) -> Result<Option<(char, PartialTxt)>, PartialTxtErr> {
        let opt = self.0.car_cdr()?;
        Ok(opt.map(|(ch, cl)| (ch, Self(cl))))
    }

    pub fn cons(&self, ch: char) -> PartialTxt {
        Self(self.0.cons(ch))
    }
}

#[derive(Debug)]
pub enum PartialTxtErr {
    NonTxtTail(RcTm),
    UninstantiatedTail(Var),
}

impl fmt::Display for PartialTxtErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonTxtTail(tail) => write!(
                f,
                "Encountered text object with non-text non-nil non-var tail: `{tail}`"
            ),
            Self::UninstantiatedTail(var) => write!(
                f,
                "Encountered text object with uninstantiated variable `{var}` as tail."
            ),
        }
    }
}

impl From<PartialTxtErr> for rt::Err {
    fn from(value: PartialTxtErr) -> Self {
        match value {
            PartialTxtErr::NonTxtTail(tail) => Self::MalformedTxtTail(tail),
            PartialTxtErr::UninstantiatedTail(var) => Self::UnexpectedPartialList {
                rel: "<unknown>".to_string(),
                key: "<unknown>".to_string(),
                partial: Tm::Var(var).into(),
            },
        }
    }
}

impl CharListTail for RcTm {
    type Err = PartialTxtErr;

    fn next_char_list(&self) -> Result<Option<&CharList<Self>>, Self::Err> {
        // Ideally we'd reify the term before matching on it...
        // That will have to be the responsibility of the caller 😬
        match self.as_ref() {
            Tm::Nil => Ok(None),
            Tm::Txt(txt) => Ok(Some(&txt.0)),
            Tm::Var(var) => Err(PartialTxtErr::UninstantiatedTail(var.clone())),
            Tm::Sym(..)
            | Tm::Int(..)
            | Tm::Block(..)
            | Tm::Rel(..)
            | Tm::BinOp(..)
            | Tm::Cons(..) => Err(PartialTxtErr::NonTxtTail(self.clone())),
        }
    }

    fn len(&self) -> Result<usize, Self::Err> {
        let mut it = self.next_char_list()?;
        let mut len = 0;
        loop {
            match it {
                None => return Ok(len),
                Some(txt) => {
                    len += txt.segment_len();
                    it = txt.tail().next_char_list()?;
                }
            }
        }
    }
}

impl From<String> for PartialTxt {
    fn from(value: String) -> Self {
        PartialTxt(CharList::from(value))
    }
}

impl From<&str> for PartialTxt {
    fn from(value: &str) -> Self {
        PartialTxt(CharList::from(value))
    }
}

impl ClassifyTerm<Var> for PartialTxt {
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

impl Dup for PartialTxt {
    fn dup(&self, duper: &mut TmDuplicator /*u: &mut UnifierSet*/) -> Self {
        // I'm afraid we need a unifier set in order to traverse this thing...
        Self::from_string_and_tail(self.segment_as_str(), self.segment_tail().dup(duper))
    }
}

impl fmt::Debug for PartialTxt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Txt(\"{}\")->{:?}",
            self.segment_as_str(),
            self.segment_tail()
        )?;

        Ok(())
    }
}

// I've decided NOT to impl Deref for this type because I was getting confused
// about method resolution 😵 Specifically `CharList::tail` vs
// `PartialTxt::segment_tail` vs `PartialTxt::final_tail`.
////////////////////////////////////////////////////////////////////////////////
// impl Deref for PartialTxt {
//     type Target = CharList<RcTm>;

//     fn deref(&self) -> &Self::Target {
//         &self.0
//     }
// }

impl PartialEq for PartialTxt {
    fn eq(&self, other: &Self) -> bool {
        self.partial_cmp(other).unwrap().is_eq()
    }
}

impl PartialOrd for PartialTxt {
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
fn cmp_text_content(mut cl_a: PartialTxt, mut cl_b: PartialTxt) -> Result<Ordering, (RcTm, RcTm)> {
    let mut seg_a = cl_a.segment_as_str();
    let mut seg_b = cl_b.segment_as_str();

    loop {
        remove_common_prefix(&mut seg_a, &mut seg_b);

        let tail_a = cl_a.segment_tail();
        let tail_b = cl_b.segment_tail();

        if (seg_a.is_empty() && seg_b.is_empty()) && (!tail_a.is_nil() || !tail_b.is_nil()) {
            return Err((tail_a.clone(), tail_b.clone()));
        }

        if !seg_a.is_empty() && !seg_b.is_empty() {
            return Ok(seg_a.cmp(seg_b));
        }

        if seg_a.is_empty() {
            cl_a = match tail_a.next_char_list() {
                Ok(Some(cl)) => PartialTxt(cl.clone()),
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Less),
            };
            seg_a = cl_a.segment_as_str();
        }

        if seg_b.is_empty() {
            cl_b = match tail_b.next_char_list() {
                Ok(Some(cl)) => PartialTxt(cl.clone()),
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Greater),
            };
            seg_b = cl_b.segment_as_str();
        }
    }
}

impl Ord for PartialTxt {
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
impl Eq for PartialTxt {}

impl Hash for PartialTxt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.segment_as_bytes());
        self.segment_tail().hash(state);
    }
}
