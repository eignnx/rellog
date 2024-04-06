use std::{cmp::Ordering, fmt, hash::Hash, ops::Deref};

use char_list::{seg_walker::SegmentWalker, CharList, CharListTail};

use crate::{data_structures::Var, rt};

use super::{
    dup::{Dup, TmDuplicator},
    RcTm, Tm,
};

/// Newtype wrapper for implementing PartialOrd, Ord, PartialEq, Eq, and Hash.
#[derive(Clone)]
pub struct Segment(CharList<RcTm>);

impl Deref for Segment {
    type Target = CharList<RcTm>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: Into<CharList<RcTm>>> From<T> for Segment {
    fn from(value: T) -> Self {
        Segment(value.into())
    }
}

impl fmt::Debug for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let seg = self.segment_as_str();
        let tail = if self.segment_tail().is_nil() {
            "".to_string()
        } else {
            format!("->{:?}", self.segment_tail())
        };
        write!(f, "\"{seg}\"{tail}")
    }
}

impl fmt::Display for Segment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let seg = self.segment_as_str();
        let tail = if self.segment_tail().is_nil() {
            "".to_string()
        } else {
            format!("[{{..{}}}]", self.segment_tail())
        };
        write!(f, "\"{seg}{tail}\"")
    }
}

// impl Txt {
//     /// Prepends `prefix` onto `self`.
//     #[must_use]
//     pub fn cons_txt(&self, u: UnifierSet, prefix: &Txt) -> Result<(UnifierSet, Self), TxtErr> {
//         Ok(match (prefix, self) {
//             (Txt::Cons(ch, tail), _) => {
//                 let (u, tmp) = self.cons_txt(u, tail)?;
//                 let consed = Self::Cons(ch.clone(), Rc::new(tmp));
//                 (u, consed)
//             }
//             (Txt::Seg(b1), Txt::Seg(b2)) => {
//                 // Compress link structure.
//                 let tmp = b2.cons_str(b1.segment_as_str());
//                 Self::Seg(Segment(tmp)).cons_txt(u, b1.tail())?
//             }
//             (Txt::Seg(b), Txt::Var(_) | Txt::Cons(_, _)) => {
//                 let mut buf = String::new();
//                 let mut segs = b.partial_segments();
//                 for seg in &mut segs {
//                     buf.push_str(seg.segment_as_str());
//                 }
//                 match segs.final_err() {
//                     Some(e) => return Err(e.clone()),
//                     None => {
//                         let new_cl = CharList::from_string_and_tail(buf, self.clone());
//                         (u, Txt::Seg(Segment(new_cl)))
//                     }
//                 }
//             }
//             (Txt::Var(v), _) => {
//                 let consed = Self::Cons(Tm::Var(v.clone()).into(), Rc::new(self.clone()));
//                 (u, consed)
//             }
//             (_, Txt::Nil) => (u, prefix.clone()),
//             (Txt::Nil, _) => (u, self.clone()),
//         })
//     }

//     fn is_nil(&self) -> bool {
//         matches!(self, Self::Nil)
//     }
// }

impl Segment {
    pub fn from_string_and_tail(s: impl Into<String>, tail: RcTm) -> Self {
        Segment(CharList::from_string_and_tail(s, tail))
    }

    /// Returns the tail of the frontmost segment of the underlying `CharList`.
    pub fn segment_tail(&self) -> &RcTm {
        self.0.tail()
    }

    /// Returns the last segment's tail (an `RcTm`).
    pub fn final_tail(&self) -> &RcTm {
        let mut cl = self;
        while let Tm::TxtSeg(next_cl) = cl.segment_tail().as_ref() {
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

    pub fn partial_segments(&self) -> SegmentWalker<'_, RcTm> {
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

    pub fn car_cdr(&self) -> Result<Option<(char, Self)>, TxtErr> {
        let opt = self.0.car_cdr()?;
        Ok(opt.map(|(ch, cl)| (ch, Self(cl))))
    }

    pub fn cons(&self, ch: char) -> Self {
        Self(self.0.cons(ch))
    }
}

#[derive(Debug, Clone)]
pub enum TxtErr {
    NonTxtTail(RcTm),
    UninstantiatedTail(Var),
    NonCharInTxt(RcTm),
}

impl fmt::Display for TxtErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NonTxtTail(rctm) => {
                write!(f, "Encountered text object with non-text tail: `{rctm}`")
            }
            Self::UninstantiatedTail(var) => write!(
                f,
                "Encountered text object with uninstantiated variable `{var}` as tail."
            ),
            Self::NonCharInTxt(tm) => write!(f, "Encountered non-character term in text: `{tm}`"),
        }
    }
}

impl From<TxtErr> for rt::Err {
    fn from(value: TxtErr) -> Self {
        match value {
            TxtErr::UninstantiatedTail(var) => Self::UnexpectedPartialList {
                rel: "<unknown>".to_string(),
                key: "<unknown>".to_string(),
                partial: Tm::Var(var).into(),
            },
            TxtErr::NonTxtTail(tm) | TxtErr::NonCharInTxt(tm) => Self::MalformedTxtTail(tm),
        }
    }
}

impl CharListTail for RcTm {
    type Err = TxtErr;

    fn next_char_list(&self) -> Result<Option<&CharList<Self>>, Self::Err> {
        // Ideally we'd reify the term before matching on it...
        // That will have to be the responsibility of the caller 😬
        match self.as_ref() {
            Tm::Nil => Ok(None),
            Tm::TxtCons(_car, cdr) => cdr.next_char_list(),
            Tm::TxtSeg(seg) => seg.tail().next_char_list(),
            Tm::Var(v) => Err(TxtErr::UninstantiatedTail(v.clone())),
            Tm::Sym(_)
            | Tm::Int(_)
            | Tm::Block(_, _)
            | Tm::Rel(_)
            | Tm::BinOp(_, _, _)
            | Tm::Cons(_, _) => Err(TxtErr::NonTxtTail(self.clone())),
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

// impl ClassifyTerm<Var> for Txt {
//     fn classify_term(&self) -> unifier_set::TermKind<&Var> {
//         match self {
//             Txt::Cons(_, _) | Txt::Seg(_) | Txt::Nil => unifier_set::TermKind::NonVar,
//             Txt::Var(v) => unifier_set::TermKind::Var(v),
//         }
//     }

//     fn superficially_unifiable(&self, other: &Self) -> bool {
//         // We need to check if the text content (without reifying the tail) is
//         // the same.
//         match (self, other) {
//             (Txt::Cons(_, _), Txt::Nil) | (Txt::Nil, Txt::Cons(_, _)) => false,

//             (Txt::Seg(b1), Txt::Seg(b2)) => {
//                 match cmp_text_content(b1, b2) {
//                     Ok(ordering) => ordering.is_eq(),
//                     // If the text content is equal but the tails are non-`Nil`, then
//                     // ***superficially***, these two are unifiable.
//                     Err((_tail_a, _tail_b)) => true,
//                 }
//             }
//             (Txt::Nil, Txt::Nil) => true,
//             (Txt::Var(_), _) | (_, Txt::Var(_)) => true,
//             (Txt::Cons(_, _), Txt::Cons(_, _)) => true,

//             // TODO:
//             (Txt::Cons(ch, _), Txt::Seg(b)) | (Txt::Seg(b), Txt::Cons(ch, _)) => {
//                 match ch.as_ref() {
//                     Tm::Sym(ch1) if ch1.to_str().len() == 1 => {
//                         match b.segment_as_str().chars().next() {
//                             Some(ch2) => ch1.to_str().chars().next().unwrap() == ch2,
//                             None => true, // Can't make a determination, assume it could match.
//                         }
//                     }
//                     Tm::Var(_) => true,
//                     _ => false,
//                 }
//             }
//             (Txt::Seg(b), Txt::Nil) | (Txt::Nil, Txt::Seg(b)) => b.segment_len() == 0,
//         }
//     }
// }

impl Dup for Segment {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        Self::from_string_and_tail(self.segment_as_str(), self.segment_tail().dup(duper))
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
    mut cl_a: &CharList<RcTm>,
    mut cl_b: &CharList<RcTm>,
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
            return Ok(seg_a.cmp(seg_b).then(tail_a.cmp(tail_b)));
        }

        if seg_a.is_empty() {
            cl_a = match tail_a.next_char_list() {
                Ok(Some(cl)) => cl,
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Less),
            };
            seg_a = cl_a.segment_as_str();
        }

        if seg_b.is_empty() {
            cl_b = match tail_b.next_char_list() {
                Ok(Some(cl)) => cl,
                // We're doing SYNTACTIC equality, so instantiation of tail
                // doesn't matter.
                _ => return Ok(Ordering::Greater),
            };
            seg_b = cl_b.segment_as_str();
        }
    }
}

impl PartialOrd for Segment {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Segment {
    fn cmp(&self, other: &Self) -> Ordering {
        match cmp_text_content(&self.0, &other.0) {
            Ok(ord) => ord,
            Err((t1, t2)) => t1.cmp(&t2),
        }
    }
}

impl PartialEq for Segment {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other).is_eq()
    }
}

/// This is only for SYNTACTIC equality, not string content equality. If both
/// char lists have tails instantiated to `Tm::Nil`, this is the same as string
/// equality. If the text in both strings matches up until they both end in a
/// non-`Nil` tail, the two `RcTm` tails will be compared.
impl Eq for Segment {}

// impl Ord for Txt {
//     fn cmp(&self, other: &Self) -> Ordering {
//         match (self, other) {
//             (Txt::Nil, Txt::Nil) => Ordering::Equal,
//             (Txt::Var(v1), Txt::Var(v2)) => v1.cmp(v2), // Remember: *syntactic* comparison
//             (Txt::Block(b1), Txt::Block(b2)) => match cmp_text_content(b1, b2) {
//                 Ok(ordering) => ordering,
//                 Err((tail_a, tail_b)) => tail_a.cmp(&tail_b),
//             },
//             (Txt::Cons(c1, t1), Txt::Cons(c2, t2)) => match c1.cmp(c2) {
//                 Ordering::Equal => t1.cmp(t2),
//                 ord => ord,
//             },

//             // Nil-Cons or Cons-Nil
//             (Txt::Nil, Txt::Cons(..)) => Ordering::Less,
//             (Txt::Cons(..), Txt::Nil) => Ordering::Greater,

//             // Nil-Block or Block-Nil
//             (Txt::Nil, Txt::Block(b)) => {
//                 if !b.segment_as_str().is_empty() {
//                     Ordering::Less
//                 } else {
//                     self.cmp(b.tail())
//                 }
//             }
//             (Txt::Block(b), Txt::Nil) => {
//                 if !b.segment_as_str().is_empty() {
//                     Ordering::Greater
//                 } else {
//                     b.tail().cmp(other)
//                 }
//             }

//             (Txt::Cons(_, _), Txt::Block(_)) => todo!(),
//             (Txt::Cons(_, _), Txt::Var(_)) => todo!(),
//             (Txt::Block(_), Txt::Cons(_, _)) => todo!(),
//             (Txt::Block(_), Txt::Var(_)) => todo!(),
//             (Txt::Var(_), Txt::Cons(_, _)) => todo!(),
//             (Txt::Var(_), Txt::Block(_)) => todo!(),
//             (Txt::Var(_), Txt::Nil) => todo!(),
//             (Txt::Nil, Txt::Var(_)) => todo!(),
//         }
//     }
// }

// impl Eq for Txt {}

impl Hash for Segment {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.segment_as_bytes());
        self.segment_tail().hash(state);
    }
}
