use std::{
    collections::{BTreeMap, HashSet},
    fmt,
    iter::{self, DoubleEndedIterator},
    ops::Deref,
    path::PathBuf,
    rc::Rc,
};

use char_list::CharList;
use nom_locate::LocatedSpan;
use rpds::{vector, Vector};
use unifier_set::{ClassifyTerm, DirectChildren, TermKind};

use crate::{
    ast::dup::{Dup, TmDuplicator},
    ast::tm_displayer::TmDisplayer,
    data_structures::{Int, Map, Sym, Var},
    interner::IStr,
    lex::{
        self,
        tok::{At, Tok},
    },
    parse::{self, Error},
};

use super::partial_char_list::PartialCharList;

pub type Rel = Map<Sym, RcTm>;

/// A term.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Int(Int),
    Txt(PartialCharList),
    Block(Tok, Vector<RcTm>),
    Rel(Rel),
    Cons(RcTm, RcTm),
    Nil,
}

impl Default for RcTm {
    fn default() -> Self {
        Tm::Nil.into()
    }
}

impl Dup for Tm {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        match self {
            Tm::Var(v) => Tm::Var(v.dup(duper)),
            Tm::Cons(h, t) => Tm::Cons(h.dup(duper), t.dup(duper)),
            Tm::Block(f, ms) => {
                let ms = ms.iter().map(|tm| tm.dup(duper)).collect();
                Tm::Block(f.clone(), ms)
            }
            Tm::Rel(rel) => {
                let rel = rel
                    .iter()
                    .map(|(name, tm)| (*name, tm.dup(duper)))
                    .collect();
                Tm::Rel(rel)
            }
            Tm::Txt(cl) => Tm::Txt(cl.dup(duper)),
            Tm::Sym(_) | Tm::Int(_) | Tm::Nil => self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RcTm(Rc<Tm>);

impl RcTm {
    pub fn try_as_list(&self) -> Option<(Vector<RcTm>, Option<Var>)> {
        let mut vec = vector![];
        let mut current = self;

        while let Tm::Cons(x, xs) = current.as_ref() {
            vec.push_back_mut(x.clone());
            current = xs;
        }

        match current.as_ref() {
            Tm::Nil => Some((vec, None)),
            Tm::Var(var) => Some((vec, Some(var.clone()))),
            _ => None,
        }
    }

    pub fn try_as_sym(&self) -> Option<Sym> {
        match self.as_ref() {
            Tm::Sym(s) => Some(*s),
            _ => None,
        }
    }

    pub fn try_as_rel(&self) -> Option<&Rel> {
        match self.as_ref() {
            Tm::Rel(r) => Some(r),
            _ => None,
        }
    }

    pub fn list_from_iter(it: impl DoubleEndedIterator<Item = RcTm>) -> Self {
        let mut list = Tm::Nil;

        for element in it.rev() {
            list = Tm::Cons(element, Self::from(list));
        }

        Self::from(list)
    }

    pub fn try_as_txt(&self) -> Option<&PartialCharList> {
        if let Tm::Txt(cl) = self.as_ref() {
            Some(cl)
        } else {
            None
        }
    }

    pub fn try_collect_txt_to_string(&self, buf: &mut String) -> Result<(), ()> {
        let mut rc_tm = self;
        loop {
            match rc_tm.as_ref() {
                Tm::Txt(cl) => {
                    buf.push_str(cl.segment_as_str());
                    rc_tm = cl.tail();
                }
                Tm::Nil => break Ok(()),
                _ => break Err(()),
            }
        }
    }

    pub fn sym(s: impl AsRef<str>) -> Self {
        Tm::Sym(IStr::from(s.as_ref())).into()
    }

    pub fn is_nil(&self) -> bool {
        matches!(self.as_ref(), Tm::Nil)
    }

    pub fn txt_from_str(s: impl Into<String>) -> Self {
        Self(Rc::new(Tm::Txt(PartialCharList(CharList::from(s.into())))))
    }
}

#[macro_export]
macro_rules! tm {
    ($ident:ident) => {
        {
            let ident = stringify!($ident);
            match ident.chars().next().unwrap() {
                ch if ch.is_lowercase() => Tm::Sym(ident.into()),
                ch if ch.is_uppercase() => Tm::Var(ident.into()),
                _ => todo!(),
            }
        }
    };

    ( $([$attr:ident])+ ) => {
        {
            let mut rel = Rel::new();

            $(
                rel.insert_mut(stringify!($attr).into(), RcTm::from(tm!($attr)));
            )+

            Tm::Rel(rel)
        }
    };

    ( $([$attr:ident $expr:expr])+ ) => {
        {
            let mut rel = Rel::new();

            $(
                rel.insert_mut(stringify!($attr).into(), $expr);
            )+

            Tm::Rel(rel)
        }
    };
}

impl Dup for RcTm {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        self.0.dup(duper).into()
    }
}

impl AsRef<Tm> for RcTm {
    fn as_ref(&self) -> &Tm {
        self.0.as_ref()
    }
}

impl Deref for RcTm {
    type Target = Tm;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl From<Tm> for RcTm {
    fn from(tm: Tm) -> Self {
        RcTm(Rc::new(tm))
    }
}

impl From<Var> for RcTm {
    fn from(var: Var) -> Self {
        Tm::Var(var).into()
    }
}

impl From<Sig> for RcTm {
    fn from(sig: Sig) -> Self {
        Self(Rc::new(Tm::Rel(
            sig.0
                .into_iter()
                .map(|sym| (*sym, Tm::Sym(*sym).into()))
                .collect(),
        )))
    }
}

impl From<Rel> for RcTm {
    fn from(rel: Rel) -> Self {
        Self(Rc::new(Tm::Rel(rel)))
    }
}

impl fmt::Display for RcTm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", TmDisplayer::default().with_tm(self.as_ref()))
    }
}

impl ClassifyTerm<Var> for RcTm {
    fn classify_term(&self) -> unifier_set::TermKind<&Var> {
        match self.as_ref() {
            Tm::Var(var) => TermKind::Var(var),
            _ => TermKind::NonVar,
        }
    }

    fn superficially_unifiable(&self, other: &Self) -> bool {
        match (self.as_ref(), other.as_ref()) {
            (Tm::Sym(s1), Tm::Sym(s2)) => s1 == s2,
            (Tm::Var(_), Tm::Var(_)) => true,
            (Tm::Int(n1), Tm::Int(n2)) => n1 == n2,
            (Tm::Txt(cl1), Tm::Txt(cl2)) => {
                // HACK: check all the text in the CharList (but not the tail if it's a non-`Nil` term)
                cl1.segment_as_str() == cl2.segment_as_str()
            }
            (Tm::Block(f1, _), Tm::Block(f2, _)) => f1 == f2,
            (Tm::Rel(r1), Tm::Rel(r2)) => r1.keys().eq(r2.keys()),
            (Tm::Cons(_, _), Tm::Cons(_, _)) => true,
            (Tm::Nil, Tm::Nil) => true,
            _ => false,
        }
    }
}

impl DirectChildren<Var> for RcTm {
    fn direct_children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a> {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Int(_) | Tm::Nil => Box::new(iter::empty()),
            Tm::Txt(cl) => {
                // HACK: return the final tail `Tm`
                Box::new(iter::empty())
            }
            Tm::Block(_, members) => Box::new(members.iter()),
            Tm::Rel(rel) => Box::new(rel.values()),
            Tm::Cons(head, tail) => Box::new(iter::once(head).chain(iter::once(tail))),
        }
    }

    fn map_direct_children<'a>(&'a self, mut f: impl FnMut(&'a Self) -> Self + 'a) -> Self {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Int(_) | Tm::Nil => self.clone(),
            Tm::Txt(cl) => {
                // HACK: actually map the tail
                self.clone()
                // Tm::Txt(char_list.clone(), f(tail)).into()
            }
            Tm::Block(functor, members) => {
                Tm::Block(functor.clone(), members.iter().map(f).collect()).into()
            }
            Tm::Rel(rel) => Tm::Rel(rel.iter().map(|(k, v)| (*k, f(v))).collect()).into(),
            Tm::Cons(head, tail) => Tm::Cons(f(head), f(tail)).into(),
        }
    }
}

/// A top-level item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    /// A command to the compiler/runtime system.
    Directive(Rel),

    /// The definition of a relation.
    RelDef(Rel, Option<RcTm>),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Directive(rel) => {
                write!(f, "[")?;
                TmDisplayer::default().fmt_rel(rel, f)?;
                write!(f, "]")?;
                Ok(())
            }
            Item::RelDef(rel, body) => {
                TmDisplayer::default().fmt_rel(rel, f)?;
                if let Some(body) = body {
                    let td = TmDisplayer::default().indented(body.as_ref());
                    write!(f, " {td}",)?;
                }
                Ok(())
            }
        }
    }
}

/// Contains a *sorted* `Vector` of keys.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sig(Vector<Sym>);

impl Sig {
    pub fn arity(&self) -> usize {
        self.0.len()
    }
}

impl From<Vector<Sym>> for Sig {
    fn from(v: Vector<Sym>) -> Self {
        let mut v: Vec<_> = v.into_iter().cloned().collect();
        v.sort();
        Self(v.into_iter().collect())
    }
}

impl From<Rel> for Sig {
    fn from(rel: Rel) -> Self {
        let mut v: Vec<_> = rel.keys().cloned().collect();
        v.sort();
        Self(v.into_iter().collect())
    }
}

impl From<&Sig> for Tm {
    fn from(sig: &Sig) -> Self {
        let rel = sig
            .0
            .iter()
            .copied()
            .map(|sym| (sym, RcTm::from(Tm::Sym(sym))))
            .collect();
        Tm::Rel(rel)
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for key in &self.0 {
            write!(f, "[{key}]")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Clause {
    pub head: Rel,
    pub body: Option<RcTm>,
}

impl Dup for Clause {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        let head = self
            .head
            .iter()
            .map(|(name, tm)| (*name, tm.dup(duper)))
            .collect();

        let body = self.body.as_ref().map(|body| body.dup(duper));

        Self { head, body }
    }
}

/// A single-file program (compilation unit).
#[derive(Debug, Default, Clone)]
pub struct Module {
    pub directives: Vec<Rel>,
    pub relations: BTreeMap<Sig, Vec<Clause>>,
    pub dependencies: HashSet<IStr>,
}

impl Module {
    pub fn parse(
        src: impl AsRef<str>,
        filename: PathBuf,
        token_buf: &mut Vec<At<Tok>>,
    ) -> Result<Module, Error<'_>> {
        let tokens = lex::tokenize_into(token_buf, LocatedSpan::new(src.as_ref()), filename)?;
        parse::entire_module(tokens)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for dir in self.directives.iter() {
            writeln!(f, "{dir}")?;
        }

        writeln!(f)?;

        for (_sig, rel_defs) in self.relations.iter() {
            for Clause { head, body } in rel_defs {
                if let Some(body) = body {
                    writeln!(f, "{head}{body}")?;
                } else {
                    writeln!(f, "{head}")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
