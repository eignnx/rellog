use std::{fmt, iter, ops::Deref, rc::Rc};

use rpds::Vector;
use unifier_set::{ClassifyTerm, DirectChildren, TermKind};

use crate::{
    data_structures::{Map, Num, Sym, Var},
    dup::Dup,
    incr::Incr,
    tm_displayer::TmDisplayer,
    tok::Tok,
};

pub type Rel = Map<Sym, RcTm>;

/// A term.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Num(Num),
    Txt(String),
    Block(Tok, Vector<RcTm>),
    Rel(Rel),
    Cons(RcTm, RcTm),
    Nil,
}

impl Dup for Tm {
    fn dup(&self, incr: &mut Incr) -> Self {
        match self {
            Tm::Var(v) => Tm::Var(v.dup(incr)),
            Tm::Cons(h, t) => Tm::Cons(h.dup(incr), t.dup(incr)),
            Tm::Block(f, ms) => {
                let ms = ms.iter().map(|tm| tm.dup(incr)).collect();
                Tm::Block(f.clone(), ms)
            }
            Tm::Rel(rel) => {
                let rel = rel
                    .iter()
                    .map(|(name, tm)| (name.clone(), tm.dup(incr)))
                    .collect();
                Tm::Rel(rel)
            }
            Tm::Sym(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Nil => self.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RcTm(Rc<Tm>);

impl Dup for RcTm {
    fn dup(&self, incr: &mut Incr) -> Self {
        self.0.dup(incr).into()
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
            (Tm::Num(_), Tm::Num(_)) => true,
            (Tm::Txt(_), Tm::Txt(_)) => true,
            (Tm::Block(f1, _), Tm::Block(f2, _)) => f1 == f2,
            (Tm::Rel(r1), Tm::Rel(r2)) => r1.keys().eq(r2.keys()),
            (Tm::Cons(_, _), Tm::Cons(_, _)) => true,
            (Tm::Nil, Tm::Nil) => true,
            _ => false,
        }
    }
}

impl DirectChildren for RcTm {
    fn direct_children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a> {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Nil => Box::new(iter::empty()),
            Tm::Block(_, members) => Box::new(members.iter()),
            Tm::Rel(rel) => Box::new(rel.values()),
            Tm::Cons(head, tail) => Box::new(iter::once(head).chain(iter::once(tail))),
        }
    }

    fn map_direct_children<'a>(&'a self, mut f: impl FnMut(&'a Self) -> Self + 'a) -> Self {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Nil => self.clone(),
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

/// A single-file program (compilation unit).
#[derive(Debug, Default)]
pub struct Module(pub Vec<Item>);

impl Module {
    pub fn rel_defs<'m>(&'m self) -> impl Iterator<Item = (&'m Rel, &'m Option<RcTm>)> + 'm {
        self.0.iter().filter_map(|item| match item {
            Item::Directive(_) => None,
            Item::RelDef(head, body) => Some((head, body)),
        })
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Module(items) = self;
        for (idx, item) in items.iter().enumerate() {
            if idx > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{item}")?;
        }
        Ok(())
    }
}
