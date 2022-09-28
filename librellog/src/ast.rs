use std::{collections::BTreeMap, fmt, iter, ops::Deref, rc::Rc};

use rpds::Vector;
use unifier_set::{ClassifyTerm, DirectChildren, TermKind};

use crate::{
    data_structures::{Map, Num, Sym, Var},
    dup::{Dup, TmDuplicator},
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
                    .map(|(name, tm)| (name.clone(), tm.dup(duper)))
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

impl DirectChildren<Var> for RcTm {
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

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sig(Vector<Sym>);

impl Sig {
    pub fn arity(&self) -> usize {
        self.0.len()
    }
}

impl From<Vector<Sym>> for Sig {
    fn from(v: Vector<Sym>) -> Self {
        Self(v)
    }
}

impl From<Rel> for Sig {
    fn from(rel: Rel) -> Self {
        Self(rel.keys().cloned().collect())
    }
}

impl From<&Rel> for Sig {
    fn from(rel: &Rel) -> Self {
        Self(rel.keys().cloned().collect())
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

#[derive(Debug, Clone)]
pub struct Clause {
    pub head: Rel,
    pub body: Option<RcTm>,
}

impl Dup for Clause {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        let head = self
            .head
            .iter()
            .map(|(name, tm)| (name.clone(), tm.dup(duper)))
            .collect();

        let body = match &self.body {
            Some(body) => Some(body.dup(duper)),
            None => None,
        };

        Self { head, body }
    }
}

/// A single-file program (compilation unit).
#[derive(Debug, Default)]
pub struct Module {
    pub directives: Vec<Rel>,
    pub relations: BTreeMap<Sig, Vec<Clause>>,
}

impl Module {
    /// Returns an `ExactSizeIterator` of all the clauses that could match the given Rel.
    /// Eventually this ought to perform smart argument-indexing.
    /// If the relation does not exist, `None` is returned.
    pub fn index_match<'m>(
        &'m self,
        rel: &Rel,
    ) -> Option<impl ExactSizeIterator<Item = &'m Clause> + 'm> {
        let sig = rel.into();
        self.relations.get(&sig).map(|clauses| clauses.iter())
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
