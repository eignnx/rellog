use std::{fmt, iter, ops::Deref, rc::Rc};

use unifier_set::{ClassifyTerm, DirectChildren, TermKind};

use crate::{
    data_structures::{Map, Num, Sym, Var},
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
    Block(Tok, Vec<RcTm>),
    Rel(Rel),
    Cons(RcTm, RcTm),
    Nil,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RcTm(Rc<Tm>);

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

#[derive(Default)]
pub struct TmDisplayer<'tm> {
    indent: usize,
    tm: Option<&'tm Tm>,
}

impl<'tm> TmDisplayer<'tm> {
    fn indented(&self, tm: impl Into<Option<&'tm Tm>>) -> Self {
        Self {
            indent: self.indent + 1,
            tm: tm.into(),
        }
    }

    pub fn with_tm(&self, tm: &'tm Tm) -> Self {
        Self {
            tm: Some(tm),
            ..*self
        }
    }

    fn fmt_block(
        &self,
        functor: &Tok,
        members: &Vec<RcTm>,
        f: &mut fmt::Formatter,
    ) -> Result<(), fmt::Error> {
        for member in members {
            writeln!(f)?;
            for _ in 0..self.indent {
                f.write_str("    ")?;
            }
            write!(f, "{functor} {}", self.indented(member.as_ref()))?;
        }
        Ok(())
    }

    fn fmt_rel(&self, map: &Rel, f: &mut fmt::Formatter) -> fmt::Result {
        for (sym, tm) in map {
            match (sym, tm.as_ref()) {
                (s1, Tm::Sym(s2)) if s1 == s2 => {
                    write!(f, "[{sym}]")?;
                }
                (s, Tm::Var(v)) if s.to_str().eq_ignore_ascii_case(v.to_str().as_ref()) => {
                    write!(f, "[{v}]")?;
                }
                _ => write!(f, "[{sym} {}]", self.with_tm(tm.as_ref()))?,
            }
        }
        Ok(())
    }

    fn fmt_list(&self, mut x: RcTm, mut xs: RcTm, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;

        loop {
            write!(f, "{}", self.indented(&*x))?;

            match &*xs {
                // xs = {y, ...{}} = {y}
                Tm::Cons(y, ys) if matches!(**ys, Tm::Nil) => {
                    return write!(f, ", {}}}", self.indented(&**y))
                }
                // xs = {y, ...ys} (continue loop)
                Tm::Cons(y, ys) => (x, xs) = (y.clone(), ys.clone()),
                // xs = {x}
                Tm::Nil => return write!(f, "}}"),
                // Malformed list like: {1, 2, ...3} (instead of {1, 2, 3, ...{}})
                xs => return write!(f, " {}{}}}", Tok::Spread, self.indented(&*xs)),
            }

            write!(f, ", ")?;
        }
    }
}

impl<'tm> fmt::Display for TmDisplayer<'tm> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tm = self
            .tm
            .as_ref()
            .expect("fmt::Display::fmt will not be called on empty TmDisplayer");

        match tm {
            Tm::Sym(s) => write!(f, "{s}"),
            Tm::Var(v) => write!(f, "{v}"),
            Tm::Num(i) => write!(f, "{i}"),
            Tm::Txt(s) => write!(f, "\"{s}\""),
            Tm::Block(functor, members) => self.fmt_block(functor, members, f),
            Tm::Rel(map) => self.fmt_rel(map, f),
            Tm::Cons(x, xs) => self.fmt_list(x.clone(), xs.clone(), f),
            Tm::Nil => write!(f, "{{}}"),
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
