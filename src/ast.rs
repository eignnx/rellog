use std::{fmt, rc::Rc};

use crate::{
    data_structures::{Map, Num, Sym, Var},
    tok::Tok,
};

pub type Rel = Map<Sym, Tm>;

/// A term.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Num(Num),
    Txt(String),
    Block(Tok, Vec<Tm>),
    Rel(Rel),
    Cons(Rc<Tm>, Rc<Tm>),
    Nil,
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
        members: &Vec<Tm>,
        f: &mut fmt::Formatter,
    ) -> Result<(), fmt::Error> {
        for member in members {
            writeln!(f)?;
            for _ in 0..self.indent {
                f.write_str("    ")?;
            }
            write!(f, "{functor} {}", self.indented(member))?;
        }
        Ok(())
    }

    fn fmt_rel(&self, map: &Rel, f: &mut fmt::Formatter) -> fmt::Result {
        for (sym, tm) in map {
            match (sym, tm) {
                (s1, Tm::Sym(s2)) if s1 == s2 => {
                    write!(f, "[{sym}]")?;
                }
                (s, Tm::Var(v)) if s.eq_ignore_ascii_case(v) => {
                    write!(f, "[{v}]")?;
                }
                _ => write!(f, "[{sym} {}]", self.with_tm(tm))?,
            }
        }
        Ok(())
    }

    fn fmt_list(&self, mut x: Rc<Tm>, mut xs: Rc<Tm>, f: &mut fmt::Formatter) -> fmt::Result {
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
    RelDef(Rel, Option<Tm>),
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
                    let td = TmDisplayer::default().indented(body);
                    write!(f, " {td}",)?;
                }
                Ok(())
            }
        }
    }
}

/// A single-file program (compilation unit).
#[derive(Debug)]
pub struct Module(pub Vec<Item>);

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
