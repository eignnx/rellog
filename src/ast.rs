use std::fmt;

use crate::{
    data_structures::{Map, Sym, Var},
    tok::Tok,
};

pub type Rel = Map<Sym, Tm>;

/// A term.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Block(Tok, Vec<Tm>),
    Rel(Rel),
}

#[derive(Default)]
pub struct TmDisplayer {
    indent: usize,
    tm: Option<Tm>,
}

impl TmDisplayer {
    pub fn new(tm: Tm) -> Self {
        Self {
            tm: Some(tm),
            ..Self::default()
        }
    }

    fn indented(&self, tm: impl Into<Option<Tm>>) -> Self {
        Self {
            indent: self.indent + 1,
            tm: tm.into(),
        }
    }

    fn replace(&self, tm: Tm) -> Self {
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
            write!(f, "{functor} {}", self.indented(member.clone()))?;
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
                _ => write!(f, "[{sym} {}]", self.replace(tm.clone()))?,
            }
        }
        Ok(())
    }
}

impl fmt::Display for TmDisplayer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tm = self
            .tm
            .as_ref()
            .expect("fmt::Display::fmt will not be called on empty TmDisplayer");

        match tm {
            Tm::Sym(s) => write!(f, "{s}"),
            Tm::Var(v) => write!(f, "{v}"),
            Tm::Block(functor, members) => self.fmt_block(functor, members, f),
            Tm::Rel(map) => self.fmt_rel(map, f),
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
                    let td = TmDisplayer::default().indented(body.clone());
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
        for item in &self.0 {
            write!(f, "{item}\n\n")?;
        }
        Ok(())
    }
}
