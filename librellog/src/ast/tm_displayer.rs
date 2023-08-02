use std::fmt::{self, Formatter};

use rpds::Vector;

use crate::{
    ast::ast::{RcTm, Rel, Tm},
    interner::IStr,
    lex::tok::Tok,
};

#[derive(Default)]
pub struct TmDisplayer<'tm> {
    indent: usize,
    tm: Option<&'tm Tm>,
}

impl<'tm> TmDisplayer<'tm> {
    pub fn indented(&self, tm: impl Into<Option<&'tm Tm>>) -> Self {
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

    fn fmt_sym(&self, f: &mut Formatter<'_>, sym: &IStr) -> Result<(), fmt::Error> {
        let sym = sym.to_str();
        if !sym.is_empty()
            && !sym.contains(|c: char| !c.is_alphanumeric())
            && sym
                .chars()
                .next()
                .map(|c| c.is_alphabetic() && c.is_ascii_lowercase())
                .expect("empty str case handled above")
        {
            write!(f, "{sym}")
        } else {
            write!(f, "'{sym}'")
        }
    }

    pub fn fmt_block(
        &self,
        functor: &Tok,
        members: &Vector<RcTm>,
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

    pub fn fmt_rel(&self, map: &Rel, f: &mut fmt::Formatter) -> fmt::Result {
        for (sym, tm) in map {
            match (sym, tm.as_ref()) {
                (s1, Tm::Sym(s2)) if s1 == s2 => {
                    write!(f, "[{sym}]")?;
                }
                (s, Tm::Var(v))
                    if s.to_str().eq_ignore_ascii_case(v.to_str().as_ref()) && v.is_original() =>
                {
                    write!(f, "[{v}]")?;
                }
                _ => write!(f, "[{sym} {}]", self.with_tm(tm.as_ref()))?,
            }
        }
        Ok(())
    }

    pub fn fmt_list(&self, mut x: RcTm, mut xs: RcTm, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;

        loop {
            write!(f, "{}", self.indented(&*x))?;

            match &*xs {
                // xs = {y ...{}} = {y}
                Tm::Cons(y, ys) if matches!(**ys, Tm::Nil) => {
                    return write!(f, " {}}}", self.indented(&**y))
                }
                // xs = {y ...ys} (continue loop)
                Tm::Cons(y, ys) => (x, xs) = (y.clone(), ys.clone()),
                // xs = {x}
                Tm::Nil => return write!(f, "}}"),
                // Malformed list like: {1 2 ...3} (instead of {1 2 3 ...{}})
                xs => return write!(f, " {}{}}}", Tok::Spread, self.indented(xs)),
            }

            write!(f, " ")?;
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
            Tm::Sym(s) => self.fmt_sym(f, s),
            Tm::Var(v) => write!(f, "{v}"),
            Tm::Num(i) => write!(f, "{i}"),
            Tm::Txt(char_list, tail) => {
                let mut char_list = char_list;
                let mut tail = tail;

                write!(f, "\"{char_list}")?;
                while let Tm::Txt(cl, tl) = tail.as_ref() {
                    char_list = cl;
                    tail = tl;
                    write!(f, "{char_list}")?;
                }

                if let Tm::Nil = tail.as_ref() {
                    write!(f, "\"")
                } else {
                    // If it's not text, and the tail wasn't Nil, break
                    // and display the tail (either Var or malformed).
                    write!(f, "[.. {tail}]\"")
                }
            }
            Tm::Block(functor, members) => self.fmt_block(functor, members, f),
            Tm::Rel(map) => self.fmt_rel(map, f),
            Tm::Cons(x, xs) => self.fmt_list(x.clone(), xs.clone(), f),
            Tm::Nil => write!(f, "{{}}"),
        }
    }
}
