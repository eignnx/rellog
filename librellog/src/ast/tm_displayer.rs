use std::fmt::{self, Formatter, Write};

use heck::ToPascalCase;
use rpds::Vector;

use crate::{
    ast::ast::{RcTm, Rel, Tm},
    interner::IStr,
    lex::tok::Tok,
};

#[derive(Clone, Default)]
pub struct TmDisplayer<'tm> {
    tm: Option<&'tm Tm>,
    indent: Indent,
}

#[derive(Clone, Copy)]
/// The formula for number of spaces to show at a give level is: `self.offset +
/// 4 * self.indent`
struct Indent {
    /// Used to describe indentation level.
    indent: usize,
    /// Used to add an additional offset (beyond indentation).
    offset: usize,
}

impl Default for Indent {
    fn default() -> Self {
        Self {
            indent: 1,
            offset: 0,
        }
    }
}

impl Indent {
    fn indented(self) -> Self {
        Self {
            indent: self.indent + 1,
            ..self
        }
    }

    fn dedented(self) -> Self {
        Self {
            indent: self.indent - 1,
            ..self
        }
    }

    fn offset_by(self, additional_offset: usize) -> Self {
        Self {
            offset: self.offset + additional_offset,
            ..self
        }
    }
}

impl fmt::Display for Indent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for _ in 0..self.offset {
            // f.write_char('·')?;
            f.write_char(' ')?;
        }
        for _ in 0..self.indent {
            // f.write_str("␣␣␣␣")?;
            f.write_str("    ")?;
        }
        Ok(())
    }
}

impl<'tm> TmDisplayer<'tm> {
    pub fn indented(&self, tm: &'tm Tm) -> Self {
        Self {
            tm: Some(tm),
            indent: self.indent.indented(),
        }
    }

    pub fn dedented(&self) -> Self {
        Self {
            indent: self.indent.dedented(),
            ..self.clone()
        }
    }

    pub fn indented_with_offset(&self, tm: &'tm Tm, additional_offset: usize) -> Self {
        Self {
            tm: Some(tm),
            indent: self.indent.indented().offset_by(additional_offset),
        }
    }

    pub fn with_tm(&self, tm: &'tm Tm) -> Self {
        Self {
            tm: Some(tm),
            ..self.clone()
        }
    }

    fn fmt_sym(&self, f: &mut Formatter<'_>, sym: &IStr) -> fmt::Result {
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
        f: &mut fmt::Formatter,
        functor: &Tok,
        members: &Vector<RcTm>,
    ) -> fmt::Result {
        const SPACE: &str = " ";
        let functor = functor.to_string();

        for member in members {
            writeln!(f)?;
            write!(
                f,
                "{}{functor}{SPACE}{}",
                self.indent,
                self.indented_with_offset(member.as_ref(), functor.len() + SPACE.len())
            )?;
        }
        Ok(())
    }

    const MIN_LINEBREAK_LEN: usize = 80;

    pub fn fmt_rel(&self, map: &Rel, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();

        // First try printing it inline.
        self.fmt_rel_inline(map, &mut buf)?;

        if buf.len() <= Self::MIN_LINEBREAK_LEN {
            write!(f, "{}", buf)
        } else {
            // If line too long, split on to multiple lines.
            self.fmt_rel_splitline(map, f)
        }
    }

    pub fn fmt_rel_splitline(&self, map: &Rel, f: &mut impl fmt::Write) -> fmt::Result {
        for (sym, tm) in map {
            match (sym, tm.as_ref()) {
                (s1, Tm::Sym(s2)) if s1 == s2 => {
                    write!(f, "[{sym}]")?;
                    // writeln!(f, "[{sym}")?;
                    // write!(f, "{}]", self.indent.dedented())?;
                }
                (s, Tm::Var(v)) if s.to_str().to_pascal_case() == v.to_str().as_ref() => {
                    write!(f, "[{v}]")?;
                    // writeln!(f, "[{v}")?;
                    // write!(f, "{}]", self.indent.dedented())?;
                }
                _ => {
                    writeln!(f, "[{sym}")?;
                    writeln!(f, "{}{}", self.indent, self.indented(tm))?;
                    write!(f, "{}]", self.dedented().indent)?;
                }
            }
        }
        Ok(())
    }

    pub fn fmt_rel_inline(&self, map: &Rel, f: &mut impl fmt::Write) -> fmt::Result {
        for (sym, tm) in map {
            match (sym, tm.as_ref()) {
                (s1, Tm::Sym(s2)) if s1 == s2 => {
                    write!(f, "[{sym}]")?;
                }
                (s, Tm::Var(v)) if s.to_str().to_pascal_case() == v.to_str().as_ref() => {
                    write!(f, "[{v}]")?;
                }
                _ => write!(f, "[{sym} {}]", self.with_tm(tm.as_ref()))?,
            }
        }
        Ok(())
    }

    pub fn fmt_list(&self, x: RcTm, xs: RcTm, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();

        // First try printing it inline.
        self.fmt_list_inline(x.clone(), xs.clone(), &mut buf)?;

        if buf.len() <= Self::MIN_LINEBREAK_LEN {
            write!(f, "{}", buf)
        } else {
            // If line too long, split on to multiple lines.
            self.fmt_list_splitline(x, xs, f)
        }
    }

    pub fn fmt_list_inline(
        &self,
        mut x: RcTm,
        mut xs: RcTm,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        const SPACE: char = ' ';

        write!(f, "{{")?;

        loop {
            write!(f, "{}", self.indented(&x))?;

            match xs.as_ref() {
                // xs = {y ...{}} = {y}
                Tm::Cons(y, ys) if matches!(**ys, Tm::Nil) => {
                    write!(f, " {}", self.indented(y))?;
                    return write!(f, "}}");
                }
                // xs = {y ...ys} (continue loop)
                Tm::Cons(y, ys) => (x, xs) = (y.clone(), ys.clone()),
                // xs = {x}
                Tm::Nil => return write!(f, "}}"),
                // Malformed list like: {1 2 ...3} (instead of {1 2 3 ...{}})
                xs => {
                    write!(f, "{SPACE}{}{}", Tok::Spread, self.indented(xs))?;
                    return write!(f, "}}");
                }
            }

            write!(f, "{SPACE}")?;
        }
    }

    pub fn fmt_list_splitline(
        &self,
        mut x: RcTm,
        mut xs: RcTm,
        f: &mut impl fmt::Write,
    ) -> fmt::Result {
        struct Sep(Indent);

        impl fmt::Display for Sep {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                writeln!(f)?;
                write!(f, "{}", self.0)?;
                Ok(())
            }
        }

        struct CloseBrace(Indent);

        impl fmt::Display for CloseBrace {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                writeln!(f)?;
                write!(f, "{}}}", self.0)?;
                Ok(())
            }
        }

        let sep = Sep(self.indent);
        let close_brace = CloseBrace(self.indent.dedented());

        write!(f, "{{{sep}")?;

        loop {
            write!(f, "{}", self.indented(&x))?;

            match &*xs {
                // xs = {y ...{}} = {y}
                Tm::Cons(y, ys) if matches!(**ys, Tm::Nil) => {
                    return write!(f, "{sep}{}{close_brace}", self.indented(y));
                }
                // xs = {y ...ys} (continue loop)
                Tm::Cons(y, ys) => (x, xs) = (y.clone(), ys.clone()),
                // xs = {x}
                Tm::Nil => return write!(f, "{close_brace}"),

                // Malformed list like: {1 2 ...3} (instead of {1 2 3 ...{}})
                xs => {
                    return write!(f, "{sep}{}{}{close_brace}", Tok::Spread, self.indented(xs));
                }
            }

            write!(f, "{sep}")?;
        }
    }

    fn fmt_txt(f: &mut Formatter<'_>, char_list: &char_list::CharList, tail: &RcTm) -> fmt::Result {
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
            write!(f, "[{} {tail}]\"", Tok::Spread)
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
            Tm::Int(i) => write!(f, "{i}"),
            Tm::Txt(char_list, tail) => Self::fmt_txt(f, char_list, tail),
            Tm::Block(functor, members) => self.fmt_block(f, functor, members),
            Tm::Rel(map) => self.fmt_rel(map, f),
            Tm::Cons(x, xs) => self.fmt_list(x.clone(), xs.clone(), f),
            Tm::Nil => write!(f, "{{}}"),
        }
    }
}
