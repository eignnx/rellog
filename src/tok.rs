use core::fmt;

use crate::{
    data_structures::{Num, Sym, Var},
    my_nom::Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Tok {
    /// Open square bracket `[`
    OBrack,

    /// Close square bracket `]`
    CBrack,

    /// The symbol `][`.
    COBrack,

    /// Open curly brace `{`
    OBrace,

    /// Close curly brace `}`
    CBrace,

    /// Open parenthesis `(`
    OParen,

    /// Close parenthesis `)`
    CParen,

    Dash,
    Pipe,
    Comma,

    /// The "cons operator" which looks like `...`
    Spread,

    Sym(Sym),
    Var(Var),
    Num(Num),
    Txt(String),

    Indent,
    Dedent,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Tok::*;
        match self {
            OBrack => write!(f, "["),
            CBrack => write!(f, "]"),
            COBrack => write!(f, "]["),
            OBrace => write!(f, "{{"),
            CBrace => write!(f, "}}"),
            OParen => write!(f, "("),
            CParen => write!(f, ")"),
            Dash => write!(f, "-"),
            Pipe => write!(f, "|"),
            Comma => write!(f, ","),
            Spread => write!(f, "..."),
            Sym(s) => write!(f, "{}", s.to_str()),
            Var(v) => write!(f, "{}", v.to_str()),
            Num(i) => write!(f, "{i}"),
            Txt(s) => write!(f, "\"{s}\""),
            Indent => write!(f, "<INDENT>"),
            Dedent => write!(f, "<DEDENT>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct At<T> {
    pub value: T,
    pub line: u32,
    pub col: usize,
}

impl<T> At<T> {
    pub fn value(self) -> T {
        self.value
    }
}

impl<T> AsRef<T> for At<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

pub trait MakeAt: Sized {
    fn at(self, i: Span) -> At<Self>;
    fn copy_loc<T>(self, other: &At<T>) -> At<Self>;
}

impl<T> MakeAt for T {
    fn at(self, i: Span) -> At<Self> {
        let line = i.location_line();
        let col = i.get_utf8_column();
        At {
            value: self,
            line,
            col,
        }
    }

    fn copy_loc<U>(self, other: &At<U>) -> At<Self> {
        At {
            value: self,
            line: other.line,
            col: other.col,
        }
    }
}
