use core::fmt;

use crate::{
    data_structures::{Sym, Var},
    my_nom::Span,
};

#[derive(Debug, Clone, PartialEq, Eq)]
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

    Indent,
    Dedent,
}

impl Tok {
    pub fn at<'i>(self, i: Span<'i>) -> At<Self> {
        let line = i.location_line();
        let col = i.get_utf8_column();
        At(self, line, col)
    }
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Tok::*;
        match self {
            OBrack => write!(f, "`[`"),
            CBrack => write!(f, "`]`"),
            COBrack => write!(f, "`][`"),
            OBrace => write!(f, "`{{`"),
            CBrace => write!(f, "`}}`"),
            OParen => write!(f, "`(`"),
            CParen => write!(f, "`)`"),
            Dash => write!(f, "`-`"),
            Pipe => write!(f, "`|`"),
            Comma => write!(f, "`,`"),
            Spread => write!(f, "`...`"),
            Sym(s) => write!(f, "symbol `{s}`"),
            Var(v) => write!(f, "variable `{v}`"),
            Indent => write!(f, "<INDENT>"),
            Dedent => write!(f, "<DEDENT>"),
        }
    }
}

#[derive(Debug)]
pub struct At<T>(T, u32, usize);

impl<T> At<T> {
    pub fn without_loc(self) -> T {
        self.0
    }
}
