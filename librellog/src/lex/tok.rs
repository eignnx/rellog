use core::fmt;

use char_list::CharList;
use nom_i9n::StartCol;

use crate::{
    data_structures::{Int, Sym, Var},
    utils::my_nom::Span,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

    Semicolon,

    /// The "cons operator" which looks like `..`
    Spread,

    /// The equal sign `=`.
    Equal,

    /// The double colon symbol `::`.
    PathSep,

    Sym(Sym),
    Var(Var),
    Int(Int),
    Txt(CharList),
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Tok::OBrack => write!(f, "["),
            Tok::CBrack => write!(f, "]"),
            Tok::COBrack => write!(f, "]["),
            Tok::OBrace => write!(f, "{{"),
            Tok::CBrace => write!(f, "}}"),
            Tok::OParen => write!(f, "("),
            Tok::CParen => write!(f, ")"),
            Tok::Dash => write!(f, "-"),
            Tok::Pipe => write!(f, "|"),
            Tok::Comma => write!(f, ","),
            Tok::Semicolon => write!(f, ";"),
            Tok::Spread => write!(f, ".."),
            Tok::Equal => write!(f, "="),
            Tok::PathSep => write!(f, "::"),
            Tok::Sym(s) => write!(f, "{s}"),
            Tok::Var(v) => write!(f, "{v}"),
            Tok::Int(i) => write!(f, "{i}"),
            Tok::Txt(s) if s.as_str().contains('\n') => {
                write!(f, "\"\"\"\n{s}\n\"\"\"")
            }
            Tok::Txt(s) => write!(f, "\"{s}\""),
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

impl<T> StartCol for At<T> {
    fn start_col(&self) -> usize {
        self.col
    }
}
