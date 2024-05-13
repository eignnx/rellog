use core::fmt;

use nom_i9n::TokLoc;

use crate::{
    data_structures::{Int, Sym, Var},
    utils::my_nom::Span,
};

pub const SPREAD: &str = "..";

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

    /// The tilde symbol `~`.
    Tilde,

    /// The double colon symbol `::`.
    PathSep,

    Sym(Sym),
    Var(Var),
    Int(Int),

    // ------------------------Text-related Tokens------------------------------
    /// A double quote mark (`"`) which begins a text template literal.
    OQuote,

    /// A double quote mark (`"`) which ends a text template literal.
    CQuote,

    /// Three double quote marks (`"""`) which begin a text template literal.
    OTripleQuote,

    /// Three double quote marks (`"""`) which end a text template literal.
    CTripleQuote,

    /// A run of literal characters contained in a text template literal.
    /// ```text
    /// "abcdefg[{H I J}]klmnop[{..Rest}]"
    ///  ^^^^^^^         ^^^^^^
    ///  ex1             ex2
    /// ```
    TxtContent(String),

    /// The symbol `[{` in a text template literal.
    OTxtInterp,

    /// The symbol `}]` in a text template literal.
    CTxtInterp,
}

impl fmt::Display for Tok {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Tok::OBrack => "[",
                Tok::CBrack => "]",
                Tok::COBrack => "][",
                Tok::OBrace => "{",
                Tok::CBrace => "}",
                Tok::OParen => "(",
                Tok::CParen => ")",
                Tok::Dash => "-",
                Tok::Pipe => "|",
                Tok::Comma => ",",
                Tok::Semicolon => ";",
                Tok::Spread => SPREAD,
                Tok::Equal => "=",
                Tok::Tilde => "~",
                Tok::PathSep => "::",
                Tok::Sym(s) => return write!(f, "{s}"),
                Tok::Var(v) => return write!(f, "{v}"),
                Tok::Int(i) => return write!(f, "{i}"),

                Tok::OQuote => "\"…",
                Tok::CQuote => "…\"",
                Tok::OTripleQuote => "\"\"\"…",
                Tok::CTripleQuote => "…\"\"\"",
                Tok::TxtContent(s) => return write!(f, "\"…{s}…\""),
                Tok::OTxtInterp => "[{",
                Tok::CTxtInterp => "}]",
            }
        )
    }
}

#[derive(Clone)]
pub struct At<T> {
    pub value: T,
    pub line: u32,
    pub col: usize,
}

impl<T: fmt::Debug> fmt::Debug for At<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}@{}:{}", self.value, self.line, self.col)
    }
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

impl<T> TokLoc for At<T> {
    fn tok_loc(&self) -> nom_i9n::Loc {
        nom_i9n::Loc {
            line: self.line as usize,
            col: self.col,
        }
    }
}
