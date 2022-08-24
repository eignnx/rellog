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

#[derive(Debug)]
pub struct At<T>(T, u32, usize);

impl<T> At<T> {
    pub fn without_loc(self) -> T {
        self.0
    }
}

impl Tok {
    pub fn at<'i>(self, i: Span<'i>) -> At<Self> {
        let line = i.location_line();
        let col = i.get_utf8_column();
        At(self, line, col)
    }
}
