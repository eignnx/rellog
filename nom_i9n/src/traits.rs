use nom::AsBytes;
use nom_locate::LocatedSpan;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

pub trait NextTokLoc<I> {
    /// Returns a line-column pair for the next token in the input. The column
    /// value ought to be the column of the *start* of the token.
    fn next_tok_loc(input: &I) -> Option<Loc>;
}

pub trait TokLoc {
    /// Returns a line-column pair for the token. The column value ought to be
    /// the column of the *start* of the token.
    fn tok_loc(&self) -> Loc;
}

/// A trait for collections that can reference their first element.
pub trait First {
    type Item;

    /// Returns the a reference to the first element in this sequence.
    fn first(&self) -> Option<&Self::Item>;
}

impl<T> First for &[T] {
    type Item = T;

    fn first(&self) -> Option<&Self::Item> {
        #[allow(clippy::get_first)]
        self.get(0)
    }
}

impl First for &str {
    type Item = u8;

    fn first(&self) -> Option<&Self::Item> {
        self.as_bytes().first()
    }
}

impl<I, E, X> First for LocatedSpan<I, X>
where
    I: First<Item = E> + AsBytes,
{
    type Item = E;

    fn first(&self) -> Option<&Self::Item> {
        self.fragment().first()
    }
}
