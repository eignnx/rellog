use nom::AsBytes;
use nom_locate::LocatedSpan;

pub trait NextTokCol<I> {
    fn next_tok_col(input: &I) -> Option<usize>;
}

pub trait StartCol {
    fn start_col(&self) -> usize;
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
