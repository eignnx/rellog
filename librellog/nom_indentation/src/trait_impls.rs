use std::{
    ops::{Deref, RangeFrom, RangeTo},
    str::FromStr,
};

use nom::{
    error::ParseError, AsBytes, Compare, ExtendInto, FindSubstring, FindToken, IResult, InputIter,
    InputLength, InputTake, InputTakeAtPosition, Offset, ParseTo, Slice,
};
use nom_locate::LocatedSpan;
use rpds::Stack;

use crate::{I9nInput, NextTokCol};

impl<I, X> NextTokCol for LocatedSpan<I, X>
where
    I: AsBytes,
{
    fn next_tok_col(&self) -> usize {
        self.get_column()
    }
}

impl<I> Deref for I9nInput<I> {
    type Target = I;

    fn deref(&self) -> &Self::Target {
        &self.input
    }
}

impl<I: AsBytes> AsBytes for I9nInput<I> {
    fn as_bytes(&self) -> &[u8] {
        self.input.as_bytes()
    }
}

impl<I> InputTake for I9nInput<I>
where
    Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>>,
{
    fn take(&self, count: usize) -> Self {
        self.slice(..count)
    }

    fn take_split(&self, count: usize) -> (Self, Self) {
        (self.slice(count..), self.slice(..count))
    }
}

impl<T> InputTakeAtPosition for I9nInput<T>
where
    T: InputTakeAtPosition + InputLength + InputIter,
    Self: Slice<RangeFrom<usize>> + Slice<RangeTo<usize>> + Clone,
{
    type Item = <T as InputIter>::Item;

    fn split_at_position_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.split_at_position(predicate) {
            Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
            res => res,
        }
    }

    fn split_at_position<P, E: ParseError<Self>>(&self, predicate: P) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.position(predicate) {
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
        }
    }

    fn split_at_position1_complete<P, E: ParseError<Self>>(
        &self,
        predicate: P,
        e: nom::error::ErrorKind,
    ) -> IResult<Self, Self, E>
    where
        P: Fn(Self::Item) -> bool,
    {
        match self.input.position(predicate) {
            Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
            Some(n) => Ok(self.take_split(n)),
            None => {
                if self.input.input_len() == 0 {
                    Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                } else {
                    Ok(self.take_split(self.input_len()))
                }
            }
        }
    }
}

impl<I: Offset> Offset for I9nInput<I> {
    fn offset(&self, second: &Self) -> usize {
        self.input.offset(second)
    }
}

impl<R, T> ParseTo<R> for I9nInput<T>
where
    T: ParseTo<R>,
    R: FromStr,
{
    fn parse_to(&self) -> Option<R> {
        self.input.parse_to()
    }
}

// impl<'a, T, R, X: Clone> Slice<R> for LocatedSpan<T, X> where
//     T: Slice<R> + Offset + AsBytes + Slice<RangeTo<usize>>,
impl<'a, I, R> Slice<R> for I9nInput<I>
where
    I: Slice<R> + Offset + AsBytes + Slice<RangeTo<usize>> + Clone,
{
    fn slice(&self, range: R) -> Self {
        Self {
            input: self.input.slice(range),
            ..self.clone()
        }
    }
}

impl<I> InputLength for I9nInput<I>
where
    I: InputLength,
{
    fn input_len(&self) -> usize {
        self.input.input_len()
    }
}

impl<I1, I2> Compare<I2> for I9nInput<I1>
where
    I1: Compare<I2>,
    I2: Into<I9nInput<I2>>,
{
    fn compare(&self, t: I2) -> nom::CompareResult {
        self.input.compare(t)
    }

    fn compare_no_case(&self, t: I2) -> nom::CompareResult {
        self.input.compare_no_case(t)
    }
}

impl<'a, I> ExtendInto for I9nInput<I>
where
    I: ExtendInto,
{
    type Item = I::Item;

    type Extender = I::Extender;

    fn new_builder(&self) -> Self::Extender {
        self.input.new_builder()
    }

    fn extend_into(&self, acc: &mut Self::Extender) {
        self.input.extend_into(acc)
    }
}

impl<I1, I2> FindSubstring<I2> for I9nInput<I1>
where
    I1: FindSubstring<I2>,
{
    fn find_substring(&self, substr: I2) -> Option<usize> {
        self.input.find_substring(substr)
    }
}

impl<I, Token> FindToken<Token> for I9nInput<I>
where
    I: FindToken<Token>,
{
    fn find_token(&self, token: Token) -> bool {
        self.input.find_token(token)
    }
}

impl<I> From<I> for I9nInput<I> {
    fn from(input: I) -> Self {
        Self {
            input,
            at_start_of_line: true,
            stack: Stack::new(),
        }
    }
}

impl<I> InputIter for I9nInput<I>
where
    I: InputIter,
{
    type Item = I::Item;

    type Iter = I::Iter;

    type IterElem = I::IterElem;

    fn iter_indices(&self) -> Self::Iter {
        self.input.iter_indices()
    }

    fn iter_elements(&self) -> Self::IterElem {
        self.input.iter_elements()
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.input.position(predicate)
    }

    fn slice_index(&self, count: usize) -> Result<usize, nom::Needed> {
        self.input.slice_index(count)
    }
}
