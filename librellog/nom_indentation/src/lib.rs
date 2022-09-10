//! # `nom_indentation`
//!
//! Provides an input type which allows for indentation sensitive parsing.
//!
//! ## Note: **I9n** Abbreviates **Indentation**
//! Throughout the crate, the word **indentation** has been abbreviated **i9n**
//! (in the word *indentation*, there are nine letters between the *i* and the *n*).

use std::cmp::Ordering;

use nom::{IResult, Parser};
mod trait_impls;

#[derive(Debug, Clone)]
pub struct I9nInput<Input> {
    input: Input,
    at_start_of_line: bool,
    stack: rpds::Stack<usize>,
}

pub trait NextTokCol {
    fn next_tok_col(&self) -> usize;
}

impl<I> NextTokCol for I9nInput<I>
where
    I: NextTokCol,
{
    fn next_tok_col(&self) -> usize {
        self.input.next_tok_col()
    }
}

pub fn begin_block<I, E>(i: I9nInput<I>) -> IResult<I9nInput<I>, (), E>
where
    I: NextTokCol + Clone,
    E: From<I9nError<I>>,
{
    if i.next_tok_col() > i.current_i9n() {
        let i = i.push_i9n(i.next_tok_col());
        return Ok((i, ()));
    }

    let e = i.make_error(I9nRelation::NotGt, I9nErrorCtx::AtNewGroup);
    Err(nom::Err::Error(e.into()))
}

#[track_caller]
pub fn end_block<I, E>(i: I9nInput<I>) -> IResult<I9nInput<I>, (), E>
where
    I: NextTokCol + Clone,
    E: From<I9nError<I>>,
{
    let col = i.next_tok_col();

    if col < i.current_i9n() {
        let i = i.pop_i9n();
        if col > i.current_i9n() {
            let e = i.make_error(I9nRelation::Gt, I9nErrorCtx::AtGroupEnd);
            return Err(nom::Err::Failure(e.into()));
        }
        return Ok((i, ()));
    }

    let e = i.make_error(I9nRelation::NotGt, I9nErrorCtx::AtGroupEnd);
    Err(nom::Err::Error(e.into()))
}

pub fn begin_line<I, E>(i: I9nInput<I>) -> IResult<I9nInput<I>, (), E>
where
    I: NextTokCol + Clone,
    E: From<I9nError<I>>,
{
    if i.next_tok_col() == i.current_i9n() {
        let mut i = i.clone();
        i.at_start_of_line = true;
        return Ok((i, ()));
    }

    let e = i.make_error(I9nRelation::NotEq, I9nErrorCtx::AtNewLine);
    Err(nom::Err::Error(e.into()))
}

pub fn tok<I, O, E>(mut p: impl Parser<I9nInput<I>, O, E>) -> impl Parser<I9nInput<I>, O, E>
where
    I: NextTokCol + Clone,
    E: From<I9nError<I>>,
{
    move |i: I9nInput<I>| match i.next_tok_col().cmp(&i.current_i9n()) {
        Ordering::Equal if i.at_start_of_line => {
            let (mut i, o) = p.parse(i)?;
            i.at_start_of_line = false;
            Ok((i, o))
        }

        Ordering::Equal => {
            let e = i.make_error(I9nRelation::Eq, I9nErrorCtx::WithinLineButAfterStart);
            Err(nom::Err::Error(e.into()))
        }

        Ordering::Greater => p.parse(i),

        Ordering::Less => {
            let e = i.make_error(I9nRelation::Lt, I9nErrorCtx::WithinLine);
            Err(nom::Err::Error(e.into()))
        }
    }
}

impl<I: NextTokCol + Clone> I9nInput<I> {
    pub fn input(&self) -> &I {
        &self.input
    }

    fn current_i9n(&self) -> usize {
        self.stack.peek().copied().unwrap_or(1)
    }

    fn push_i9n(&self, col: usize) -> Self {
        Self {
            stack: self.stack.push(col),
            ..self.clone()
        }
    }

    #[track_caller]
    fn pop_i9n(&self) -> Self {
        Self {
            stack: self.stack.pop().unwrap(),
            ..self.clone()
        }
    }

    #[inline]
    fn make_error(&self, relation: I9nRelation, ctx: I9nErrorCtx) -> I9nError<I> {
        I9nError {
            input: self.input.clone(),
            situation: I9nErrorSituation {
                relation,
                expected: self.current_i9n(),
                actual: self.next_tok_col(),
            },
            ctx,
        }
    }
}

#[derive(Debug, Clone)]
pub struct I9nError<I> {
    pub input: I,
    pub situation: I9nErrorSituation,
    pub ctx: I9nErrorCtx,
}

#[derive(Debug, Clone)]
pub struct I9nErrorSituation {
    pub relation: I9nRelation,
    pub expected: usize,
    pub actual: usize,
}

#[derive(Debug, Clone)]
pub enum I9nRelation {
    NotGt,
    NotEq,
    Gt,
    Eq,
    Lt,
}

#[derive(Debug, Clone, Copy)]
pub enum I9nErrorCtx {
    AtNewGroup,
    WithinLine,
    AtNewLine,
    AtGroupEnd,
    WithinLineButAfterStart,
}
