//! # `nom_i9n`
//!
//! Provides an input type and parser combinators which allow for indentation
//! sensitive parsing with `nom`.
//!
//! ## Note: **I9n** Abbreviates **Indentation**
//! Throughout the crate, the word **indentation** has been abbreviated **i9n**
//! (in the word *indentation*, there are nine letters between the *i* and the *n*).

pub use crate::{errors::*, i9n_input::*, traits::*};
use nom::{error::ParseError, multi::many1, sequence::preceded, IResult, InputLength, Parser};
use std::{cmp::Ordering, fmt::Debug, marker::PhantomData};

mod errors;
mod i9n_input;
mod trait_impls;
mod traits;

/// Used as a [`I9nInput`]'s `TokenFinder` type parameter.
///
/// ## When to use?
/// When your underlying input has already been tokenized into a stream of
/// semantic tokens.
///
/// ## Why to use?
/// This type implements the [`NextTokCol`] trait, which is needed by the
/// indentation parsers in this crate.
///
/// Note that you'll also need to implement [`StartCol`] for your token type.
/// This teaches the parsers how to pluck start-column information out of your
/// tokens.
#[derive(Debug, Clone)]
pub struct TokenizedInput<Input, Token> {
    _input: PhantomData<Input>,
    _token: PhantomData<Token>,
}

/// [`FrontOfTokenizedInput`] knows how to pluck the first token from the input
/// and examine its column location.
impl<Input, Token> NextTokCol<Input> for TokenizedInput<Input, Token>
where
    Input: InputLength + First<Item = Token>,
    Token: StartCol,
{
    fn next_tok_col(input: &Input) -> Option<usize> {
        input.first().map(StartCol::start_col)
    }
}

pub fn begin_block<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokCol<I> + Clone,
    E: From<I9nError<I>>,
{
    if i.current_col() > i.current_i9n() {
        let i = i.push_i9n(i.current_col());
        return Ok((i, ()));
    }

    let e = i.make_error(I9nRelation::NotGt, I9nErrorCtx::AtNewGroup);
    Err(nom::Err::Error(e.into()))
}

#[track_caller]
pub fn end_block<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokCol<I> + Clone,
    E: From<I9nError<I>>,
{
    let col = i.current_col();

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

pub fn begin_line<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokCol<I> + Clone,
    E: From<I9nError<I>>,
{
    if i.current_col() == i.current_i9n() {
        let mut i = Clone::clone(&i);
        i.at_start_of_line = true;
        return Ok((i, ()));
    }

    let e = i.make_error(I9nRelation::NotEq, I9nErrorCtx::AtNewLine);
    Err(nom::Err::Error(e.into()))
}

pub fn tok<I, Tf, O, E>(
    mut p: impl Parser<I9nInput<I, Tf>, O, E>,
) -> impl Parser<I9nInput<I, Tf>, O, E>
where
    I: Clone,
    Tf: NextTokCol<I> + Clone,
    E: From<I9nError<I>>,
{
    move |i: I9nInput<I, Tf>| match i.current_col().cmp(&i.current_i9n()) {
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

pub fn indented_block<'lp, I, Tf, O, E>(
    line_parser: &'lp mut impl Parser<I9nInput<I, Tf>, O, E>,
) -> impl Parser<I9nInput<I, Tf>, Vec<O>, E> + 'lp
where
    I: Clone + InputLength,
    Tf: NextTokCol<I> + Clone,
    E: From<I9nError<I>> + ParseError<I9nInput<I, Tf>>,
{
    move |i: I9nInput<I, Tf>| {
        let (i, ()) = begin_block(i)?;
        let line_parser = |ts| line_parser.parse(ts);
        let (i, o) = many1(line_parser).parse(i)?;
        let (i, ()) = end_block(i)?;
        Ok((i, o))
    }
}

pub fn line<I, Tf, P, O, E>(p: P) -> impl Parser<I9nInput<I, Tf>, O, E>
where
    I: Clone + InputLength,
    Tf: NextTokCol<I> + Clone,
    P: Parser<I9nInput<I, Tf>, O, E>,
    E: From<I9nError<I>> + ParseError<I9nInput<I, Tf>>,
{
    preceded(begin_line, p)
}
