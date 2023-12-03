//! # `nom_i9n`
//!
//! Provides an input type and parser combinators which allow for indentation
//! sensitive parsing with `nom`.
//!
//! ## Note: **I9n** Abbreviates **Indentation**
//! Throughout the crate, the word **indentation** has been abbreviated **i9n**
//! (in the word *indentation*, there are nine letters between the *i* and the *n*).

#![feature(result_option_inspect)]

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

/// [`NextTokLoc`] knows how to pluck the first token from the input and examine
/// its column location if the tokens implement [`TokLoc`].
impl<Input, Token> NextTokLoc<Input> for TokenizedInput<Input, Token>
where
    Input: InputLength + First<Item = Token>,
    Token: TokLoc,
{
    fn next_tok_loc(input: &Input) -> Option<Loc> {
        input.first().map(TokLoc::tok_loc)
    }
}

/// Pushes a new indentation level onto the stack, requiring all following
/// tokens to have a column of at least this new indentation.
pub fn begin_block<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
    E: From<I9nError<I>>,
{
    println!("<begin_block TRY {}", loc(&i));
    if i.current_loc().col > i.current_i9n() {
        let i = i.push_i9n(i.current_loc().col);
        println!("SUCCEED {} />", loc(&i));
        return Ok((i, ()));
    }

    println!("FAIL NotGt AtNewGroup {} />", loc(&i));
    let e = i.make_error(I9nRelation::NotGt, I9nErrorCtx::AtNewGroup);
    Err(nom::Err::Error(e.into()))
}

/// If this parser succeeds, it pops one indentation level off the stack,
/// resetting the current indentation to the value it had before the matching
/// `begin_block`.
#[track_caller]
pub fn end_block<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
    E: From<I9nError<I>>,
{
    let col = i.current_loc().col;
    println!("<end_block ");

    if col < i.current_i9n() {
        let i = i.pop_i9n();
        if col > i.current_i9n() {
            let e = i.make_error(I9nRelation::Gt, I9nErrorCtx::AtGroupEnd);
            println!("FAIL Gt AtGroupEnd {} />", loc(&i));
            return Err(nom::Err::Error(e.into()));
        }
        println!("SUCCEED {} />", loc(&i));
        return Ok((i, ()));
    }

    println!("FAIL NotGt AtGroupEnd {} />", loc(&i));
    let e = i.make_error(I9nRelation::NotGt, I9nErrorCtx::AtGroupEnd);
    Err(nom::Err::Error(e.into()))
}

fn loc<I, Tf>(i: &I9nInput<I, Tf>) -> String
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
{
    format!(
        "loc:i9n=\"{:?}\" loc:loc=\"{}:{}\"{}",
        i.stack.into_iter().collect::<Vec<_>>(),
        i.current_loc().line,
        i.current_loc().col,
        if i.at_start_of_line() {
            " at_start_of_line"
        } else {
            ""
        }
    )
}

/// Succeeds if the input column **equals** the current indentation level.
pub fn begin_line<I, Tf, E>(i: I9nInput<I, Tf>) -> IResult<I9nInput<I, Tf>, (), E>
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
    E: From<I9nError<I>>,
{
    println!("<begin_line ");
    if i.current_loc().col == i.current_i9n() && i.at_start_of_line() {
        println!("SUCCEED {} />", loc(&i));
        // Note: we don't advance the input. This means `begin_line` can be
        // parsed repeatedly at the same location.
        return Ok((i, ()));
    }

    println!("FAIL {} />", loc(&i));
    let e = i.make_error(I9nRelation::NotEq, I9nErrorCtx::AtNewLine);
    Err(nom::Err::Error(e.into()))
}

/// Every terminal symbol in your grammar needs to be wrapped in this parser to
/// ensure it respects block indentation.
pub fn tok<I, Tf, O, E>(
    mut p: impl Parser<I9nInput<I, Tf>, O, E>,
) -> impl Parser<I9nInput<I, Tf>, O, E>
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
    E: From<I9nError<I>>,
{
    move |i: I9nInput<I, Tf>| {
        println!("<tok TRY {}>", loc(&i));
        match i.current_loc().col.cmp(&i.current_i9n()) {
            Ordering::Equal if !i.at_start_of_line() => {
                // This would happen if you're still on the same line as before
                // the block, i.e.:
                // ```
                // [blah] - my_block_start
                //        - my_block_middle
                //        - my_block_end
                // ```
                println!("FAIL (Eq WithinLineButAfterStart) {}\n</tok>", loc(&i));
                let e = i.make_error(I9nRelation::Eq, I9nErrorCtx::WithinLineButAfterStart);
                Err(nom::Err::Error(e.into()))
            }

            Ordering::Equal => {
                let (i, o) = p.parse(i).inspect_err(|_| println!("</tok>"))?;
                // i.at_start_of_line = false;
                println!("SUCCEED Ordering::Equal {}\n</tok>", loc(&i));
                Ok((i, o))
            }

            Ordering::Greater => {
                let (i, x) = p.parse(i).inspect_err(|_| println!("</tok>"))?;
                println!("SUCCEED Ordering::Greater {}\n</tok>", loc(&i));
                Ok((i, x))
            }

            Ordering::Less => {
                println!("FAIL (Lt WithinLine) {}\n</tok>", loc(&i));
                let e = i.make_error(I9nRelation::Lt, I9nErrorCtx::WithinLine);
                Err(nom::Err::Error(e.into()))
            }
        }
    }
}

/// Succeeds if the provided parser is begins at a column *strictly greater*
/// than the current indentation level.
///
/// ## Example
/// In this example, the return statements could be parsed with this parser.
/// ```python
/// def ok_one(): return 1
///
/// def ok_two():
///     return 1
///
/// def bad_three():
/// return 3 # Would not parse
/// ```
pub fn indented<I, Tf, O, E>(
    line_parser: &mut impl Parser<I9nInput<I, Tf>, O, E>,
) -> impl Parser<I9nInput<I, Tf>, O, E> + '_
where
    I: Clone + InputLength,
    Tf: NextTokLoc<I> + Clone,
    E: From<I9nError<I>> + ParseError<I9nInput<I, Tf>>,
{
    move |i: I9nInput<I, Tf>| {
        let (i, ()) = begin_block(i)?;
        let (i, o) = line_parser.parse(i)?;
        let (i, ()) = end_block(i)?;
        Ok((i, o))
    }
}

/// Succeeds if the next token is indented, `line_parser` succeeds at least
/// once, and the block is then dedented.
///
/// To be in an indented block, every token needs to be in a column greater than
/// or equal to the indentation of the first token in the block.
pub fn indented_block<I, Tf, O, E>(
    line_parser: &mut impl Parser<I9nInput<I, Tf>, O, E>,
) -> impl Parser<I9nInput<I, Tf>, Vec<O>, E> + '_
where
    I: Clone + InputLength,
    Tf: NextTokLoc<I> + Clone,
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

/// This only parses if both: the input is at the beginning of a new line, and
/// the provided parser `p` succeeds.
pub fn on_new_line<I, Tf, P, O, E>(p: P) -> impl Parser<I9nInput<I, Tf>, O, E>
where
    I: Clone + InputLength,
    Tf: NextTokLoc<I> + Clone,
    P: Parser<I9nInput<I, Tf>, O, E>,
    E: From<I9nError<I>> + ParseError<I9nInput<I, Tf>>,
{
    preceded(begin_line, p)
}
