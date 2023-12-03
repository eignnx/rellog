use std::{fmt, marker::PhantomData, ops::RangeFrom};

use nom::Slice;

use crate::{
    errors::{I9nError, I9nErrorCtx, I9nErrorSituation, I9nRelation},
    First, Loc, NextTokLoc, TokenizedInput,
};

#[derive(Clone)]
pub struct I9nInput<Input, TokenFinder> {
    pub(crate) input: Input,
    pub(crate) prev_line: usize,
    pub(crate) stack: rpds::Stack<usize>,
    pub(crate) _token_finder: PhantomData<TokenFinder>,
}

impl<I, Tf> fmt::Debug for I9nInput<I, Tf>
where
    I: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("I9nInput")
            .field("input", &self.input)
            .field("prev_line", &self.prev_line)
            .field("stack", &self.stack)
            .finish()
    }
}

impl<I, Tf> fmt::Display for I9nInput<I, Tf>
where
    I: fmt::Display,
    Tf: NextTokLoc<I>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.input())
    }
}

impl<I, Tf> I9nInput<I, Tf> {
    #[inline]
    pub fn input(&self) -> &I {
        &self.input
    }
}

impl<I, Tf> I9nInput<I, Tf>
where
    Tf: NextTokLoc<I>,
{
    pub fn at_start_of_line(&self) -> bool {
        Tf::next_tok_loc(&self.input)
            .map(|loc| loc.line > self.prev_line)
            .unwrap_or(true)
    }

    pub fn current_i9n(&self) -> usize {
        self.stack.peek().copied().unwrap_or(1)
    }
}

impl<I, T> I9nInput<I, TokenizedInput<I, T>>
where
    Self: Slice<RangeFrom<usize>>,
    I: First<Item = T>,
{
    pub fn split_first(&self) -> Option<(&T, Self)> {
        match self.input().first() {
            Some(first) => {
                let rest = self.slice(1..);
                Some((first, rest))
            }
            _ => None,
        }
    }
}

impl<I, Tf> I9nInput<I, Tf>
where
    I: Clone,
    Tf: NextTokLoc<I> + Clone,
{
    /// Returns the line-column pair of the next token in the input.
    pub fn current_loc(&self) -> Loc {
        const EOF_LINE: usize = usize::MAX;
        const EOF_COLUMN: usize = 0; // Note: beginning of line is usually at column 1.
        Tf::next_tok_loc(&self.input).unwrap_or(Loc {
            line: EOF_LINE,
            col: EOF_COLUMN,
        })
    }

    pub(crate) fn push_i9n(&self, col: usize) -> Self {
        Self {
            stack: self.stack.push(col),
            ..Clone::clone(self)
        }
    }

    #[track_caller]
    pub(crate) fn pop_i9n(&self) -> Self {
        Self {
            stack: self.stack.pop().unwrap(),
            ..Clone::clone(self)
        }
    }

    #[inline]
    pub fn make_error(&self, relation: I9nRelation, ctx: I9nErrorCtx) -> I9nError<I> {
        I9nError {
            input: self.input.clone(),
            situation: I9nErrorSituation {
                relation,
                expected: self.current_i9n(),
                actual: self.current_loc().col,
            },
            ctx,
        }
    }
}
