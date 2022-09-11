//! Goal right now:
//! Parse very simple versions of Haskell's `do` expressions.
//!
//! ```ignore
//! expr --> 'ident'>
//! expr --> 'do'> |stmts|>
//!
//! stmts --> |expr|=
//! stmts --> stmts= |stmt|=
//!
//! stmt --> 'ident'> '='> expr=
//! ```
//!

use std::fmt;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{multispace0, multispace1},
    error::ParseError,
    multi::many1,
    sequence::{preceded, tuple},
    IResult, Parser, Slice,
};
use nom_indentation::{begin_block, begin_line, end_block, tok, I9nError, I9nInput, NextTokCol};
use nom_locate::LocatedSpan;

struct Bad(String);

impl fmt::Debug for Bad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Bad({})", self.0)
    }
}

#[derive(Debug, Clone)]
struct SkipWsTf;

impl<'i> NextTokCol<LocatedSpan<&'i str>> for SkipWsTf {
    fn next_tok_col(i: &LocatedSpan<&'i str>) -> Option<usize> {
        // Skip beyond any whitespace, then get the column.
        let (i, _) = multispace0::<LocatedSpan<&'i str>, ()>(*i).ok()?;
        Some(i.get_column())
    }
}

type Input = I9nInput<LocatedSpan<&'static str>, SkipWsTf>;
type Err = Bad;
type Res<O> = IResult<Input, O, Err>;

impl From<I9nError<LocatedSpan<&'static str>>> for Bad {
    fn from(e: I9nError<LocatedSpan<&'static str>>) -> Self {
        Bad(format!("{:?}, {:?}", e.situation, e.ctx))
    }
}

impl ParseError<Input> for Bad {
    fn from_error_kind(input: Input, kind: nom::error::ErrorKind) -> Self {
        Bad(format!("input={:?}, kind={kind:?}", input.slice(0..6)))
    }

    fn append(input: Input, kind: nom::error::ErrorKind, other: Self) -> Self {
        Bad(format!(
            "input={:?}, kind={kind:?}, other={}",
            input.slice(0..6),
            other.0
        ))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Stmt {
    Stmt(&'static str),
    Block(&'static str, Vec<Stmt>),
}

fn stmt(i: Input) -> Res<Stmt> {
    alt((
        single_stmt.map(Stmt::Stmt),
        while_stmt.map(|(head, body)| Stmt::Block(head, body)),
    ))(i)
}

fn single_stmt(i: Input) -> Res<&'static str> {
    let p = preceded(tuple((multispace0, begin_line)), tok(tag("stmt")));
    let mut p = p.map(|i: Input| i.fragment().clone());
    p.parse(i)
}

fn while_stmt(i: Input) -> Res<(&'static str, Vec<Stmt>)> {
    let (i, _) = tuple((
        multispace0,
        begin_line,
        tok(tag("while")),
        multispace1,
        tok(tag("True")),
        multispace0,
        tok(tag(":")),
    ))
    .parse(i)?;

    let (i, (_, stmts, _)) = tuple((
        begin_block,
        many1(preceded(tuple((multispace1, begin_line)), stmt)),
        end_block,
    ))
    .parse(i)?;

    Ok((i, ("while", stmts)))
}

#[test]
fn python_while_stmt() {
    let src = "\
while True:
    stmt
    while True:
        stmt
        while True:
            stmt
            stmt
    stmt
";
    let src = LocatedSpan::from(src);
    let (i, stmts) = stmt(src.into()).unwrap();
    let (i, _) = multispace0::<_, ()>(i).unwrap();

    assert!(i.is_empty(), "i = {i:?}");

    let expected = Stmt::Block(
        "while",
        vec![
            Stmt::Stmt("stmt"),
            Stmt::Block(
                "while",
                vec![
                    Stmt::Stmt("stmt"),
                    Stmt::Block("while", vec![Stmt::Stmt("stmt"), Stmt::Stmt("stmt")]),
                ],
            ),
            Stmt::Stmt("stmt"),
        ],
    );

    assert_eq!(stmts, expected);
}
