use nom::{
    branch::alt,
    combinator::cut,
    error::context,
    multi::{many0, many1},
    sequence::{delimited, preceded},
    Parser,
};

use crate::{
    ast::{txt::Segment, Tm},
    data_structures::Sym,
    expect_token,
    lex::tok::Tok::{self, *},
};

use super::{sym, tok, var, Error, Res, Toks};

#[derive(Debug)]
enum InterpSegment {
    /// An interpolation of a single character or variable. Does *not* include the spread operator.
    ///
    /// ```text
    /// [{… Letter …}]
    ///     ^^^^^^
    /// ```
    CharInterp(Tm),
    /// An interpolation which reprents the tail of the text template.
    ///
    /// ```text
    /// [{… ..Rest …}]
    ///     ^^^^^^
    /// ```
    TailInterp(Tm),
}

#[derive(Debug)]
enum TmplSegment {
    /// A run of literal text in a text template.
    ///
    /// ```text
    /// "abc[{H I J}]xyz[{..Rest}]"
    ///  ^^^         ^^^
    ///  ex1         ex2
    /// ```
    TxtContent(String),
    /// A list of things inside `[{` and `}]` in a text template.
    ///
    /// ```text
    /// "abc[{H I J}]xyz[{X ..Rest}]"
    ///     ^^^^^^^^^   ^^^^^^^^^^^^
    ///     ex1         ex2
    /// ```
    Interpolation(Vec<InterpSegment>),
}

fn txt_content(ts: Toks) -> Res<TmplSegment> {
    expect_token!(
        ts,
        Tok::TxtContent(s) => TmplSegment::TxtContent(s),
        t => "Expected text content, found {t:?}"
    )
}

fn single_char_sym(ts: Toks) -> Res<Sym> {
    let (new_ts, s) = sym(ts.clone())?;
    if s.is_one_char() {
        Ok((new_ts, s))
    } else {
        Err(nom::Err::Error(Error::with_message(
            ts,
            "Expected symbol to be exactly 1 character long.".to_owned(),
        )))
    }
}

/// # Examples
///
/// * `..Var` -> `TailInterp`
/// * `.."text string"` -> `TailInterp`
/// * `Var` -> `CharInterp`
/// * `a` -> `CharInterp`
fn interpolation_sequence_element(ts: Toks) -> Res<InterpSegment> {
    alt((
        preceded(
            tok(Spread),
            alt((
                (var.map(Tm::Var).map(InterpSegment::TailInterp)),
                txt.map(InterpSegment::TailInterp),
                cut(context(
                    "spread expression: Only variables or text segments may be embedded in `[{… ..Tail}]` blocks in text templates.",
                    |i| Err(nom::Err::Failure(Error::with_message(i, "Expected variable or text segment.".to_owned())))
                ))
            )),
        ),
        var.map(Tm::Var).map(InterpSegment::CharInterp),
        single_char_sym.map(Tm::Sym).map(InterpSegment::CharInterp),
    ))
    .parse(ts)
}

/// # Examples
/// * `[{H i J}]` -> `Interpolation(vec![CharInterp(H), CharInterp(i), CharInterp(J)])`
/// * `[{X ..Xs}]` -> `Interpolation(vec![CharInterp(X), TailInterp(Xs)])`
/// * `"text string"` -> `TxtContent("text string")`
fn tmpl_segment(ts: Toks) -> Res<TmplSegment> {
    alt((
        txt_content,
        delimited(
            tok(OTxtInterp),
            many1(interpolation_sequence_element).map(TmplSegment::Interpolation),
            tok(CTxtInterp),
        ),
    ))
    .parse(ts)
}

/// Checks that there's at most one segment with a `TailInterp`, and if so, that
/// it's the last one.
///
/// # Examples
///
/// * `"a[{B}]c[{..Tail}]"` -> `Ok(…)`
/// * `"abc[{..Tail}]xyz"` -> `Err("tail interp must be last")`
/// * `"abc[{..Tail1}]xyz[{..Tail2 I J}]"` -> `Err("only one tail interp allowed")`
///
/// # Walkthrough
/// ```text
/// "abc[{..Tail1}]xyz[{..Tail2 I J}]"
/// ```
/// ## Step 1: Enumerate reversed segments
///
/// ```text
/// [
///     (0, [{..Tail2 I J}]),
///     (1, "xyz"),
///     (2, [{..Tail1}]),
///     (3, "abc"),
/// ]
/// ```
///
/// ## Step 2: Enumerate reversed interpolations within segments
///
/// ```text
/// [
///     (0, [{
///         (0, J)
///         (1, I)
///         (2, ..Tail2)
///     }]),
///     (1, "xyz"),
///     (2, [{
///         (0, ..Tail1)
///     }]),
///     (3, "abc"),
/// ]
/// ```
///
/// ## Step 3: Keep indices of tail interps
/// * Found tail interp at index `(0, 2)`
/// * Found another tail interp at index `(2, 0)`
fn validate_interps<'ts>(ts: Toks<'ts>, segments: &[TmplSegment]) -> Res<'ts, Tm> {
    let mut tail_interp_indices = vec![];

    for (i, seg) in segments.iter().rev().enumerate() {
        if let TmplSegment::Interpolation(interps) = seg {
            for (j, interp) in interps.iter().rev().enumerate() {
                if let InterpSegment::TailInterp(_) = interp {
                    tail_interp_indices.push((i, j));
                }
            }
        }
    }

    match &tail_interp_indices[..] {
        // No tail interps found. No checking needed.
        [] => Ok((ts, Tm::Nil)),
        // Found exactly one tail interp at the end of the text template.
        [(0, 0)] => {
            // Retrieve that tail interpolation segment
            let Some(TmplSegment::Interpolation(last_interp)) = segments.last() else {
                unreachable!();
            };
            let Some(InterpSegment::TailInterp(tm)) = last_interp.last() else {
                unreachable!();
            };
            Ok((ts, tm.clone()))
        }
        [_] => Err(nom::Err::Failure(Error::with_message(
            ts,
            "The tail interpolation segment (`[{..Tail}]`) must be the last \
                segment in a text template."
                .to_owned(),
        ))),
        // Found more than one tail interp, that's an error.
        [_, _, ..] => Err(nom::Err::Failure(Error::with_message(
            ts,
            "Only one tail interpolation (`[{..Tail}]`) segment is allowed \
                in a text template."
                .to_owned(),
        ))),
    }
}

fn inner_txt_tmpl(ts: Toks) -> Res<Tm> {
    let (ts, segments): (_, Vec<TmplSegment>) = many0(tmpl_segment).parse(ts)?;
    let (_, tail) = validate_interps(ts.clone(), &segments[..])?;

    let mut tmpl = tail;

    for seg in segments.into_iter().rev() {
        match seg {
            TmplSegment::TxtContent(s) => {
                tmpl = Tm::TxtSeg(Segment::from_string_and_tail(s, tmpl.into()))
            }
            TmplSegment::Interpolation(interps) => {
                for interp in interps.into_iter().rev() {
                    match interp {
                        InterpSegment::CharInterp(tm) => tmpl = Tm::TxtCons(tm.into(), tmpl.into()),
                        InterpSegment::TailInterp(tm) => tmpl = tm,
                    }
                }
            }
        }
    }

    Ok((ts, tmpl))
}

fn quoted_txt_tmpl(ts: Toks) -> Res<Tm> {
    delimited(tok(OQuote), inner_txt_tmpl, tok(CQuote)).parse(ts)
}

fn triple_quoted_txt_tmpl(ts: Toks) -> Res<Tm> {
    delimited(tok(OTripleQuote), inner_txt_tmpl, tok(CTripleQuote)).parse(ts)
}

pub fn txt(ts: Toks) -> Res<Tm> {
    alt((triple_quoted_txt_tmpl, quoted_txt_tmpl)).parse(ts)
}
