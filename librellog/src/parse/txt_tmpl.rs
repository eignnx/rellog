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

enum TmplSegment {
    /// A run of literal text in a text template.
    ///
    /// ```text
    /// "abc[{H I J}]xyz[{..Rest}]"
    ///  ^^^         ^^^
    ///  ex1         ex2
    /// ```
    TxtContent(String),
    /// An interpolation of a single character or variable. Does *not* include the spread operator.
    ///
    /// ```text
    /// "abc[{Letter}]xyz[{..Rest}]"
    ///     ^^^^^^^^^^
    /// ```
    ///
    /// ```text
    /// "abc[{D E F}]g" == "abc[{D}][{E}][{F}]g"
    /// ```
    CharInterp(Tm),
    /// An interpolation which reprents the tail of the text template.
    ///
    /// ```text
    /// "abc[{..Rest}]"
    ///       ^^^^^^^
    /// ```
    ///
    /// ```text
    /// "abc[{D E F ..Rest}]" == "abc[{D}][{E}][{F}][{..Rest}]"
    ///             ^^^^^^                          ^^^^^^^^^^
    /// ```
    TailInterp(Tm),
}

fn txt_content(ts: Toks) -> Res<TmplSegment> {
    expect_token!(
        ts,
        Tok::TxtContent(s) => TmplSegment::TxtContent(s),
        t => "Expected text content, found {t:?}"
    )
}

fn single_char_sym(ts: Toks) -> Res<Sym> {
    let (ts, s) = sym(ts)?;
    if s.is_one_char() {
        Ok((ts, s))
    } else {
        Err(nom::Err::Error(Error::with_message(
            ts,
            "Expected symbol to be exactly 1 character long.".to_owned(),
        )))
    }
}

fn char_in_list_of_chars_interp(ts: Toks) -> Res<TmplSegment> {
    alt((
        preceded(
            tok(Spread),
            alt((
                (var.map(Tm::Var).map(TmplSegment::TailInterp)),
                txt.map(TmplSegment::TailInterp),
                cut(context(
                    "spread expression: Only variables or text segments may be embedded in `[{… ..Tail}]` blocks in text templates.",
                    |i| Err(nom::Err::Failure(Error::with_message(i, "Expected variable or text segment.".to_owned())))
                ))
            )),
        ),
        var.map(Tm::Var).map(TmplSegment::CharInterp),
        single_char_sym.map(Tm::Sym).map(TmplSegment::CharInterp),
    ))
    .parse(ts)
}

// fn list_of_chars_interp(ts: Toks) -> Res<Vec<TmplSegment>> {
//     many0(char_in_list_of_chars_interp).parse(ts)
// }

fn inner_txt_tmpl(ts: Toks) -> Res<Tm> {
    let (ts, sections): (_, Vec<Vec<TmplSegment>>) = many0(alt((
        txt_content.map(|seg| vec![seg]),
        delimited(
            tok(OTxtInterp),
            many1(char_in_list_of_chars_interp),
            tok(CTxtInterp),
        ),
    )))
    .parse(ts)?;

    // Check that there's at most one TailInterp segment, and that it's the last one.
    let mut tail_interps = sections
        .iter()
        .flatten()
        .enumerate()
        .filter(|(_i, seg)| matches!(seg, TmplSegment::TailInterp(_)));

    let tail = if let Some((i, TmplSegment::TailInterp(tail_interp))) = tail_interps.next() {
        if tail_interps.next().is_some() {
            return Err(nom::Err::Failure(Error::with_message(
                ts,
                "Only one tail interpolation (`[{..Tail}]`) segment is allowed in a text template."
                    .to_owned(),
            )));
        }
        if i != sections.iter().flatten().count() - 1 {
            return Err(nom::Err::Failure(Error::with_message(
                ts,
                "The tail interpolation segment (`[{..Tail}]`) must be the last \
                segment in a text template."
                    .to_owned(),
            )));
        }
        tail_interp.clone()
    } else {
        Tm::Nil
    };

    let tmpl = sections
        .into_iter()
        .flatten()
        .filter(|seg| !matches!(seg, TmplSegment::TailInterp(_)))
        .rfold(tail, |acc, seg| match seg {
            TmplSegment::TxtContent(s) => Tm::TxtSeg(Segment::from_string_and_tail(s, acc.into())),
            TmplSegment::CharInterp(tm) => Tm::TxtCons(tm.into(), acc.into()),
            TmplSegment::TailInterp(_) => unreachable!(),
        });

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
