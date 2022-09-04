use nom::{
    branch::alt,
    combinator::{all_consuming, cut, opt},
    error::{context, ParseError, VerboseErrorKind},
    error::{ErrorKind, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::{preceded, tuple},
    Finish, IResult, Parser,
};
use rpds::Vector;

use crate::{
    ast::{Clause, Item, Module, RcTm, Rel, Tm},
    data_structures::{Map, Num, Sym, Var},
    tok::{
        At,
        Tok::{self, *},
    },
};

type Res<'ts, T> = IResult<Toks<'ts>, T, Error<'ts>>;
pub type Error<'ts> = VerboseError<Toks<'ts>>;
type Toks<'ts> = &'ts [At<Tok>];

#[derive(Debug)]
pub struct AllocError(VerboseError<Vec<At<Tok>>>);

impl<'ts> From<Error<'ts>> for AllocError {
    fn from(e: Error<'ts>) -> Self {
        let errors: Vec<_> = e
            .errors
            .into_iter()
            .map(|(i, e)| (i.to_owned(), e))
            .collect();
        let ve = VerboseError { errors };
        Self(ve)
    }
}

pub fn entire_module(ts: Toks) -> Result<Module, Error> {
    let (_ts, m) = all_consuming(module)(ts).finish()?;
    Ok(m)
}

pub fn entire_term(ts: Toks) -> Result<RcTm, Error> {
    let (_ts, t) = all_consuming(tm)(ts).finish()?;
    Ok(t.into())
}

pub fn display_parse_err(verbose_err: &Error) {
    eprintln!("Parse error:");
    let (last, init) = verbose_err.errors.split_last().unwrap();
    for (i, ekind) in init {
        let loc = match i {
            [t, ..] => format!("{}:{}", t.line, t.col),
            [] => "eof".into(),
        };

        eprintln!("\t[{loc}] Parser {ekind:?} failed because...");
    }
    let (i, last) = last;
    eprintln!(
        "\t...{last:?}, got {}.",
        i.first().map_or_else(
            || "end of input".into(),
            |tok| format!("token `{}` at [{}:{}]", tok.value, tok.line, tok.col)
        )
    );
}

fn verbose_error(ts: Toks, kind: ErrorKind) -> nom::Err<VerboseError<Toks>> {
    nom::Err::Error(VerboseError::from_error_kind(ts, kind))
}

fn tok<'ts>(tgt: Tok) -> impl Fn(Toks<'ts>) -> Res<'ts, At<Tok>> {
    move |ts| match ts.split_first() {
        Some((t, rest)) if t.as_ref() == &tgt => Ok((rest, t.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn sym(ts: Toks) -> Res<Sym> {
    match ts.split_first().map(|(x, xs)| (x.as_ref(), xs)) {
        Some((Tok::Sym(s), rest)) => Ok((rest, s.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn var(ts: Toks) -> Res<Var> {
    match ts.split_first().map(|(x, xs)| (x.as_ref(), xs)) {
        Some((Var(v), rest)) => Ok((rest, v.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn num(ts: Toks) -> Res<Num> {
    match ts.split_first().map(|(x, xs)| (x.as_ref(), xs)) {
        Some((Num(i), rest)) => Ok((rest, *i)),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn txt(ts: Toks) -> Res<String> {
    match ts.split_first().map(|(x, xs)| (x.as_ref(), xs)) {
        Some((Txt(s), rest)) => Ok((rest, s.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn attr(ts: Toks) -> Res<(Sym, RcTm)> {
    alt((
        // [name Value]
        tuple((sym, tm)),
        // [AttrVarSameName]
        var.map(|v| {
            // `lower` created on separated line so INTERNER isn't shared AND mutated.
            let lower = v.to_str().to_lowercase();
            (lower.into(), Tm::Var(v))
        }),
        // [attr_sym_same_name]
        sym.map(|s| (s, Tm::Sym(s))),
    ))
    .map(|(sym, tm)| (sym, tm.into()))
    .parse(ts)
}

fn rel(ts: Toks) -> Res<Rel> {
    let (ts, _) = tok(OBrack)(ts)?;
    let (ts, attrs) = separated_list1(
        tok(COBrack),
        context("Expected relation attribute", cut(attr)),
    )(ts)?;
    let (ts, _) = tok(CBrack)(ts)?;
    let map = attrs.into_iter().collect::<Map<_, _>>();
    Ok((ts, map))
}

fn block_member(ts: Toks) -> Res<(Tok, Tm)> {
    tuple((alt((tok(Dash), tok(Pipe))).map(At::value), tm))(ts)
}

fn block(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(Indent)(ts)?;
    let (ts, pairs) = many1(block_member)(ts)?;

    let first_functor = pairs[0].0.clone();
    let members = pairs
        .into_iter()
        .map(|(functor, tm)| {
            if functor == first_functor {
                Ok(tm.into())
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(ts, VerboseErrorKind::Context("Block functor mismatch"))],
                }))
            }
        })
        .collect::<Result<Vector<_>, nom::Err<_>>>()?;

    let (ts, _) = cut(tok(Dedent))(ts)?;
    Ok((ts, Tm::Block(first_functor, members)))
}

fn list(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(OBrace)(ts)?;

    if let Ok((ts, _)) = tok(CBrace)(ts) {
        return Ok((ts, Tm::Nil));
    }

    // We now know there's at least one element in the list.
    let (ts, first) = tm(ts)?;

    let mut ts = ts;
    let mut xs_reversed = vec![first];

    let spread_element = loop {
        use ListSegment::*;
        enum ListSegment {
            CommaTm(Tm),
            SpreadTmEnd(Tm),
            CloseBrace,
        }

        let spread_tm_end =
            tuple((tok(Spread), tm, opt(tok(Comma)), tok(CBrace))).map(|(_, tm, _, _)| tm);

        let mut list_segment = alt((
            preceded(tok(Comma), tm).map(CommaTm), // ts = ", tm"
            preceded(
                opt(tok(Comma)),
                alt((
                    tok(CBrace).map(|_| CloseBrace), // ts = ", }"
                    spread_tm_end.map(SpreadTmEnd),  // ts = ", ...tm, }"
                )),
            ),
        ));

        // Parse the list segment.
        let (new_ts, x) = list_segment(ts)?;

        ts = new_ts;

        match x {
            CommaTm(tm) => xs_reversed.push(tm),
            SpreadTmEnd(tm) => break tm,
            CloseBrace => break Tm::Nil,
        }
    };

    let xs = xs_reversed
        .into_iter()
        .rfold(spread_element, |tail, x| Tm::Cons(x.into(), tail.into()));

    Ok((ts, xs))
}

pub fn tm(ts: Toks) -> Res<Tm> {
    alt((
        sym.map(Tm::Sym),
        var.map(Tm::Var),
        num.map(Tm::Num),
        txt.map(Tm::Txt),
        block,
        rel.map(Tm::Rel),
        list,
    ))(ts)
}

fn rel_def(ts: Toks) -> Res<Item> {
    let (ts, (r, b)) = tuple((rel, opt(block.map(Into::into))))(ts)?;
    Ok((ts, Item::RelDef(r, b)))
}

fn directive(ts: Toks) -> Res<Item> {
    let (ts, (_, r, _)) = tuple((tok(OBrack), rel, tok(CBrack)))(ts)?;
    Ok((ts, Item::Directive(r)))
}

pub fn item(ts: Toks) -> Res<Item> {
    alt((directive, rel_def))(ts)
}

pub fn module(ts: Toks) -> Res<Module> {
    many0(item)
        .map(|items| {
            let mut m = Module::default();
            for item in items {
                match item {
                    Item::Directive(rel) => m.directives.push(rel),
                    Item::RelDef(head, body) => {
                        m.relations
                            .entry(head.clone().into())
                            .or_default()
                            .push(Clause { head, body });
                    }
                }
            }
            m
        })
        .parse(ts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex::tokenize, my_nom::Span, tm_displayer::TmDisplayer};
    use pretty_assertions::assert_eq;
    use rpds::vector;

    fn parse_to_tm(src: &'static str) -> Tm {
        let tokens = tokenize(Span::from(src));
        let (rest, t) = tm(&tokens).unwrap();

        assert!(
            rest.is_empty(),
            "\nCould not parse entire input: ```\n{src}\n```\n\
             Remaining input begins with: [{}]\n",
            rest.into_iter()
                .take(5)
                .map(|tok| format!("`{}`", tok.as_ref()))
                .collect::<Vec<_>>()
                .join(", "),
        );

        t
    }

    #[test]
    fn simple_nested_relation_tm() {
        crate::init_interner();
        let src = r#"[goal [List][Pred]][initial Sublist][final empty_list]"#;
        let actual = parse_to_tm(src);

        let expected = Tm::Rel(
            vec![
                (
                    "goal".into(),
                    Tm::Rel(
                        vec![
                            ("list".into(), Tm::Var("List".into()).into()),
                            ("pred".into(), Tm::Var("Pred".into()).into()),
                        ]
                        .into_iter()
                        .collect(),
                    )
                    .into(),
                ),
                ("initial".into(), Tm::Var("Sublist".into()).into()),
                ("final".into(), Tm::Sym("empty_list".into()).into()),
            ]
            .into_iter()
            .collect(),
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn simple_block() {
        crate::init_interner();
        let src = "
    - [Blah]
    - [Blah]
    - [Blah]\n";

        let actual = parse_to_tm(src);

        let blah: RcTm = Tm::Rel(
            vec![("blah".into(), Tm::Var("Blah".into()).into())]
                .into_iter()
                .collect(),
        )
        .into();

        let expected = Tm::Block(Dash, vector![blah.clone(), blah.clone(), blah.clone()]);

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn nested_block() {
        crate::init_interner();
        let src = "
    - [Blah]
    -
        | [Blah]
        | [Blah]
    - [Blah]\n";

        let actual = parse_to_tm(src);

        let blah: RcTm = Tm::Rel(
            vec![("blah".into(), Tm::Var("Blah".into()).into())]
                .into_iter()
                .collect(),
        )
        .into();

        let expected = Tm::Block(
            Dash,
            vector![
                blah.clone(),
                Tm::Block(Pipe, vector![blah.clone(), blah.clone()]).into(),
                blah.clone()
            ],
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn empty_list() {
        crate::init_interner();
        assert_eq!(parse_to_tm("{}"), Tm::Nil);
    }

    #[test]
    fn singleton_list() {
        crate::init_interner();
        assert_eq!(
            parse_to_tm("{123}"),
            Tm::Cons(Tm::Num(123).into(), Tm::Nil.into())
        );
    }

    #[test]
    fn singleton_list_trailing_comma() {
        crate::init_interner();
        assert_eq!(
            parse_to_tm("{123,}"),
            Tm::Cons(Tm::Num(123).into(), Tm::Nil.into())
        );
    }

    #[test]
    fn multi_element_list() {
        crate::init_interner();
        let src = r#"{123, "asdf", {}, [aardvark][Zebra], socrates, Unknown, {1, 2, 3}}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, src);
    }

    #[test]
    fn list_with_spread() {
        crate::init_interner();
        let src = r#"{X ...Xs}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, src);
    }

    #[test]
    fn list_with_spread_and_1_comma() {
        crate::init_interner();
        let src = r#"{X, ...Xs}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, "{X ...Xs}");
    }

    #[test]
    fn list_with_spread_and_2_commas() {
        crate::init_interner();
        let src = r#"{X, ...Xs,}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, "{X ...Xs}");
    }
}
