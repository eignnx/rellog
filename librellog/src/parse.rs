use std::iter;

use char_list::CharList;
use nom::{
    branch::alt,
    combinator::{all_consuming, cut, opt},
    error::{context, ParseError},
    error::{ContextError, ErrorKind},
    multi::{many0, many1, separated_list1},
    sequence::{preceded, tuple},
    Finish, IResult, Parser,
};
use nom_i9n::{I9nError, I9nInput, TokenizedInput};
use rpds::Vector;

use crate::{
    ast::{Clause, Item, Module, RcTm, Rel, Tm},
    data_structures::{Map, Num, Sym, Var},
    lex::tok::{
        At,
        Tok::{self, *},
    },
};

type Res<'ts, T> = IResult<Toks<'ts>, T, Error<'ts>>;
type BaseInput<'ts> = &'ts [At<Tok>];
type TokenFinder<'ts> = TokenizedInput<BaseInput<'ts>, At<Tok>>;
type Toks<'ts> = I9nInput<BaseInput<'ts>, TokenFinder<'ts>>;

/// TODO: Replace with Miette error type!
#[derive(Debug)]
pub struct Error<'ts> {
    stack: Vec<(BaseInput<'ts>, Problem<'ts>)>,
}

impl<'ts> Error<'ts> {
    fn with_message(ts: Toks, message: String) -> Error {
        Error {
            stack: vec![(ts.input(), Problem::CustomMessage(message))],
        }
    }
}

impl<'ts> ContextError<Toks<'ts>> for Error<'ts> {
    fn add_context(input: Toks<'ts>, ctx: &'static str, mut other: Self) -> Self {
        other.stack.push((input.input(), Problem::Context(ctx)));
        other
    }
}

#[derive(Debug)]
pub enum Problem<'ts> {
    /// TODO: replace with miette diagnostic or something
    CustomMessage(String),
    Context(&'static str),
    Blah(Toks<'ts>),
}

impl<'ts> ParseError<Toks<'ts>> for Error<'ts> {
    fn from_error_kind(input: Toks<'ts>, kind: ErrorKind) -> Self {
        Error {
            stack: vec![(input.input(), Problem::CustomMessage(format!("{kind:?}")))],
        }
    }

    fn append(input: Toks<'ts>, kind: ErrorKind, mut other: Self) -> Self {
        other
            .stack
            .push((input.input(), Problem::CustomMessage(format!("{kind:?}"))));
        other
    }
}

pub fn entire_module(ts: Toks) -> Result<Module, Error> {
    let (_ts, m) = all_consuming(module)(ts).finish()?;
    Ok(m)
}

pub fn entire_term(ts: Toks) -> Result<RcTm, Error> {
    let (_ts, t) = all_consuming(tm).parse(ts).finish()?;
    Ok(t.into())
}

pub fn display_parse_err(verbose_err: &Error) {
    eprintln!("Parse error:");
    let (last, init) = verbose_err.stack.split_last().unwrap();
    for (i, problem) in init {
        let loc = match i {
            [t, ..] => format!("{}:{}", t.line, t.col),
            [] => "eof".into(),
        };

        eprintln!("\t[{loc}] {problem:?} because...");
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

impl<'ts> From<I9nError<BaseInput<'ts>>> for Error<'ts> {
    fn from(e: I9nError<BaseInput<'ts>>) -> Self {
        Error {
            stack: vec![(e.input, Problem::CustomMessage(format!("{e:?}")))],
        }
    }
}

fn tok<'ts>(tgt: Tok) -> impl Parser<Toks<'ts>, At<Tok>, Error<'ts>> {
    let p = move |ts: Toks<'ts>| -> Res<'ts, At<Tok>> {
        match ts.split_first() {
            Some((t, rest)) if t.as_ref() == &tgt => Ok((rest, t.clone())),
            Some((t, _)) => {
                let e =
                    Error::with_message(ts.clone(), format!("Expected {tgt:?}, got token {t:?}"));
                Err(nom::Err::Error(e))
            }
            None => Err(eof_error(ts)),
        }
    };
    nom_i9n::tok(p)
}

fn eof_error<'ts>(ts: Toks<'ts>) -> nom::Err<Error<'ts>> {
    let e = Error::from_error_kind(ts, nom::error::ErrorKind::Eof);
    nom::Err::Error(e)
}

fn sym(ts: Toks) -> Res<Sym> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Sym(s), rest)) => Ok((rest, s.clone())),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected symbol, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn var(ts: Toks) -> Res<Var> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Var(v), rest)) => Ok((rest, v.clone())),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected variable, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn num(ts: Toks) -> Res<Num> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Num(n), rest)) => Ok((rest, n)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected number, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn txt(ts: Toks) -> Res<CharList> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Txt(s), rest)) => Ok((rest, s)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected text literal, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
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
    let (ts, _) = tok(OBrack).parse(ts)?;
    let (ts, attrs) = separated_list1(
        tok(COBrack),
        context("Expected relation attribute", cut(attr)),
    )(ts)?;
    let (ts, _) = tok(CBrack).parse(ts)?;
    let map = attrs.into_iter().collect::<Map<_, _>>();
    Ok((ts, map))
}

fn block_member(ts: Toks) -> Res<(Tok, Tm)> {
    tuple((alt((tok(Dash), tok(Pipe))).map(At::value), tm)).parse(ts)
}

fn block(ts: Toks) -> Res<Tm> {
    let (ts, pairs) = nom_i9n::indented_block(&mut nom_i9n::line(block_member)).parse(ts)?;

    let first_functor = pairs[0].0.clone();
    let members = pairs
        .into_iter()
        .map(|(functor, tm)| {
            if functor == first_functor {
                Ok(tm.into())
            } else {
                Err(nom::Err::Error(Error::with_message(
                    ts.clone(),
                    "Block functor mismatch".to_owned(),
                )))
            }
        })
        .collect::<Result<Vector<_>, nom::Err<_>>>()?;

    Ok((ts, Tm::Block(first_functor, members)))
}

fn list(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(OBrace).parse(ts)?;

    if let Ok((ts, _)) = tok(CBrace).parse(ts.clone()) {
        return Ok((ts, Tm::Nil));
    }

    // We now know there's at least one element in the list.
    let (ts, first) = tm(ts)?;

    let mut ts = ts;
    let mut xs_reversed = vec![first];

    let (ts, spread_element) = loop {
        if let Ok((ts, _)) = tok(Spread).parse(ts.clone()) {
            let (ts, spread_elemnt) = tm.parse(ts)?;
            let (ts, _) = tok(CBrace).parse(ts)?;
            break (ts, spread_elemnt);
        }

        if let Ok((ts, _)) = tok(CBrace).parse(ts.clone()) {
            break (ts, Tm::Nil);
        }

        let (new_ts, element) = tm.parse(ts)?;
        xs_reversed.push(element);
        ts = new_ts;
    };

    let xs = xs_reversed
        .into_iter()
        .rfold(spread_element, |tail, x| Tm::Cons(x.into(), tail.into()));

    Ok((ts, xs))
}

fn and_list(ts: Toks) -> Res<Tm> {
    let (ts, (first, rest)) = tuple((
        non_operator_tm,
        many1(preceded(tok(Semicolon), non_operator_tm)),
    ))
    .parse(ts)?;

    let terms = iter::once(first).chain(rest).map(RcTm::from).collect();
    Ok((ts, Tm::Block(Dash, terms)))
}

fn non_operator_tm(ts: Toks) -> Res<Tm> {
    alt((
        sym.map(Tm::Sym),
        var.map(Tm::Var),
        num.map(Tm::Num),
        txt.map(|t| Tm::Txt(t, Tm::Nil.into())),
        rel.map(Tm::Rel),
        list,
        block,
        parenthesized_tm,
    ))
    .parse(ts)
}
fn parenthesized_tm(ts: Toks) -> Res<Tm> {
    let (ts, (_, t, _)) = tuple((tok(OParen), tm, tok(CParen))).parse(ts)?;
    Ok((ts, t))
}

fn operator_tm(ts: Toks) -> Res<Tm> {
    and_list(ts)
}

pub fn tm(ts: Toks) -> Res<Tm> {
    alt((operator_tm, non_operator_tm)).parse(ts)
}

fn rel_def(ts: Toks) -> Res<Item> {
    let mut p = tuple((nom_i9n::line(rel), opt(block.map(Into::into))));
    let (ts, (r, b)) = p.parse(ts)?;
    Ok((ts, Item::RelDef(r, b)))
}

fn directive(ts: Toks) -> Res<Item> {
    let mut p = nom_i9n::line(tuple((tok(OBrack), rel, tok(CBrack))));
    let (ts, (_, r, _)) = p.parse(ts)?;
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
    use crate::{lex::tokenize, tm_displayer::TmDisplayer, utils::my_nom::Span};
    use pretty_assertions::assert_eq;
    use rpds::vector;

    #[track_caller]
    fn parse_to_tm(src: &'static str) -> Tm {
        let tokens = tokenize(Span::from(src));
        let (rest, t) = tm.parse(tokens[..].into()).unwrap();

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
    fn multi_element_list() {
        crate::init_interner();
        let src = r#"{123 "asdf" {} [aardvark][Zebra] socrates Unknown {1 2 3}}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, src);
    }

    #[test]
    fn list_with_spread() {
        crate::init_interner();
        let src = r#"{X ...Xs}"#;
        let tm = parse_to_tm(src);
        let formatted = TmDisplayer::default().with_tm(&tm).to_string();

        assert_eq!(formatted, src);
    }
}
