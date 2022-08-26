use nom::{
    branch::alt,
    combinator::{cut, opt},
    error::{context, ParseError, VerboseErrorKind},
    error::{ErrorKind, VerboseError},
    multi::{many0, many1, separated_list1},
    sequence::tuple,
    IResult, Parser,
};

use crate::{
    ast::{
        Item, Module, Rel,
        Tm::{self},
    },
    data_structures::{Map, Num, Sym, Var},
    tok::Tok::{self, *},
};

type Toks<'ts> = &'ts [Tok];
type Res<'ts, T> = IResult<Toks<'ts>, T, VerboseError<Toks<'ts>>>;

fn verbose_error<'ts>(ts: Toks<'ts>, kind: ErrorKind) -> nom::Err<VerboseError<Toks<'ts>>> {
    nom::Err::Error(VerboseError::from_error_kind(ts, kind))
}

fn tok<'ts>(tgt: Tok) -> impl Fn(Toks<'ts>) -> Res<'ts, Tok> {
    move |ts| match ts.split_first() {
        Some((t, rest)) if t == &tgt => Ok((rest, t.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn sym(ts: Toks) -> Res<Sym> {
    match ts.split_first() {
        Some((Sym(s), rest)) => Ok((rest, s.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn var(ts: Toks) -> Res<Var> {
    match ts.split_first() {
        Some((Var(v), rest)) => Ok((rest, v.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn num(ts: Toks) -> Res<Num> {
    match ts.split_first() {
        Some((Num(i), rest)) => Ok((rest, *i)),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn txt(ts: Toks) -> Res<String> {
    match ts.split_first() {
        Some((Txt(s), rest)) => Ok((rest, s.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn attr(ts: Toks) -> Res<(Sym, Tm)> {
    alt((
        // [name Value]
        tuple((sym, tm)),
        // [AttrVarSameName]
        var.map(|v| (v.to_lowercase(), Tm::Var(v))),
        // [attr_sym_same_name]
        sym.map(|s| (s.clone(), Tm::Sym(s))),
    ))(ts)
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
    tuple((alt((tok(Dash), tok(Pipe))), tm))(ts)
}

fn block(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(Indent)(ts)?;
    let (ts, pairs) = many1(block_member)(ts)?;

    let first_functor = pairs[0].0.clone();
    let members = pairs
        .into_iter()
        .map(|(functor, tm)| {
            if &functor == &first_functor {
                Ok(tm)
            } else {
                Err(nom::Err::Error(VerboseError {
                    errors: vec![(ts, VerboseErrorKind::Context("Block functor mismatch"))],
                }))
            }
        })
        .collect::<Result<Vec<Tm>, nom::Err<_>>>()?;

    let (ts, _) = cut(tok(Dedent))(ts)?;
    Ok((ts, Tm::Block(first_functor, members)))
}

pub fn tm(ts: Toks) -> Res<Tm> {
    alt((
        sym.map(Tm::Sym),
        var.map(Tm::Var),
        num.map(Tm::Num),
        txt.map(Tm::Txt),
        block,
        rel.map(Tm::Rel),
    ))(ts)
}

fn rel_def(ts: Toks) -> Res<Item> {
    let (ts, (r, b)) = tuple((rel, opt(block)))(ts)?;
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
    many0(item).map(Module).parse(ts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex::tokenize, my_nom::Span};
    use pretty_assertions::assert_eq;

    #[test]
    fn simple_nested_relation_tm() {
        let src = r#"[goal [List][Pred]][initial Sublist][final empty_list]"#;
        let tokens = tokenize(Span::from(src));
        let (rest, actual) = tm(&tokens).unwrap();

        let expected = Tm::Rel(
            vec![
                (
                    "goal".into(),
                    Tm::Rel(
                        vec![
                            ("list".into(), Tm::Var("List".into())),
                            ("pred".into(), Tm::Var("Pred".into())),
                        ]
                        .into_iter()
                        .collect(),
                    ),
                ),
                ("initial".into(), Tm::Var("Sublist".into())),
                ("final".into(), Tm::Sym("empty_list".into())),
            ]
            .into_iter()
            .collect(),
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
        assert!(
            rest.is_empty(),
            "expected empty remainder, got: {:?}, input was {:?}",
            rest,
            src
        );
    }

    #[test]
    fn simple_block() {
        let src = "
    - [Blah]
    - [Blah]
    - [Blah]\n";

        let tokens = tokenize(Span::from(src));
        let (rest, actual) = tm(&tokens).unwrap();

        let blah = Tm::Rel(
            vec![("blah".into(), Tm::Var("Blah".into()))]
                .into_iter()
                .collect(),
        );

        let expected = Tm::Block(Dash, vec![blah.clone(), blah.clone(), blah.clone()]);

        assert_eq!(actual, expected, "Input was {:?}", src);
        assert!(
            rest.is_empty(),
            "expected empty remainder, got: {:?}, input was {:?}",
            rest,
            src
        );
    }

    #[test]
    fn nested_block() {
        let src = "
    - [Blah]
    -
        | [Blah]
        | [Blah]
    - [Blah]\n";

        let tokens = tokenize(Span::from(src));
        let (rest, actual) = tm(&tokens).unwrap();

        let blah = Tm::Rel(
            vec![("blah".into(), Tm::Var("Blah".into()))]
                .into_iter()
                .collect(),
        );

        let expected = Tm::Block(
            Dash,
            vec![
                blah.clone(),
                Tm::Block(Pipe, vec![blah.clone(), blah.clone()]),
                blah.clone(),
            ],
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
        assert!(
            rest.is_empty(),
            "expected empty remainder, got: {:?}, input was {:?}",
            rest,
            src
        );
    }
}
