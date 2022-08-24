use nom::{
    branch::alt,
    combinator::cut,
    error::{context, ParseError},
    error::{ErrorKind, VerboseError},
    multi::separated_list1,
    sequence::tuple,
    IResult, Parser,
};

use crate::{
    ast::Tm::{self},
    data_structures::{Map, Sym, Var},
    tok::Tok::{self, *},
};

type Toks<'ts> = &'ts [Tok];
type Res<'ts, T> = IResult<Toks<'ts>, T, VerboseError<Toks<'ts>>>;

fn verbose_error<'ts>(ts: Toks<'ts>, kind: ErrorKind) -> nom::Err<VerboseError<Toks<'ts>>> {
    nom::Err::Error(VerboseError::from_error_kind(ts, kind))
}

fn tag<'tgt, 'ts>(tgt: Toks<'tgt>) -> impl Fn(Toks<'ts>) -> Res<Toks<'ts>> + 'tgt {
    move |ts| {
        if tgt.len() > ts.len() {
            return Err(verbose_error(ts, ErrorKind::Tag));
        }

        let (prefix, rest) = ts.split_at(tgt.len());

        if prefix == tgt {
            Ok((rest, prefix))
        } else {
            Err(verbose_error(ts, ErrorKind::Tag))
        }
    }
}

fn tok<'ts>(tgt: Tok) -> impl Fn(Toks<'ts>) -> Res<'ts, Tok> {
    move |ts| match ts.split_first() {
        Some((t, rest)) if t == &tgt => Ok((rest, t.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn var(ts: Toks) -> Res<Var> {
    match ts.split_first() {
        Some((Var(v), rest)) => Ok((rest, v.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn sym(ts: Toks) -> Res<Sym> {
    match ts.split_first() {
        Some((Sym(s), rest)) => Ok((rest, s.clone())),
        _ => Err(verbose_error(ts, ErrorKind::Char)),
    }
}

fn attr(ts: Toks) -> Res<(Sym, Tm)> {
    alt((
        tuple((sym, tm)),
        var.map(|v| (v.to_lowercase(), Tm::Var(v))),
    ))(ts)
}

fn rel(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(OBrack)(ts)?;
    let (ts, attrs) = separated_list1(
        tok(COBrack),
        context("Expected relation attribute", cut(attr)),
    )(ts)?;
    let (ts, _) = tok(CBrack)(ts)?;
    let map = attrs.into_iter().collect::<Map<_, _>>();
    Ok((ts, Tm::Rel(map)))
}

pub fn tm(ts: Toks) -> Res<Tm> {
    alt((rel, sym.map(Tm::Sym), var.map(Tm::Var)))(ts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lex::tokenize, my_nom::Span};
    use pretty_assertions::assert_eq;

    #[test]
    fn test1() {
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
}
