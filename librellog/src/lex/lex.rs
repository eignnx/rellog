use crate::{
    interner::IStr,
    lex::tok::{At, MakeAt, Tok},
    utils::my_nom::{Res, Span},
};
use char_list::CharList;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_until, take_while},
    character::complete::{anychar, i64, multispace0},
    combinator::{recognize, verify},
    multi::many0,
    sequence::tuple,
    Parser,
};
use nom_i9n::{I9nInput, TokenizedInput};

fn text_literal<'i>(i: Span<'i>) -> Res<'i, CharList> {
    alt((
        move |i: Span<'i>| {
            let start_col = i.get_column() - 1;
            let (i, _) = tag(r#"""""#)(i)?;
            let (i, text) = take_until(r#"""""#)(i)?;
            let (i, _) = tag(r#"""""#)(i)?;

            let mut out = String::new();

            if text.starts_with("\n") || text.starts_with("\r\n") {
                // _ _ _ """
                // _ _ _ _ _ asdf
                // _ _ _ """
                // should produce
                // """
                // _ _ asdf
                // """
                for line in text.lines().skip(1) {
                    out.push_str(&line[start_col..]);
                }

                if out.ends_with("\n") {
                    out.pop();
                }
            } else {
                // _ _ _ """adsf
                // _ _ _ _ _ _ asdf
                // _ _ _ """
                // should produce
                // "adsf\n _ asdf"
                out.push_str(text.lines().next().unwrap_or_default());
                out.push('\n');
                let start_col = start_col + 3;
                for line in text.lines().skip(1) {
                    out.push_str(&line[start_col..]);
                    out.push('\n');
                }
                out.pop();
            }

            Ok((i, CharList::from(out)))
        },
        move |i: Span<'i>| {
            let (i, _) = tag("\"")(i)?;
            let (i, text) = take_while(|c: char| c != '"')(i)?;
            let (i, _) = tag("\"")(i)?;
            Ok((i, CharList::from(*text.fragment())))
        },
    ))
    .parse(i)
}

fn any_symbol(i: Span) -> Res<IStr> {
    recognize(tuple((
        verify(anychar::<Span, _>, |&c| c.is_alphabetic() || c == '_'),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    )))
    .map(|span| span.fragment().into())
    .parse(i)
}

fn var_or_sym(i: Span) -> Res<Tok> {
    any_symbol
        .map(|sym| {
            if sym.to_str().starts_with(char::is_uppercase) {
                Tok::Var(sym.into())
            } else if sym.to_str().starts_with(char::is_lowercase) {
                Tok::Sym(sym)
            } else {
                unreachable!()
            }
        })
        .parse(i)
}

fn one_token(i: Span) -> Res<At<Tok>> {
    let (i, _) = multispace0(i)?;
    alt((
        i64.map(Tok::Num),
        text_literal.map(Tok::Txt),
        var_or_sym,
        tag("][").map(|_| Tok::COBrack),
        tag("[").map(|_| Tok::OBrack),
        tag("]").map(|_| Tok::CBrack),
        tag("{").map(|_| Tok::OBrace),
        tag("}").map(|_| Tok::CBrace),
        tag("(").map(|_| Tok::OParen),
        tag(")").map(|_| Tok::CParen),
        tag("-").map(|_| Tok::Dash),
        tag("|").map(|_| Tok::Pipe),
        tag(",").map(|_| Tok::Comma),
        tag(";").map(|_| Tok::Semicolon),
        tag("..").map(|_| Tok::Spread),
        anychar.map(move |_| {
            panic!(
                "[{}:{}] Unknown symbol encountered: {:?}",
                i.location_line(),
                i.get_column(),
                i.chars().take(5).collect::<String>(),
            );
        }),
    ))
    .map(|t| t.at(i))
    .parse(i)
}

pub fn tokenize<'i>(src: impl Into<Span<'i>> + 'i) -> Vec<At<Tok>> {
    let src: Span = src.into();
    let (_i, tokens) = many0(one_token).parse(src).unwrap();
    tokens
}

pub fn tokenize_into<'i, 'buf>(
    buf: &'buf mut Vec<At<Tok>>,
    src: impl Into<Span<'i>> + 'i,
) -> I9nInput<&'buf [At<Tok>], TokenizedInput<&'buf [At<Tok>], At<Tok>>> {
    *buf = tokenize(src);
    I9nInput::from(&**buf)
}

#[cfg(test)]
mod block_tests {
    use super::tokenize;
    use super::Tok::*;
    use crate::tok::At;
    use pretty_assertions::assert_eq;

    #[test]
    fn tokenize_basic_relation_def() {
        crate::init_interner();
        let src = r"


[A]
    - [blah]
    - [florp]
    - [glop]
";

        let actual = tokenize(src).into_iter().map(At::value).collect::<Vec<_>>();

        let expected = vec![
            OBrack,
            Var("A".into()),
            CBrack,
            Dash,
            OBrack,
            Sym("blah".into()),
            CBrack,
            Dash,
            OBrack,
            Sym("florp".into()),
            CBrack,
            Dash,
            OBrack,
            Sym("glop".into()),
            CBrack,
        ];

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn tokenize_relation_term() {
        let src = r#"[asdf Qwerty][Poiu]"#;

        let actual = tokenize(src).into_iter().map(At::value).collect::<Vec<_>>();

        let expected = vec![
            OBrack,
            Sym("asdf".into()),
            Var("Qwerty".into()),
            COBrack,
            Var("Poiu".into()),
            CBrack,
        ];

        assert_eq!(actual, expected, "Input was {:?}", src);
    }
}
