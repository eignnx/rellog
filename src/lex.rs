use std::cmp::Ordering;

use crate::{
    data_structures::Sym,
    my_nom::{Res, Span},
    tok::{At, Tok},
};
use nom::{
    bytes::complete::{tag, take_while},
    combinator::opt,
    error::{make_error, ErrorKind},
    multi::many0,
    Parser,
};

fn parse_block_line<'state, 'i: 'state>(
    prev_indent: &'state mut usize,
    out: &'state mut impl Extend<At<Tok>>,
) -> impl FnMut(Span<'i>) -> Res<'i, ()> + 'state {
    move |i: Span<'i>| {
        if i.is_empty() {
            return Err(nom::Err::Error(make_error(i, ErrorKind::NonEmpty)));
        }

        let (i, indent) = take_while(|c| c == ' ')
            .map(|ws: Span<'i>| ws.len())
            .parse(i)?;

        let (i, line) = take_while(|c| c != '\n')(i)?;

        if !line.is_empty() {
            match indent.cmp(prev_indent) {
                Ordering::Greater => out.extend([Tok::Indent.at(i)]),
                Ordering::Less => out.extend([Tok::Dedent.at(i)]),
                _ => {}
            }

            *prev_indent = indent;
            out.extend([Tok::Sym(line.to_string()).at(i)]);
        }

        let (i, _) = opt(tag("\n"))(i)?;

        Ok((i, ()))
    }
}

fn parse_block<'i>(i: Span<'i>) -> Res<'i, Vec<At<Tok>>> {
    let mut out = vec![];
    let mut prev_indent = 0usize;
    let (i, _) = many0(parse_block_line(&mut prev_indent, &mut out))(i)?;
    Ok((i, out))
}

#[cfg(test)]
mod block_tests {
    use super::parse_block;
    use super::At;
    use super::Tok::*;

    #[test]
    fn test_parse_block() {
        let src = r"
def foo():
    bar()
    while True:
           
        blah()

    return 3
  
x = 2
        "
        .trim();

        let (rest, parsed) = parse_block(src.into()).unwrap();

        assert_eq!(rest.to_string(), "".to_string());

        let actual = parsed.into_iter().map(At::without_loc).collect::<Vec<_>>();
        let expected = vec![
            Sym("def foo():".into()),
            Indent,
            Sym("bar()".into()),
            Sym("while True:".into()),
            Indent,
            Sym("blah()".into()),
            Dedent,
            Sym("return 3".into()),
            Dedent,
            Sym("x = 2".into()),
        ];

        assert_eq!(actual, expected);
    }
}
