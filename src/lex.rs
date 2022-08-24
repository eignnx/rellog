use std::{
    borrow::{Borrow, BorrowMut},
    cell::{Cell, RefCell, RefMut},
    cmp::Ordering,
    rc::Rc,
};

use crate::{
    data_structures::Sym,
    my_nom::{PErr, Res, Span},
    tok::{At, Tok},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::multispace0,
    combinator::{opt, value},
    error::{make_error, ErrorKind},
    multi::many0,
    sequence::preceded,
    Parser,
};

fn split_indent_text<'i>(line: Span<'i>) -> Res<'i, Span<'i>> {
    take_while(char::is_whitespace)(line)
}

const INDENT_SIZE_IN_SPACES: usize = 4;

fn indent_count<'i>(indent_text: Span<'i>) -> usize {
    if indent_text.is_empty() {
        return 0;
    }

    enum IndentChar {
        Tabs,
        Spaces,
    }

    use IndentChar::*;

    let mut indent_char = None;

    for ch in indent_text.chars() {
        match (ch, &indent_char) {
            (' ', &None) => indent_char = Some(Spaces),
            ('\t', &None) => indent_char = Some(Tabs),
            (' ', &Some(Spaces)) => {}
            ('\t', &Some(Tabs)) => {}
            _ => panic!("Mixed indentation characters!"),
        }
    }

    return match indent_char {
        Some(Tabs) => indent_text.len(),
        Some(Spaces) if indent_text.len() % INDENT_SIZE_IN_SPACES == 0 => {
            indent_text.len() / INDENT_SIZE_IN_SPACES
        }
        Some(Spaces) => panic!("Indent is not a multiple of {INDENT_SIZE_IN_SPACES} spaces!"),
        _ => unreachable!(),
    };
}

enum SimpleTok<'i> {
    Text(Span<'i>),
    Indent,
    Dedent,
}

fn insert_indent_dedent_tokens<'i>(
    lines: impl Iterator<Item = (usize, Span<'i>)>,
) -> impl Iterator<Item = SimpleTok<'i>> {
    let prev = Rc::new(Cell::new(0usize));
    let fm_prev = prev.clone();

    lines
        .flat_map(move |(curr, line)| {
            let prev = fm_prev.clone();
            if curr == prev.get() {
                vec![SimpleTok::Text(line)]
            } else {
                let mut out = vec![];
                for _ in 0..(curr as isize - prev.get() as isize).abs() {
                    out.push(if curr > prev.get() {
                        SimpleTok::Indent
                    } else {
                        SimpleTok::Dedent
                    })
                }
                out.push(SimpleTok::Text(line));
                prev.replace(curr);
                out
            }
        })
        .chain((0..=prev.get()).map(|_| SimpleTok::Dedent))
}

#[derive(Debug, Clone, Copy)]
enum BlockCtx {
    Block,
    Bracketed,
}

fn tokenize_line<'st, 'i: 'st>(
    mut line: Span<'i>,
    state: &'st mut BlockCtx,
) -> impl Iterator<Item = Tok> + 'st {
    std::iter::from_fn(move || loop {
        if line.is_empty() {
            return None;
        }

        if let Ok((rem, sym)) = take_while1::<_, _, PErr<'i>>(char::is_alphanumeric)(line) {
            line = rem;
            return Some(Tok::Sym(sym.to_string()));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("[")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OBrack);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("]")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CBrack);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("{")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OBrace);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("}")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CBrace);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("-")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::Dash);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("|")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::Pipe);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>(",")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::Comma);
        }

        if let Ok((rem, _)) = take_while1::<_, _, PErr<'i>>(char::is_whitespace)(line) {
            line = rem;
            continue;
        }

        panic!("Unknown symbol encountered: {:?}", &line[0..5])
    })
}

fn lex<'i>(src: Span<'i>) -> Vec<Tok> {
    let lines = src
        .lines()
        .map(|s| s.into()) // TODO: do we lose position info?
        .filter_map(|line| split_indent_text(line).ok())
        .map(|(text, ws)| (indent_count(ws), text));

    let simple_toks = insert_indent_dedent_tokens(lines);

    let mut state = BlockCtx::Block;
    let mut tokens = vec![];

    for stok in simple_toks {
        match (stok, state.clone()) {
            (SimpleTok::Text(line), _) => tokens.extend(tokenize_line(line, &mut state)),
            (SimpleTok::Indent, BlockCtx::Block) => tokens.push(Tok::Indent),
            (SimpleTok::Dedent, BlockCtx::Block) => tokens.push(Tok::Dedent),
            _ => {}
        }
    }

    tokens
}

// value(Tok::COBrack, tag("][")),
// value(Tok::OBrack, tag("[")),
// value(Tok::CBrack, tag("]")),
// value(Tok::OBrace, tag("{")),
// value(Tok::CBrace, tag("}")),
// value(Tok::Dash, tag("-")),
// value(Tok::Pipe, tag("|")),
// value(Tok::Comma, tag(",")),
// value(Tok::Spread, tag("...")),

//////////////////////////////////////////////////////////////////////////

#[cfg(test)]
mod block_tests {
    use super::lex;

    use super::At;
    use super::Tok::*;

    #[test]
    fn test_parse_block2() {
        let src = r"
[A] if
    - [blah]
    - [florp]
    - [glop]
        "
        .trim();

        let actual = lex(src.into());

        let expected = vec![
            OBrack,
            Var("A".into()),
            CBrack,
            Sym("if".into()),
            Indent,
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
            Dedent,
        ];

        assert_eq!(actual, expected);
    }
}
