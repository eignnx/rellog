use std::{
    cell::{RefCell, RefMut},
    cmp::Ordering,
};

use crate::{
    data_structures::Sym,
    my_nom::{Res, Span},
    tok::{At, Tok},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::multispace0,
    combinator::{opt, value},
    error::{make_error, ErrorKind},
    multi::many0,
    sequence::preceded,
    Parser,
};

fn lex_token<'state, 'i: 'state>(
    st: &'state RefCell<LexState>,
) -> impl FnMut(Span<'i>) -> Res<'i, Tok> + 'state {
    move |i: Span<'i>| {
        let mut st = st.borrow_mut();

        let (i, tok) = preceded(
            multispace0,
            alt((
                value(Tok::COBrack, tag("][")),
                value(Tok::OBrack, tag("[")),
                value(Tok::CBrack, tag("]")),
                value(Tok::OBrace, tag("{")),
                value(Tok::CBrace, tag("}")),
                value(Tok::Dash, tag("-")),
                value(Tok::Pipe, tag("|")),
                value(Tok::Comma, tag(",")),
                value(Tok::Spread, tag("...")),
            )),
        )(i)?;

        st.push_tok(tok.clone().at(i));

        match tok {
            Tok::Dash | Tok::Pipe => {
                let indent = i.get_utf8_column();
                st.block_ctx.push(BlockCtx::Block(indent));
            }
            Tok::OBrack | Tok::OBrace => {
                st.block_ctx.push(BlockCtx::WsInsensitive);
            }
            Tok::CBrack | Tok::CBrace => {
                st.block_ctx.pop().expect("non-empty block_ctx");
            }
            _ => {}
        }

        Ok((i, tok))
    }
}

fn parse_indentation<'i>(i: Span<'i>) -> Res<'i, usize> {
    if i.is_empty() {
        return Err(nom::Err::Error(make_error(i, ErrorKind::NonEmpty)));
    }

    take_while(|c| c == ' ')
        .map(|ws: Span<'i>| ws.len())
        .parse(i)
}

fn parse_block_line<'state, 'i: 'state>(
    st: &'state RefCell<LexState>,
) -> impl FnMut(Span<'i>) -> Res<'i, ()> + 'state {
    move |i: Span<'i>| {
        if i.is_empty() {
            return Err(nom::Err::Error(make_error(i, ErrorKind::Eof)));
        }

        let (i, indent) = take_while(|c| c == ' ')
            .map(|ws: Span<'i>| ws.len())
            .parse(i)?;

        let (i, line) = take_while(|c| c != '\n')(i)?;

        if !line.is_empty() {
            st.borrow_mut().indent(indent, i)?;
            let (_i, _vec) = many0(lex_token(st))(line)?;
        }

        let (i, _) = opt(tag("\n"))(i)?;

        Ok((i, ()))
    }
}

fn parse_block<'state, 'i: 'state>(
    st: &'state RefCell<LexState>,
) -> impl FnMut(Span<'i>) -> Res<'i, ()> + 'state {
    move |i: Span<'i>| {
        let (i, _) = many0(parse_block_line(st))(i)?;
        Ok((i, ()))
    }
}

#[derive(Debug, Clone, Copy)]
enum BlockCtx {
    Block(usize),
    WsInsensitive,
}

#[derive(Debug)]
struct BadDedent;

struct LexState {
    block_ctx: Vec<BlockCtx>,
    toks: Vec<At<Tok>>,
}

impl LexState {
    fn new() -> Self {
        Self {
            block_ctx: vec![],
            toks: vec![],
        }
    }

    fn new_cell() -> RefCell<Self> {
        RefCell::new(Self::new())
    }

    #[must_use]
    fn indent<'i>(&mut self, new_indent: usize, i: Span<'i>) -> Res<'i> {
        if let Some(BlockCtx::Block(prev_indent)) = self.block_ctx.pop() {
            match new_indent.cmp(&prev_indent) {
                Ordering::Equal => {}
                Ordering::Greater => {
                    self.push_tok(Tok::Indent.at(i));
                }
                Ordering::Less => {
                    self.push_tok(Tok::Dedent.at(i));
                }
            }
        }

        Ok((i, ()))
    }

    fn current_block_ctx(&self) -> BlockCtx {
        self.block_ctx
            .last()
            .copied()
            .unwrap_or(BlockCtx::WsInsensitive)
    }

    fn push_tok(&mut self, tok: At<Tok>) {
        self.toks.push(tok);
    }
}

#[cfg(test)]
mod block_tests {
    use crate::lex::LexState;

    use super::parse_block;
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

        let st = LexState::new_cell();
        let (rest, ()) = parse_block(&st)(src.into()).unwrap();

        assert_eq!(rest.to_string(), "".to_string());

        let actual = st
            .into_inner()
            .toks
            .into_iter()
            .map(At::without_loc)
            .collect::<Vec<_>>();

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
