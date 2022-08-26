use std::{
    cell::Cell,
    cmp::Ordering,
    iter::{once, repeat},
    rc::Rc,
};

use crate::{
    data_structures::Sym,
    my_nom::{PErr, Res, Span},
    tok::Tok,
};
use nom::{
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{anychar, i64},
    combinator::{recognize, verify},
    sequence::tuple,
    Parser,
};

fn split_indent_text(line: Span) -> Res<Span> {
    take_while(char::is_whitespace)(line)
}

const INDENT_SIZE_IN_SPACES: usize = 4;

fn indent_count(indent_text: Span) -> usize {
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

    match indent_char {
        Some(Tabs) => indent_text.len(),
        Some(Spaces) if indent_text.len() % INDENT_SIZE_IN_SPACES == 0 => {
            indent_text.len() / INDENT_SIZE_IN_SPACES
        }
        Some(Spaces) => panic!("Indent is not a multiple of {INDENT_SIZE_IN_SPACES} spaces!"),
        _ => unreachable!(),
    }
}

#[derive(Clone)]
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
            let out: Box<dyn Iterator<Item = _>> = match curr.cmp(&prev.get()) {
                Ordering::Equal => Box::new(once(SimpleTok::Text(line))),
                Ordering::Greater => Box::new(
                    repeat(SimpleTok::Indent)
                        .take(curr - prev.get())
                        .chain(once(SimpleTok::Text(line))),
                ),
                Ordering::Less => Box::new(
                    repeat(SimpleTok::Dedent)
                        .take(prev.get() - curr)
                        .chain(once(SimpleTok::Text(line))),
                ),
            };
            prev.replace(curr);
            out
        })
        .chain(repeat(SimpleTok::Dedent).take_while(move |_| {
            let old = prev.get();
            if old > 0 {
                prev.replace(old - 1);
            }
            old > 0
        }))
}

#[derive(Debug, Clone, Copy)]
enum BlockCtx {
    Block,
    Bracketed,
}

fn text_literal(i: Span) -> Res<String> {
    let (i, _) = tag("\"")(i)?;
    let (i, text) = take_while(|c: char| c != '"')(i)?;
    let (i, _) = tag("\"")(i)?;
    Ok((i, text.to_string()))
}

fn any_symbol(i: Span) -> Res<Sym> {
    recognize(tuple((
        verify(anychar::<Span, _>, |&c| c.is_alphabetic() || c == '_'),
        take_while(|c: char| c.is_alphanumeric() || c == '_'),
    )))
    .map(|span| span.to_string())
    .parse(i)
}

fn tokenize_line<'st, 'i: 'st>(
    mut line: Span<'i>,
    state: &'st mut BlockCtx,
) -> impl Iterator<Item = Tok> + 'st {
    std::iter::from_fn(move || loop {
        if line.is_empty() {
            return None;
        }

        if let Ok((rem, num)) = i64::<_, PErr<'i>>(line) {
            line = rem;
            return Some(Tok::Num(num));
        }

        if let Ok((rem, txt)) = text_literal(line) {
            line = rem;
            return Some(Tok::Txt(txt));
        }

        if let Ok((rem, sym)) = any_symbol(line) {
            line = rem;
            if sym.starts_with(char::is_uppercase) {
                return Some(Tok::Var(sym.into()));
            } else if sym.starts_with(char::is_lowercase) {
                return Some(Tok::Sym(sym.into()));
            } else {
                todo!()
            }
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("][")(line) {
            line = rem;
            return Some(Tok::COBrack);
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

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("(")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OParen);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>(")")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CParen);
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
            return Some(Tok::Comma);
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("...")(line) {
            line = rem;
            return Some(Tok::Spread);
        }

        if let Ok((rem, _)) = take_while1::<_, _, PErr<'i>>(char::is_whitespace)(line) {
            line = rem;
            continue;
        }

        panic!("Unknown symbol encountered: {:?}", &line[0..5])
    })
}

pub fn tokenize(src: Span) -> Vec<Tok> {
    let lines = src
        .lines()
        .map(|s| s.into()) // TODO: do we lose position info?
        .filter_map(|line| split_indent_text(line).ok())
        .map(|(text, ws)| (indent_count(ws), text));

    let simple_toks = insert_indent_dedent_tokens(lines);

    let mut state = BlockCtx::Block;
    let mut tokens = vec![];

    for stok in simple_toks {
        match (stok, state) {
            (SimpleTok::Text(line), _) => tokens.extend(tokenize_line(line, &mut state)),
            (SimpleTok::Indent, BlockCtx::Block) => tokens.push(Tok::Indent),
            (SimpleTok::Dedent, BlockCtx::Block) => tokens.push(Tok::Dedent),
            _ => {}
        }
    }

    tokens
}

#[cfg(test)]
mod block_tests {
    use super::tokenize;
    use super::Tok::*;
    use crate::my_nom::Span;
    use pretty_assertions::assert_eq;

    #[test]
    fn tokenize_basic_relation_def() {
        let src = r"
[A] if
    - [blah]
    - [florp]
    - [glop]
        "
        .trim();

        let actual = tokenize(src.into());

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

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn tokenize_relation_term() {
        let src = r#"[asdf Qwerty][Poiu]"#;
        let actual = tokenize(Span::from(src));
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
