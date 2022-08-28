use std::{
    cell::Cell,
    cmp::Ordering,
    iter::{once, repeat, repeat_with},
    rc::Rc,
};

use crate::{
    data_structures::Sym,
    my_nom::{PErr, Res, Span},
    tok::{At, MakeAt, Tok},
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
) -> impl Iterator<Item = At<SimpleTok<'i>>> {
    let prev = Rc::new(Cell::new(0usize));
    let end = Rc::new(Cell::new(None));

    let (fm_prev, fm_end) = (prev.clone(), end.clone());

    lines
        .flat_map(move |(curr, line)| {
            let prev = fm_prev.clone();
            let end = fm_end.clone();
            let out: Box<dyn Iterator<Item = _>> = match curr.cmp(&prev.get()) {
                Ordering::Equal => Box::new(once(SimpleTok::Text(line).at(line))),
                Ordering::Greater => Box::new(
                    repeat(SimpleTok::Indent.at(line))
                        .take(curr - prev.get())
                        .chain(once(SimpleTok::Text(line).at(line))),
                ),
                Ordering::Less => Box::new(
                    repeat(SimpleTok::Dedent.at(line))
                        .take(prev.get() - curr)
                        .chain(once(SimpleTok::Text(line).at(line))),
                ),
            };
            prev.replace(curr);
            end.replace(Some(line));
            out
        })
        .chain(
            repeat_with(move || SimpleTok::Dedent.at(end.get().unwrap())).take_while(move |_| {
                let old = prev.get();
                if old > 0 {
                    prev.replace(old - 1);
                }
                old > 0
            }),
        )
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
    .map(|span| span.fragment().into())
    .parse(i)
}

fn tokenize_line<'st, 'i: 'st>(
    mut line: Span<'i>,
    state: &'st mut BlockCtx,
) -> impl Iterator<Item = At<Tok>> + 'st {
    std::iter::from_fn(move || loop {
        if line.is_empty() {
            return None;
        }

        let old_line = line;

        if let Ok((rem, num)) = i64::<_, PErr<'i>>(line) {
            line = rem;
            return Some(Tok::Num(num).at(old_line));
        }

        if let Ok((rem, txt)) = text_literal(line) {
            line = rem;
            return Some(Tok::Txt(txt).at(old_line));
        }

        if let Ok((rem, sym)) = any_symbol(line) {
            line = rem;
            if sym.to_str().starts_with(char::is_uppercase) {
                return Some(Tok::Var(sym).at(old_line));
            } else if sym.to_str().starts_with(char::is_lowercase) {
                return Some(Tok::Sym(sym).at(old_line));
            } else {
                todo!()
            }
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("][")(line) {
            line = rem;
            return Some(Tok::COBrack.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("[")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OBrack.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("]")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CBrack.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("{")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OBrace.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("}")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CBrace.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("(")(line) {
            line = rem;
            *state = BlockCtx::Bracketed;
            return Some(Tok::OParen.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>(")")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::CParen.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("-")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::Dash.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("|")(line) {
            line = rem;
            *state = BlockCtx::Block;
            return Some(Tok::Pipe.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>(",")(line) {
            line = rem;
            return Some(Tok::Comma.at(old_line));
        }

        if let Ok((rem, _)) = tag::<_, _, PErr<'i>>("...")(line) {
            line = rem;
            return Some(Tok::Spread.at(old_line));
        }

        if let Ok((rem, _)) = take_while1::<_, _, PErr<'i>>(char::is_whitespace)(line) {
            line = rem;
            continue;
        }

        panic!(
            "[{}:{}] Unknown symbol encountered: {:?}",
            line.location_line(),
            line.get_column(),
            line.chars().take(5).collect::<String>(),
        )
    })
}

fn span_lines(mut src: Span) -> impl Iterator<Item = Span> {
    use nom::Slice;
    std::iter::from_fn(move || -> Option<Span> {
        if src.is_empty() {
            return None;
        }
        if let Some(idx) = src.find('\n') {
            let old = src;
            src = src.slice(idx + 1..);
            Some(old.slice(0..idx))
        } else {
            Some(src)
        }
    })
}

pub fn tokenize<'i>(src: impl Into<Span<'i>> + 'i) -> Vec<At<Tok>> {
    let lines = span_lines(src.into())
        .filter_map(|line| split_indent_text(line).ok())
        .map(|(text, ws)| (indent_count(ws), text));

    let simple_toks = insert_indent_dedent_tokens(lines);

    let mut state = BlockCtx::Block;
    let mut tokens: Vec<At<Tok>> = vec![];

    for stok in simple_toks {
        match (&stok.value, state) {
            (SimpleTok::Text(line), _) => tokens.extend(tokenize_line(*line, &mut state)),
            (SimpleTok::Indent, BlockCtx::Block) => tokens.push(Tok::Indent.copy_loc(&stok)),
            (SimpleTok::Dedent, BlockCtx::Block) => tokens.push(Tok::Dedent.copy_loc(&stok)),
            _ => {}
        }
    }

    tokens
}

#[cfg(test)]
mod block_tests {
    use super::tokenize;
    use super::Tok::*;
    use crate::tok::At;
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

        let actual = tokenize(src).into_iter().map(At::value).collect::<Vec<_>>();

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
