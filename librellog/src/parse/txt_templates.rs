use nom::{
    character::complete::multispace0, combinator::all_consuming, multi::many0,
    sequence::terminated, Finish, Parser,
};

use crate::{
    ast::partial_char_list::PartialCharList,
    lex::{one_token, tok::Tok, LexError},
    parse::{tm, tok},
    utils::my_nom::Span,
};

pub fn process_txt_template(content: &str) -> Option<PartialCharList> {
    if !content.ends_with(']') {
        return Some(content.into());
    }

    let (before_tail, tail) = find_tail_brackets(content)?;

    let src: Span = tail.into();
    let tokens = all_consuming(terminated(many0(one_token), multispace0))
        .parse(src)
        .finish()
        .map(|(_i, tokens)| tokens)
        .map_err(|e| {
            let mut e = LexError::from(e);
            e.file = Some("<text-template>".into());
            e
        })
        .ok()?;

    let ts = tokens[..].into();
    let (ts, _) = tok(Tok::OBrack).parse(ts).ok()?;
    let (ts, _) = tok(Tok::Spread).parse(ts).ok()?;
    let (ts, tail_term) = tm(ts).ok()?;
    let (ts, _) = tok(Tok::CBrack).parse(ts).ok()?;

    if !ts.is_empty() {
        return None;
    }

    Some(PartialCharList::from_string_and_tail(
        before_tail,
        tail_term.into(),
    ))
}

fn find_tail_brackets(content: &str) -> Option<(&str, &str)> {
    let mut bracket_balance = 0;

    for (i, ch) in content.char_indices().rev() {
        match ch {
            '[' => bracket_balance += 1,
            ']' => bracket_balance -= 1,
            _ => {}
        }

        if bracket_balance == 0 {
            return Some(content.split_at(i));
        }
    }
    None
}

#[cfg(test)]
mod txt_template_tests {
    use crate::{
        ast::{RcTm, Tm},
        data_structures::Var,
        init_interner,
    };

    use super::*;
    use assert2::check;

    #[test]
    fn test_it_out() {
        init_interner();

        let content = "Once upon a time[..Rest]";
        let (before, after) = find_tail_brackets(content).unwrap();
        check!(before == "Once upon a time");
        check!(after == "[..Rest]");

        let cl = process_txt_template(content).unwrap();
        check!(cl.tail() == &RcTm::from(Tm::Var(Var::from("Rest"))));
        check!(format!("{cl:?}") == format!("{content}"));
    }
}
