use std::{fmt::Display, path::PathBuf, str::FromStr};

use crate::{
    data_structures::{Int, Sym, Var},
    lex::tok::{At, MakeAt, Tok},
    utils::my_nom::{Res, Span},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while, take_while1},
    character::complete::{multispace0, satisfy},
    combinator::{cut, fail, opt, recognize},
    error::{context, VerboseError},
    multi::many0,
    sequence::{delimited, preceded, tuple},
    Finish, Parser,
};
use nom_i9n::{I9nInput, TokenizedInput};

use super::tok::SPREAD;

// fn text_literal(i: Span<'_>) -> Res<'_, String> {
//     let (i, raw_text) = alt((multiline_txt_lit, single_line_txt_lit)).parse(i)?;
//     let txt_tm = parse_txt_lit_content(&raw_text);
//     todo!();
//     Ok((i, txt_tm))
// }

// fn single_line_txt_lit(i: Span) -> Res<String> {
//     let (i, _) = tag("\"")(i)?;
//     let (i, text) = take_while(|c: char| c != '"')(i)?;
//     let (i, _) = tag("\"")(i)?;
//     Ok((i, String::from(*text.fragment())))
// }

// fn multiline_txt_lit(i: Span) -> Res<String> {
//     let start_col = i.get_column() - 1;
//     let (i, _) = tag(r#"""""#)(i)?;
//     let (i, text) = take_until(r#"""""#)(i)?;
//     let (i, _) = cut(tag(r#"""""#))(i)?;

//     let mut out = String::new();

//     if text.starts_with('\n') || text.starts_with("\r\n") {
//         // _ _ _ """
//         // _ _ _ _ _ asdf
//         // _ _ _ """
//         // should produce
//         // """
//         // _ _ asdf
//         // """
//         for line in text.lines().skip(1) {
//             if line.trim().is_empty() {
//                 out.push('\n');
//             } else {
//                 if line.len() <= start_col {
//                     return context("Not enough indentation in multiline string literal.", fail)
//                         .parse(i);
//                 }
//                 let dedented = &line[start_col..];
//                 out.push_str(dedented);
//             }
//         }

//         if out.ends_with('\n') {
//             out.pop();
//         }
//     } else {
//         // _ _ _ """adsf
//         // _ _ _ _ _ _ asdf
//         // _ _ _ """
//         // should produce
//         // "adsf\n _ asdf"
//         out.push_str(text.lines().next().unwrap_or_default());
//         out.push('\n');
//         let start_col = start_col + 3;
//         for line in text.lines().skip(1) {
//             out.push_str(&line[start_col..]);
//             out.push('\n');
//         }
//         out.pop();
//     }

//     Ok((i, out))
// }

fn unquoted_sym(i: Span) -> Res<Sym> {
    recognize(tuple((
        satisfy(|c| c.is_alphabetic() && c.is_lowercase()),
        take_while(|c: char| (c.is_alphabetic() && c.is_lowercase()) || c.is_numeric() || c == '_'),
    )))
    .map(|span: Span| -> Sym { span.fragment().into() })
    .parse(i)
}

fn quoted_sym(i: Span) -> Res<Sym> {
    delimited(
        tag("'"),
        recognize(take_while(|c: char| c != '\'')),
        tag("'"),
    )
    .map(|span: Span| -> Sym { span.fragment().into() })
    .parse(i)
}

pub fn sym(i: Span) -> Res<Sym> {
    alt((quoted_sym, unquoted_sym)).parse(i)
}

pub fn var(i: Span) -> Res<Var> {
    let name_parser = recognize(
        satisfy(|c| c.is_uppercase() || c == '_').and(many0(satisfy(|c| c.is_alphanumeric()))),
    );

    let nat_sym = take_while1(char::is_numeric).map(|i: Span| Sym::from(i));

    let suffix_parser = preceded(
        tag(Var::SUFFIX_SEPARATOR),
        context(
            "unquoted symbol or non-negative whole number",
            cut(alt((unquoted_sym, nat_sym))),
        ),
    );

    name_parser
        .and(opt(suffix_parser))
        .map(|(name, suffix)| Var::from_source(name, suffix))
        .parse(i)
}

fn int(i: Span) -> Res<Int> {
    recognize(opt(tag("-")).and(take_while1(char::is_numeric)))
        .map(|i: Span| {
            Int::from_str(i.as_ref())
                .expect("take_while1(char::is_numeric) feeds into Int::from_str without error")
        })
        .parse(i)
}

#[derive(Debug, Clone)]
pub struct LexError {
    pub file: Option<PathBuf>,
    pub line: usize,
    pub column: usize,
    pub fragment: String,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            file,
            line,
            column,
            fragment,
        } = self;
        write!(
            f,
            "[{}:{line}:{column}]: `{fragment}…`",
            file.as_ref().unwrap().to_string_lossy()
        )
    }
}

impl<'i> From<VerboseError<Span<'i>>> for LexError {
    fn from(ve: VerboseError<Span<'i>>) -> Self {
        // debug_assert!(ve.errors.len() == 1);
        let (i, _kind) = ve.errors.first().expect("nonempty error list");
        let fragment = i.fragment().trim_start();
        let preview_len = fragment.len().min(5);
        let fragment = fragment[..preview_len].to_string();
        Self {
            file: None,
            line: i.location_line() as usize,
            column: i.get_column(), // TODO: use `i.get_utf8_column()` instead?
            fragment,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LexCtx {
    QuotedTxt,
    TripleQuotedTxt,
    Expr,
}

fn tokenize_impl<'i>(mut input: Span<'i>, ts: &mut Vec<At<Tok>>) -> Res<'i, ()> {
    let mut stack: Vec<LexCtx> = vec![LexCtx::Expr];

    while !input.is_empty() {
        let i = input;
        let state = *stack
            .last()
            .expect("stack begins initialized with LexCtx::Expr");

        let (i, t) = match state {
            LexCtx::Expr => {
                let (i, _) = multispace0(i)?;
                input = i;
                let (i, t) = alt((
                    int.map(Tok::Int),
                    sym.map(Tok::Sym),
                    var.map(Tok::Var),
                    tag("\"\"\"").map(|_| Tok::OTripleQuote),
                    tag("\"").map(|_| Tok::OQuote),
                    // Nested alt because max tuple size is 21.
                    alt((
                        tag("[{").map(|_| Tok::OTxtInterp), // Keep this in here for error reporting purposes.
                        tag("][").map(|_| Tok::COBrack),
                        tag("[").map(|_| Tok::OBrack),
                        tag("]").map(|_| Tok::CBrack),
                        tag("{").map(|_| Tok::OBrace),
                        tag("}").map(|_| Tok::CBrace),
                        tag("(").map(|_| Tok::OParen),
                        tag(")").map(|_| Tok::CParen),
                    )),
                    tag("-").map(|_| Tok::Dash),
                    tag("|").map(|_| Tok::Pipe),
                    tag(",").map(|_| Tok::Comma),
                    tag(";").map(|_| Tok::Semicolon),
                    tag(SPREAD).map(|_| Tok::Spread),
                    tag("=").map(|_| Tok::Equal),
                    tag("~").map(|_| Tok::Tilde),
                    tag("::").map(|_| Tok::PathSep),
                ))
                .parse(i)?;

                match t {
                    Tok::OQuote => stack.push(LexCtx::QuotedTxt),
                    Tok::OTripleQuote => stack.push(LexCtx::TripleQuotedTxt),
                    _ => {}
                }

                (i, t.at(input))
            }

            LexCtx::QuotedTxt | LexCtx::TripleQuotedTxt => {
                let mut buf = String::new();
                let mut inp = i;
                loop {
                    // Just search for 1 end quote (or open bracket). We'll deal with
                    // triple quotes after.
                    let (i, content) = take_till(|c| c == '"' || c == '[').parse(inp)?;
                    buf.push_str(content.as_ref());
                    // Gotta do a double check because `take_till` can't search for both
                    // triple quotes AND open bracket.
                    match state {
                        LexCtx::QuotedTxt if i.starts_with('"') => {
                            break (i, Tok::CQuote.at(input));
                        }
                        LexCtx::TripleQuotedTxt if i.starts_with("\"\"\"") => {
                            break (i, Tok::CTripleQuote.at(input));
                        }
                        _ if i.starts_with("[{") && i.is_empty() => {
                            break (i, Tok::OTxtInterp.at(input));
                        }
                        _ => {} // Otherwise, we need to keep searching for the triple quote (or bracket).
                    }
                    inp = i;
                }
            }
        };

        match &t.value {
            Tok::OQuote => {
                stack.push(LexCtx::QuotedTxt);
            }
            Tok::CQuote => {
                let Some(LexCtx::QuotedTxt) = stack.pop() else {
                    return context("Mismatched quote mark", fail).parse(i);
                };
            }
            Tok::OTripleQuote => {
                stack.push(LexCtx::TripleQuotedTxt);
            }
            Tok::CTripleQuote => {
                let Some(LexCtx::TripleQuotedTxt) = stack.pop() else {
                    return context("Mismatched triple quote mark", fail).parse(i);
                };
            }
            Tok::OTxtInterp => {
                let Some(LexCtx::Expr) = stack.pop() else {
                    return context("Unmatched opening text interpolation bracket pair", fail)
                        .parse(i);
                };
                stack.push(LexCtx::Expr);
            }
            Tok::CTxtInterp => {
                let Some(LexCtx::Expr) = stack.pop() else {
                    return context("Unmatched closing text interpolation bracket pair", fail)
                        .parse(i);
                };
            }
            _ => {}
        }

        ts.push(t);
        input = i;
    }

    Ok((input, ()))
}

pub fn tokenize<'i>(
    src: impl Into<Span<'i>> + 'i,
    filename: PathBuf,
) -> Result<Vec<At<Tok>>, LexError> {
    let input: Span = src.into();
    let mut ts = Vec::new();
    tokenize_impl(input, &mut ts)
        .finish()
        .map_err(|e| LexError {
            file: Some(filename),
            ..LexError::from(e)
        })?;
    Ok(ts)
}

type RellogTokenFinder<'tokbuf> = TokenizedInput<&'tokbuf [At<Tok>], At<Tok>>;

pub fn tokenize_into<'i, 'buf>(
    buf: &'buf mut Vec<At<Tok>>,
    src: impl Into<Span<'i>> + 'i,
    filename: PathBuf,
) -> Result<I9nInput<&'buf [At<Tok>], RellogTokenFinder<'buf>>, LexError> {
    *buf = tokenize(src, filename)?;
    Ok(I9nInput::from(&**buf))
}

#[cfg(test)]
mod block_tests {
    use super::tokenize;
    use super::Tok::*;
    use crate::data_structures::Var;
    use crate::lex::tok::At;
    use pretty_assertions::assert_eq;

    fn src_var(s: &str) -> Var {
        Var::from_source(s, None)
    }

    #[test]
    fn tokenize_basic_relation_def() {
        crate::init_interner();
        let src = r"


[A]
    - [blah]
    - [florp]
    - [glop]
";

        let actual = tokenize(src, "blah.rellog".into())
            .unwrap()
            .into_iter()
            .map(At::value)
            .collect::<Vec<_>>();

        let expected = vec![
            OBrack,
            Var(src_var("A")),
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

        let actual = tokenize(src, "blah.rellog".into())
            .unwrap()
            .into_iter()
            .map(At::value)
            .collect::<Vec<_>>();

        let expected = vec![
            OBrack,
            Sym("asdf".into()),
            Var(src_var("Qwerty")),
            COBrack,
            Var(src_var("Poiu")),
            CBrack,
        ];

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn test_txt_interpolation() {
        let src = r#" "BEFORE[{X}]AFTER" "#;

        let actual = tokenize(src, "blah.rellog".into())
            .unwrap()
            .into_iter()
            .map(At::value)
            .collect::<Vec<_>>();

        let expected = vec![
            OQuote,
            TxtContent("BEFORE".to_string()),
            OTxtInterp,
            Var(src_var("X")),
            CTxtInterp,
            TxtContent("AFTER".to_string()),
            CQuote,
        ];

        assert_eq!(actual, expected, "Input was {:?}", src);
    }
}
