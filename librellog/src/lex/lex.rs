use std::{assert_matches::debug_assert_matches, fmt::Display, path::PathBuf, str::FromStr};

use crate::{
    data_structures::{Int, Sym, Var},
    lex::tok::{At, MakeAt, Tok},
    utils::my_nom::{Res, Span},
};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{anychar, multispace0, satisfy},
    combinator::{cut, fail, not, opt, recognize, value},
    error::{context, VerboseError},
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    Finish, Parser,
};
use nom_i9n::{I9nInput, TokenizedInput};

use super::tok::SPREAD;

fn unquoted_sym(i: Span) -> Res<Sym> {
    recognize(tuple((
        satisfy(|c| c.is_alphabetic() && c.is_lowercase()),
        take_while(|c: char| (c.is_alphabetic() && c.is_lowercase()) || c.is_numeric() || c == '_'),
    )))
    .map(|span: Span| -> Sym { Sym::from(*span.fragment()) })
    .parse(i)
}

fn quoted_sym(i: Span) -> Res<Sym> {
    delimited(
        tag("'"),
        recognize(take_while(|c: char| c != '\'')),
        tag("'"),
    )
    .map(|span: Span| -> Sym { Sym::from(*span.fragment()) })
    .parse(i)
}

pub fn sym(i: Span) -> Res<Sym> {
    alt((quoted_sym, unquoted_sym)).parse(i)
}

pub fn var(i: Span) -> Res<Var> {
    let mut name_parser = recognize(
        satisfy(|c| c.is_uppercase() || c == '_').and(many0(satisfy(|c| c.is_alphanumeric()))),
    );

    let nat_sym = take_while1(char::is_numeric).map(|i: Span| Sym::from(*i.fragment()));

    let suffix_parser = preceded(
        tag(Var::SUFFIX_SEPARATOR),
        context(
            "unquoted symbol or non-negative whole number",
            cut(alt((unquoted_sym, nat_sym))),
        ),
    );

    let (i, name) = name_parser.parse(i)?;
    let (i, suffix) = opt(suffix_parser).parse(i)?;
    Ok((i, Var::from_source(*name.fragment(), suffix)))
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
    pub description: String,
}

impl Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            file,
            line,
            column,
            fragment,
            description,
        } = self;
        let file = file.as_ref().map_or("".into(), |f| f.to_string_lossy());
        writeln!(f, "TOKENIZATION ERROR")?;
        writeln!(f, "[{file}:{line}:{column}]: {description}")?;
        writeln!(f, "⇒ `{fragment}`")?;
        Ok(())
    }
}

impl<'i> From<VerboseError<Span<'i>>> for LexError {
    fn from(ve: VerboseError<Span<'i>>) -> Self {
        let (i, kind) = ve.errors.first().expect("nonempty error list");
        let fragment = i.fragment();
        const PREVIEW_LEN: usize = 5;
        let preview_len = usize::min(fragment.len(), PREVIEW_LEN);
        let ellipsis_indicator = if preview_len == PREVIEW_LEN {
            '…'
        } else {
            '␃'
        };
        let fragment = format!("{}{}", &fragment[..preview_len], ellipsis_indicator);
        Self {
            file: None,
            line: i.location_line() as usize,
            column: i.get_column(), // TODO: use `i.get_utf8_column()` instead?
            fragment,
            description: match kind {
                nom::error::VerboseErrorKind::Context(msg) => msg.to_string(),
                nom::error::VerboseErrorKind::Char(c) => format!("Expected character `{c}`"),
                nom::error::VerboseErrorKind::Nom(e) => {
                    format!("Parser `{}` failed to tokenize input", e.description())
                }
            },
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum LexCtx {
    Expr,
    Quoted(Quote),
    TxtInterp,
}

#[derive(Debug, Clone, Copy)]
enum Quote {
    One,
    Three,
}

fn expr_tok(i: Span) -> Res<Tok> {
    let (i, _) = multispace0(i)?;
    alt((
        // Nested `alt`s because max tuple size is 21.
        alt((
            int.map(Tok::Int),
            sym.map(Tok::Sym),
            var.map(Tok::Var),
            value(
                Tok::OTripleQuote,
                terminated(tag("\"\"\""), opt(alt((tag("\r\n"), tag("\n"))))),
            ),
            value(Tok::OQuote, tag("\"")),
        )),
        alt((
            value(Tok::OTxtInterp, tag("[{")), // Keep this in here for error reporting purposes.
            value(Tok::COBrack, tag("][")),
            value(Tok::OBrack, tag("[")),
            value(Tok::CBrack, tag("]")),
            value(Tok::OBrace, tag("{")),
            value(Tok::CBrace, tag("}")),
            value(Tok::OParen, tag("(")),
            value(Tok::CParen, tag(")")),
        )),
        alt((
            value(Tok::Dash, tag("-")),
            value(Tok::Pipe, tag("|")),
            value(Tok::Comma, tag(",")),
            value(Tok::Semicolon, tag(";")),
            value(Tok::Spread, tag(SPREAD)),
            value(Tok::Equal, tag("=")),
            value(Tok::Tilde, tag("~")),
            value(Tok::PathSep, tag("::")),
        )),
    ))
    .parse(i)
}

fn tokenize_impl<'i>(mut input: Span<'i>, ts: &mut Vec<At<Tok>>) -> Res<'i, ()> {
    let mut stack: Vec<LexCtx> = vec![LexCtx::Expr];

    while !input.is_empty() {
        let state = *stack
            .last()
            .expect("stack begins initialized with LexCtx::Expr");

        let (i, t) = match state {
            LexCtx::Expr => {
                let (i_after_ws, _) = multispace0(input)?;
                let (i, t) = context("Token in expression context", expr_tok)(i_after_ws)?;
                (i, t.at(i_after_ws))
            }

            LexCtx::Quoted(q) => {
                let mut buf = String::new();
                let close_quote = match q {
                    Quote::One => |i| value(Tok::CQuote, tag("\"")).parse(i),
                    Quote::Three => |i| {
                        value(
                            Tok::CTripleQuote,
                            preceded(opt(alt((tag("\r\n"), tag("\n")))), tag("\"\"\"")),
                        )
                        .parse(i)
                    },
                };

                let (i, content) = context(
                    "Text content",
                    recognize(many0(
                        not(alt((&close_quote, value(Tok::OTxtInterp, tag("[{"))))).and(anychar),
                    )),
                )
                .parse(input)?;

                buf.push_str(content.as_ref());
                if !buf.is_empty() {
                    // Skip empty strings.
                    ts.push(Tok::TxtContent(buf).at(input));
                }
                input = i;

                // Now parse escape or closing quote.
                let (i, end) = context(
                    "Closing quote or escape",
                    alt((close_quote, value(Tok::OTxtInterp, tag("[{")))),
                )
                .parse(i)?;
                (i, end.at(input))
            }

            LexCtx::TxtInterp => {
                let (i_after_ws, _) = multispace0(input)?;
                let close_interp = value(Tok::CTxtInterp, tag("}]"));
                context(
                    "Expression token or closing template interpolation symbol `}]`",
                    alt((close_interp, expr_tok)),
                )
                .map(|t| t.at(i_after_ws))
                .parse(i_after_ws)?
            }
        };

        match &t.value {
            Tok::OQuote => {
                debug_assert_matches!(stack.last(), Some(LexCtx::Expr | LexCtx::TxtInterp) | None);
                stack.push(LexCtx::Quoted(Quote::One));
            }
            Tok::CQuote => {
                let Some(LexCtx::Quoted(Quote::One)) = stack.pop() else {
                    return context("Mismatched quote mark", fail).parse(i);
                };
            }
            Tok::OTripleQuote => {
                debug_assert_matches!(stack.last(), Some(LexCtx::Expr | LexCtx::TxtInterp) | None);
                stack.push(LexCtx::Quoted(Quote::Three));
            }
            Tok::CTripleQuote => {
                let Some(LexCtx::Quoted(Quote::Three)) = stack.pop() else {
                    return context("Mismatched triple quote mark", fail).parse(i);
                };
            }
            Tok::OTxtInterp => {
                debug_assert_matches!(stack.last(), Some(LexCtx::Quoted(_)));
                stack.push(LexCtx::TxtInterp);
            }
            Tok::CTxtInterp => {
                let Some(LexCtx::TxtInterp) = stack.pop() else {
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
    match tokenize_impl(input, &mut ts).finish() {
        Ok((_, _)) => Ok(ts),
        Err(e) => {
            if e.errors.first().unwrap().0.fragment().is_empty() {
                Ok(ts)
            } else {
                Err(LexError {
                    file: Some(filename),
                    ..LexError::from(e)
                })
            }
        }
    }
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
        crate::init_interner();
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
        crate::init_interner();
        let src = r#" "BEFORE[{X}]AFTER" "#;

        let actual = tokenize(src, "blah.rellog".into())
            .unwrap_or_else(|e| panic!("{}", e))
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
