use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
    path::PathBuf,
};

use char_list::CharList;
use nom::{
    branch::alt,
    combinator::{all_consuming, cut, opt},
    error::{context, ParseError},
    error::{ContextError, ErrorKind},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, tuple},
    Finish, IResult, Parser,
};
use nom_i9n::{I9nError, I9nErrorCtx, I9nErrorSituation, I9nInput, TokenizedInput};
use rpds::Vector;

use crate::{
    ast::{BinOpSymbol, Clause, Item, Module, RcTm, Rel, Tm},
    data_structures::{Int, Map, Sym, Var},
    lex::{
        tok::{
            At,
            Tok::{self, *},
        },
        LexError,
    },
};

type Res<'ts, T> = IResult<Toks<'ts>, T, Error<'ts>>;
type BaseInput<'ts> = &'ts [At<Tok>];
type TokenFinder<'ts> = TokenizedInput<BaseInput<'ts>, At<Tok>>;
type Toks<'ts> = I9nInput<BaseInput<'ts>, TokenFinder<'ts>>;

/// TODO: Replace with Miette error type!
#[derive(Debug)]
pub struct Error<'ts> {
    pub fname: Box<RefCell<Option<PathBuf>>>,
    stack: Vec<(BaseInput<'ts>, Problem)>,
}

impl<'ts> From<LexError> for Error<'ts> {
    fn from(le: LexError) -> Self {
        Self {
            fname: Box::new(RefCell::new(le.file.clone())),
            stack: vec![(&[], Problem::LexError(le))],
        }
    }
}

impl<'ts> Display for Error<'ts> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let user_input_buf = PathBuf::from("<unknown_src>");
        let fname_borrow = self.fname.borrow();
        let fname = fname_borrow.as_ref().unwrap_or(&user_input_buf);

        for (i, problem) in &self.stack {
            let loc = match i {
                [t, ..] => format!("{}:{}:{}", fname.display(), t.line, t.col),
                [] => format!("{}:<end>", fname.display()),
            };

            writeln!(f, "\t[{loc}] {}", problem)?;
        }

        if let Some((i, _problem)) = self.stack.last() {
            writeln!(
                f,
                "\t(Last token: `{}`)",
                i.first()
                    .map_or_else(|| "<eof>".to_string(), |tok| tok.value.to_string())
            )?;
        }

        Ok(())
    }
}

impl<'ts> Error<'ts> {
    fn with_message(ts: Toks, message: String) -> Error {
        Error {
            fname: Box::new(RefCell::new(None)),
            stack: vec![(ts.input(), Problem::CustomMessage(message))],
        }
    }
}

impl<'ts> ContextError<Toks<'ts>> for Error<'ts> {
    fn add_context(input: Toks<'ts>, ctx: &'static str, mut other: Self) -> Self {
        other.stack.push((input.input(), Problem::Context(ctx)));
        other
    }
}

#[derive(Debug)]
pub enum Problem {
    /// TODO: replace with miette diagnostic or something
    CustomMessage(String),
    Context(&'static str),
    LexError(LexError),
}

impl Display for Problem {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Problem::CustomMessage(msg) => f.write_str(msg),
            Problem::Context(msg) => write!(f, "While parsing {msg}"),
            Problem::LexError(le) => write!(
                f,
                "While tokenizing [{}:{}:{}]: {}",
                le.file.as_ref().unwrap().to_string_lossy(),
                le.line,
                le.column,
                le.fragment,
            ),
        }
    }
}

impl<'ts> ParseError<Toks<'ts>> for Error<'ts> {
    fn from_error_kind(input: Toks<'ts>, kind: ErrorKind) -> Self {
        Error {
            fname: Box::new(RefCell::new(None)),
            stack: vec![(
                input.input(),
                Problem::CustomMessage(kind.description().to_string()),
            )],
        }
    }

    fn append(input: Toks<'ts>, kind: ErrorKind, mut other: Self) -> Self {
        other.stack.push((
            input.input(),
            Problem::CustomMessage(kind.description().to_string()),
        ));
        other
    }
}

pub fn entire_module(ts: Toks) -> Result<Module, Error> {
    let (_ts, m) = all_consuming(module)(ts).finish()?;
    Ok(m)
}

pub fn entire_term(ts: Toks) -> Result<RcTm, Error> {
    let (_ts, t) = all_consuming(tm).parse(ts).finish()?;
    Ok(t.into())
}

impl<'ts> From<I9nError<BaseInput<'ts>>> for Error<'ts> {
    fn from(e: I9nError<BaseInput<'ts>>) -> Self {
        Error {
            fname: Box::new(RefCell::new(None)),
            stack: vec![(
                e.input,
                Problem::CustomMessage({
                    let I9nError {
                        input,
                        situation,
                        ctx,
                    } = e;
                    let situation = I9nErrorSituationDisplay(situation);
                    let ctx = I9nErrorCtxDisplay(ctx);
                    let input_preview = input
                        .first()
                        .map(|tok| tok.value.to_string())
                        .unwrap_or_else(|| "end of input".into());
                    format!("Indentation error:\n\t\t{situation}\n\t\t{ctx}\n\t\tAt token `{input_preview}`.")
                }),
            )],
        }
    }
}

struct I9nErrorSituationDisplay(I9nErrorSituation);

impl Display for I9nErrorSituationDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let relation = match self.0.relation {
            nom_i9n::I9nRelation::NotGt => "not indented more than",
            nom_i9n::I9nRelation::NotEq => "not the same indentation as",
            nom_i9n::I9nRelation::Gt => "indented more than",
            nom_i9n::I9nRelation::Eq => "indented to the exact same level as",
            nom_i9n::I9nRelation::Lt => "indented less than",
        };

        write!(
            f,
            "Current line was {relation} the current block's indentation.\n\t\t(expected={}, actual={})",
            self.0.expected, self.0.actual
        )?;

        Ok(())
    }
}

struct I9nErrorCtxDisplay(I9nErrorCtx);

impl Display for I9nErrorCtxDisplay {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg = match self.0 {
            I9nErrorCtx::AtNewGroup => "at a new indentation group",
            I9nErrorCtx::WithinLine => "within a line",
            I9nErrorCtx::AtNewLine => "at the beginning of a new line",
            I9nErrorCtx::AtGroupEnd => "at the end of an indentation group",
            I9nErrorCtx::WithinLineButAfterStart => "not at the beginning of a line",
        };
        write!(f, "Location: {msg}.")
    }
}

fn tok<'ts>(tgt: Tok) -> impl Parser<Toks<'ts>, At<Tok>, Error<'ts>> {
    let p = move |ts: Toks<'ts>| -> Res<'ts, At<Tok>> {
        match ts.split_first() {
            Some((t, rest)) if t.as_ref() == &tgt => Ok((rest, t.clone())),
            Some((t, _)) => {
                let e = Error::with_message(
                    ts.clone(),
                    format!("Expected `{tgt}`, got token `{}`", t.value),
                );
                Err(nom::Err::Error(e))
            }
            None => Err(eof_error(ts)),
        }
    };
    nom_i9n::tok(p)
}

fn eof_error(ts: Toks) -> nom::Err<Error> {
    let e = Error::from_error_kind(ts, nom::error::ErrorKind::Eof);
    nom::Err::Error(e)
}

fn sym(ts: Toks) -> Res<Sym> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Sym(s), rest)) => Ok((rest, s)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected symbol, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn var(ts: Toks) -> Res<Var> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Var(v), rest)) => Ok((rest, v)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected variable, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn num(ts: Toks) -> Res<Int> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Int(n), rest)) => Ok((rest, n)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected number, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn txt(ts: Toks) -> Res<CharList> {
    match I9nInput::split_first(&ts).map(|(x, xs)| (x.clone().value(), xs)) {
        Some((Tok::Txt(s), rest)) => Ok((rest, s)),
        Some((t, _)) => Err(nom::Err::Error(Error::with_message(
            ts,
            format!("Expected text literal, found {t:?}"),
        ))),
        None => Err(eof_error(ts)),
    }
}

fn attr(ts: Toks) -> Res<(Sym, RcTm)> {
    alt((
        // [name Value]
        tuple((
            sym,
            alt((
                delimited(
                    nom_i9n::begin_block,
                    nom_i9n::on_new_line(non_block_tm),
                    nom_i9n::end_block,
                ),
                tm,
            )),
        )),
        // [AttrVarSameName] or [AttrVarSameName.3] or [AttrVarSameName.New]
        var.map(|v| {
            // `lower` created on separate line so INTERNER isn't shared AND mutated.
            let lower = format!("{}", heck::AsSnakeCase(&v.name.to_str()[..]));
            (lower.into(), Tm::Var(v))
        }),
        // [attr_sym_same_name]
        sym.map(|s| (s, Tm::Sym(s))),
    ))
    .map(|(sym, tm)| (sym, tm.into()))
    .parse(ts)
}

fn rel(ts: Toks) -> Res<Rel> {
    let (ts, _) = tok(OBrack).parse(ts)?;
    let (ts, attrs) = context(
        "relation attributes",
        cut(separated_list1(
            tok(COBrack),
            context("relation attribute", cut(attr)),
        )),
    )
    .parse(ts)?;

    let (ts, _) = context("closing bracket for relation", cut(tok(CBrack))).parse(ts)?;
    let map = attrs.into_iter().collect::<Map<_, _>>();
    Ok((ts, map))
}

fn block_member(ts: Toks) -> Res<(Tok, Tm)> {
    context(
        "block member",
        tuple((alt((tok(Dash), tok(Pipe))).map(At::value), tm)),
    )
    .parse(ts)
}

fn block(ts: Toks) -> Res<Tm> {
    let (ts, pairs) = context(
        "block",
        nom_i9n::indented_block(&mut nom_i9n::on_new_line(block_member)),
    )
    .parse(ts)?;

    let first_functor = pairs[0].0.clone();
    let members = pairs
        .into_iter()
        .map(|(functor, tm)| {
            if functor == first_functor {
                Ok(tm.into())
            } else {
                Err(nom::Err::Error(Error::with_message(
                    ts.clone(),
                    "Block functor mismatch".to_owned(),
                )))
            }
        })
        .collect::<Result<Vector<_>, nom::Err<_>>>()?;

    Ok((ts, Tm::Block(first_functor, members)))
}

fn list(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(OBrace).parse(ts)?;

    if let Ok((ts, _)) = tok(CBrace).parse(ts.clone()) {
        return Ok((ts, Tm::Nil));
    }

    // We now know there's at least one element in the list.
    let (ts, first) = tm(ts)?;

    let mut ts = ts;
    let mut xs_reversed = vec![first];

    let (ts, spread_element) = loop {
        if let Ok((ts, _)) = tok(Spread).parse(ts.clone()) {
            let (ts, spread_elemnt) = tm.parse(ts)?;
            let (ts, _) = tok(CBrace).parse(ts)?;
            break (ts, spread_elemnt);
        }

        if let Ok((ts, _)) = tok(CBrace).parse(ts.clone()) {
            break (ts, Tm::Nil);
        }

        let (new_ts, element) = tm.parse(ts)?;
        xs_reversed.push(element);
        ts = new_ts;
    };

    let xs = xs_reversed
        .into_iter()
        .rfold(spread_element, |tail, x| Tm::Cons(x.into(), tail.into()));

    Ok((ts, xs))
}

#[allow(unused)]
fn left_associative_binop(
    bin_op_token: Tok,
    bin_op_symbol: BinOpSymbol,
) -> impl Fn(Toks) -> Res<Tm> {
    move |ts| {
        let (ts, (first, rest)) = tuple((
            non_operator_tm,
            many1(preceded(tok(bin_op_token.clone()), non_operator_tm)),
        ))
        .parse(ts)?;

        let output = rest.into_iter().fold(first, |so_far, next| {
            Tm::BinOp(bin_op_symbol, so_far.into(), next.into())
        });

        Ok((ts, output))
    }
}

fn right_associative_binop(
    bin_op_token: Tok,
    bin_op_symbol: BinOpSymbol,
) -> impl Fn(Toks) -> Res<Tm> {
    move |ts| {
        let (ts, (first, mut rest)) = tuple((
            non_operator_tm,
            many1(preceded(tok(bin_op_token.clone()), non_operator_tm)),
        ))
        .parse(ts)?;

        let mut terms = vec![first];
        terms.append(&mut rest);

        let output = terms
            .into_iter()
            .reduce(|so_far, next| Tm::BinOp(bin_op_symbol, so_far.into(), next.into()))
            .expect("At least 2 items since its a binop");

        Ok((ts, output))
    }
}

fn non_operator_tm(ts: Toks) -> Res<Tm> {
    alt((non_operator_non_block_tm, block)).parse(ts)
}

fn non_block_tm(ts: Toks) -> Res<Tm> {
    alt((
        operator_tm,
        parenthesized_tm,
        sym.map(Tm::Sym),
        var.map(Tm::Var),
        num.map(Tm::Int),
        txt.map(|t| Tm::Txt(t, Tm::Nil.into())),
        rel.map(Tm::Rel),
        list,
    ))
    .parse(ts)
}

fn non_operator_non_block_tm(ts: Toks) -> Res<Tm> {
    alt((
        parenthesized_tm,
        sym.map(Tm::Sym),
        var.map(Tm::Var),
        num.map(Tm::Int),
        txt.map(|t| Tm::Txt(t, Tm::Nil.into())),
        rel.map(Tm::Rel),
        list,
    ))
    .parse(ts)
}

fn parenthesized_tm(ts: Toks) -> Res<Tm> {
    let (ts, _) = tok(OParen).parse(ts)?;
    let (ts, t) = context(
        "parenthesized term",
        cut(alt((
            nom_i9n::on_new_line(delimited(
                nom_i9n::begin_block,
                nom_i9n::on_new_line(non_block_tm),
                nom_i9n::end_block,
            )),
            tm,
        ))),
    )
    .parse(ts)?;
    let (ts, _) = context("closing paren of parenthetical", cut(tok(CParen))).parse(ts)?;
    Ok((ts, t))
}

fn operator_tm(ts: Toks) -> Res<Tm> {
    context(
        "operator term",
        alt((
            right_associative_binop(Tok::Semicolon, BinOpSymbol::Semicolon),
            right_associative_binop(Tok::Equal, BinOpSymbol::Equal),
            right_associative_binop(Tok::Tilde, BinOpSymbol::Tilde),
            right_associative_binop(Tok::PathSep, BinOpSymbol::PathSep),
        )),
    )
    .parse(ts)
}

pub fn tm(ts: Toks) -> Res<Tm> {
    context("term", alt((operator_tm, non_operator_tm))).parse(ts)
}

fn rel_def(ts: Toks) -> Res<Item> {
    let (ts, r) = context("relation definition", nom_i9n::on_new_line(rel)).parse(ts)?;
    let (ts, b) =
        opt(alt((block, nom_i9n::indented(&mut non_block_tm))).map(RcTm::from)).parse(ts)?;
    Ok((ts, Item::RelDef(r, b)))
}

fn directive(ts: Toks) -> Res<Item> {
    let mut p = context(
        "directive",
        nom_i9n::on_new_line(tuple((
            tok(OBrack),
            rel,
            context("closing bracket of directive", cut(tok(CBrack))),
        ))),
    );
    let (ts, (_, r, _)) = p.parse(ts)?;
    Ok((ts, Item::Directive(r)))
}

pub fn item(ts: Toks) -> Res<Item> {
    alt((directive, rel_def))(ts)
}

pub fn module(ts: Toks) -> Res<Module> {
    many0(item)
        .map(|items| {
            let mut m = Module::default();
            for item in items {
                m.items.push(item);
            }
            m
        })
        .parse(ts)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::tm_displayer::TmDisplayer, lex::tokenize, utils::my_nom::Span};
    use pretty_assertions::assert_eq;
    use rpds::vector;

    #[track_caller]
    fn parse_to_tm(src: &'static str) -> Tm {
        let tokens = tokenize(Span::from(src), "blah.rellog".into()).unwrap();
        let (rest, t) = tm.parse(tokens[..].into()).unwrap();

        assert!(
            rest.is_empty(),
            "\nCould not parse entire input: ```\n{src}\n```\n\
             Remaining input begins with: [{}]\n",
            rest.iter()
                .take(5)
                .map(|tok| format!("`{}`", tok.as_ref()))
                .collect::<Vec<_>>()
                .join(", "),
        );

        t
    }

    #[test]
    fn simple_nested_relation_tm() {
        crate::init_interner();
        let src = r#"[goal [List][Pred]][initial Sublist][final empty_list]"#;
        let actual = parse_to_tm(src);

        let expected = Tm::Rel(
            vec![
                (
                    "goal".into(),
                    Tm::Rel(
                        vec![
                            (
                                "list".into(),
                                Tm::Var(Var::from_source("List", None)).into(),
                            ),
                            (
                                "pred".into(),
                                Tm::Var(Var::from_source("Pred", None)).into(),
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    )
                    .into(),
                ),
                (
                    "initial".into(),
                    Tm::Var(Var::from_source("Sublist", None)).into(),
                ),
                ("final".into(), Tm::Sym("empty_list".into()).into()),
            ]
            .into_iter()
            .collect(),
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn simple_block() {
        crate::init_interner();
        let src = "
    - [Blah]
    - [Blah]
    - [Blah]\n";

        let actual = parse_to_tm(src);

        let blah: RcTm = Tm::Rel(
            vec![(
                "blah".into(),
                Tm::Var(Var::from_source("Blah", None)).into(),
            )]
            .into_iter()
            .collect(),
        )
        .into();

        let expected = Tm::Block(Dash, vector![blah.clone(), blah.clone(), blah.clone()]);

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn nested_block() {
        crate::init_interner();
        let src = "
    - [Blah]
    -
        | [Blah]
        | [Blah]
    - [Blah]\n";

        let actual = parse_to_tm(src);

        let blah: RcTm = Tm::Rel(
            vec![(
                "blah".into(),
                Tm::Var(Var::from_source("Blah", None)).into(),
            )]
            .into_iter()
            .collect(),
        )
        .into();

        let expected = Tm::Block(
            Dash,
            vector![
                blah.clone(),
                Tm::Block(Pipe, vector![blah.clone(), blah.clone()]).into(),
                blah.clone()
            ],
        );

        assert_eq!(actual, expected, "Input was {:?}", src);
    }

    #[test]
    fn empty_list() {
        crate::init_interner();
        assert_eq!(parse_to_tm("{}"), Tm::Nil);
    }

    #[test]
    fn singleton_list() {
        crate::init_interner();
        assert_eq!(
            parse_to_tm("{123}"),
            Tm::Cons(Tm::Int(123.into()).into(), Tm::Nil.into())
        );
    }

    #[test]
    fn multi_element_list() {
        crate::init_interner();
        let src = r#"{123 "asdf" {} [aardvark][Zebra] socrates Unknown {1 2 3}}"#;

        let formatted = TmDisplayer::default()
            .with_tm(&parse_to_tm(src))
            .to_string();

        assert_eq!(formatted, src);
    }

    #[test]
    fn list_with_spread() {
        crate::init_interner();
        let src = r#"{X ...Xs}"#;
        let tm = parse_to_tm(src);
        let formatted = TmDisplayer::default().with_tm(&tm).to_string();

        assert_eq!(formatted, src);
    }
}
