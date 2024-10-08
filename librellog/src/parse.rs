use std::{
    cell::RefCell,
    fmt::{self, Display, Formatter},
    path::{Path, PathBuf},
};

use nom::{
    branch::alt,
    combinator::{all_consuming, cut, opt},
    error::{context, ContextError, ErrorKind, ParseError},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, preceded, tuple},
    Finish, IResult, Parser,
};
use nom_i9n::{I9nError, I9nErrorCtx, I9nErrorSituation, I9nInput, TokenizedInput};
use rpds::Vector;

use crate::{
    ast::{BinOpSymbol, Item, Module, RcTm, Rel, Tm},
    data_structures::{Int, Map, Sym, Var},
    lex::{
        tok::{
            At,
            Tok::{self, *},
        },
        LexError,
    },
};

use self::txt_tmpl::txt;

#[cfg(test)]
mod tests;
mod txt_tmpl;

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
        let fname_borrow = self.fname.borrow();
        let binding = PathBuf::from("<unknown_src>");
        let fname = fname_borrow.as_ref().unwrap_or(&binding);

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

pub fn entire_module(ts: Toks, fname: Option<impl AsRef<Path>>) -> Result<Module, Error> {
    let (_ts, m) = all_consuming(module)(ts).finish().map_err(|e| {
        *e.fname.borrow_mut() = fname.map(|p| p.as_ref().into());
        e
    })?;
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

#[macro_export]
macro_rules! expect_token {
    ($ts:expr, $p:pat => $e:expr, $t:pat => $err_msg:expr) => {
        match ::nom_i9n::I9nInput::split_first(&$ts).map(|(x, xs)| (x.clone().value(), xs)) {
            Some(($p, i)) => Ok((i, $e)),
            Some(($t, _)) => Err(nom::Err::Error(Error::with_message($ts, format!($err_msg)))),
            None => Err($crate::parse::eof_error($ts)),
        }
    };
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
    expect_token!(
        ts,
        Tok::Sym(s) => s,
        t => "Expected symbol, found {t:?}"
    )
}

fn var(ts: Toks) -> Res<Var> {
    expect_token!(
        ts,
        Tok::Var(v) => v,
        t => "Expected variable, found {t:?}"
    )
}

fn num(ts: Toks) -> Res<Int> {
    expect_token!(
        ts,
        Tok::Int(n) => n,
        t => "Expected number, found {t:?}"
    )
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
        // [AttrVarSameName] or [AttrVarSameName.3] or [AttrVarSameName.new]
        var.map(|v| {
            // `lower` created on separate line so INTERNER isn't shared AND mutated.
            let name = v.name.to_str().to_string();
            let lower = Sym::from(format!("{}", heck::AsSnakeCase(name)).as_ref());
            (lower, Tm::Var(v))
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

        if ts.is_empty() {
            return Err(nom::Err::Error(Error::with_message(
                ts,
                "Expected spread or closing brace".to_owned(),
            )));
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

        let mut all = rest.into_iter().rev().chain(std::iter::once(first));

        let mut output = all.next().expect("At least 1 item since its a binop");

        for next in all {
            output = Tm::BinOp(bin_op_symbol, next.into(), output.into());
        }

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
        txt,
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
        txt,
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
            left_associative_binop(Tok::PathSep, BinOpSymbol::PathSep),
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
