use std::{collections::HashSet, fmt, iter, ops::Deref, path::PathBuf, rc::Rc};

use heck::ToPascalCase;
use nom_locate::LocatedSpan;
use rpds::{vector, Vector};
use unifier_set::{ClassifyTerm, DirectChildren, TermKind};

use crate::{
    ast::dup::{Dup, TmDuplicator},
    ast::tm_displayer::TmDisplayer,
    data_structures::{Generation, Int, Map, Sym, Var},
    interner::IStr,
    lex::{
        self,
        tok::{At, Tok},
    },
    parse::{self, Error},
};

use super::partial_txt::{PartialTxt, PartialTxtErr};

pub type Rel = Map<Sym, RcTm>;

/// A term.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Tm {
    Sym(Sym),
    Var(Var),
    Int(Int),
    Txt(PartialTxt),
    Block(Tok, Vector<RcTm>),
    Rel(Rel),
    BinOp(BinOpSymbol, RcTm, RcTm),
    Cons(RcTm, RcTm),
    Nil,
}

impl Dup for Tm {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        match self {
            Tm::Var(v) => Tm::Var(v.dup(duper)),
            Tm::Cons(h, t) => Tm::Cons(h.dup(duper), t.dup(duper)),
            Tm::Block(f, ms) => {
                let ms = ms.iter().map(|tm| tm.dup(duper)).collect();
                Tm::Block(f.clone(), ms)
            }
            Tm::Rel(rel) => {
                let rel = rel
                    .iter()
                    .map(|(name, tm)| (*name, tm.dup(duper)))
                    .collect();
                Tm::Rel(rel)
            }
            Tm::BinOp(op, x, y) => Tm::BinOp(*op, x.dup(duper), y.dup(duper)),
            Tm::Txt(txt) => {
                // Better way to do this? See note in
                // `<RcTm as DirectChildren>::map_direct_children`
                let new_tail = txt.segment_tail().dup(duper);
                let new_txt = PartialTxt::from_string_and_tail(txt.segment_as_str(), new_tail);
                Tm::Txt(new_txt)
            }
            Tm::Sym(_) | Tm::Int(_) | Tm::Nil => self.clone(),
        }
    }
}

impl Default for Tm {
    fn default() -> Self {
        Self::Nil
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RcTm(Rc<Tm>);

impl RcTm {
    pub fn try_as_list(&self) -> Option<(Vector<RcTm>, Option<Var>)> {
        let mut vec = vector![];
        let mut current = self;

        while let Tm::Cons(x, xs) = current.as_ref() {
            vec.push_back_mut(x.clone());
            current = xs;
        }

        match current.as_ref() {
            Tm::Nil => Some((vec, None)),
            Tm::Var(var) => Some((vec, Some(var.clone()))),
            _ => None,
        }
    }

    pub fn try_as_set_from_list(&self) -> Option<(HashSet<RcTm>, Option<Var>)> {
        let mut set = HashSet::new();
        let mut current = self;

        while let Tm::Cons(x, xs) = current.as_ref() {
            set.insert(x.clone());
            current = xs;
        }

        match current.as_ref() {
            Tm::Nil => Some((set, None)),
            Tm::Var(var) => Some((set, Some(var.clone()))),
            _ => None,
        }
    }

    pub fn try_as_var(&self) -> Option<Var> {
        match self.as_ref() {
            Tm::Var(v) => Some(v.clone()),
            _ => None,
        }
    }

    pub fn try_as_sym(&self) -> Option<Sym> {
        match self.as_ref() {
            Tm::Sym(s) => Some(*s),
            _ => None,
        }
    }

    pub fn try_as_rel(&self) -> Option<&Rel> {
        match self.as_ref() {
            Tm::Rel(r) => Some(r),
            _ => None,
        }
    }

    pub fn list_from_iter(it: impl DoubleEndedIterator<Item = RcTm>) -> Self {
        let mut list = Tm::Nil;

        for element in it.rev() {
            list = Tm::Cons(element, Self::from(list));
        }

        Self::from(list)
    }

    pub fn try_collect_txt_to_string(&self, buf: &mut String) -> Result<(), PartialTxtErr> {
        let mut rc_tm = self;
        loop {
            match rc_tm.as_ref() {
                Tm::Txt(txt) => {
                    buf.push_str(txt.segment_as_str());
                    rc_tm = txt.segment_tail();
                }
                Tm::Nil => return Ok(()),
                Tm::Var(var) => return Err(PartialTxtErr::UninstantiatedTail(var.clone())),
                Tm::Sym(..)
                | Tm::Int(..)
                | Tm::Block(..)
                | Tm::Rel(..)
                | Tm::BinOp(..)
                | Tm::Cons(..) => return Err(PartialTxtErr::NonTxtTail(rc_tm.clone())),
            }
        }
    }

    pub fn source_vars_to_repl_vars(&self) -> Self {
        self.map_direct_children(|child| match child.as_ref() {
            Tm::Var(v) if matches!(v.gen, Generation::Source) => {
                Tm::Var(Var::from_repl(v.name, v.suffix)).into()
            }
            Tm::Var(_) => child.clone(), // Prevent infinite recursion.
            _ => child.source_vars_to_repl_vars(),
        })
    }

    pub fn sym(s: impl AsRef<str>) -> Self {
        Tm::Sym(IStr::from(s.as_ref())).into()
    }

    pub fn sym_true() -> Self {
        Tm::Sym("true".into()).into()
    }

    pub fn sym_false() -> Self {
        Tm::Sym("false".into()).into()
    }

    pub fn rel_true() -> Self {
        Rel::new()
            .insert(Sym::from("true"), Self::sym_true())
            .into()
    }

    pub fn is_nil(&self) -> bool {
        matches!(self.as_ref(), Tm::Nil)
    }
}

#[macro_export]
macro_rules! tm {
    ($ident:ident) => {
        {
            let ident = stringify!($ident);
            match ident.chars().next().unwrap() {
                ch if ch.is_lowercase() => Tm::Sym(ident.into()),
                ch if ch.is_uppercase() => Tm::Var(Var::from_source(ident, None)),
                _ => todo!(),
            }
        }
    };

    ( $([$attr:ident])+ ) => {
        {
            let mut rel = $crate::ast::Rel::new();

            $(
                rel.insert_mut(stringify!($attr).into(), RcTm::from(tm!($attr)));
            )+

            Tm::Rel(rel)
        }
    };

    ( $([$attr:ident $expr:expr])+ ) => {
        {
            let mut rel = $crate::ast::Rel::new();

            $(
                rel.insert_mut(stringify!($attr).into(), $expr);
            )+

            Tm::Rel(rel)
        }
    };
}

#[macro_export]
macro_rules! rel_match {
    ($expr:expr, { $( $([$key:ident = $value:pat])+ $(| $([$key2:ident = $value2:ident])+)* => $block:expr, )* else => $default:expr }) => {
        loop {
            $(
                $(
                    let $key = $expr.get(&$crate::data_structures::Sym::from(stringify!($key)));
                )+
                if let ( $(Some($value),)+ ) = ( $($key, )+ ) {
                    break $block;
                }

                $(
                    $(
                        let $key2 = $expr.get(&IStr::from(stringify!($key2)));
                    )+
                    if let ( $(Some($value2),)+ ) = ( $($key2, )+ ) {
                        break $block;
                    }
                )*
            )*

            break $default;
        }
    };
}

impl Dup for RcTm {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        self.0.dup(duper).into()
    }
}

impl AsRef<Tm> for RcTm {
    fn as_ref(&self) -> &Tm {
        self.0.as_ref()
    }
}

impl Deref for RcTm {
    type Target = Tm;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl Default for RcTm {
    fn default() -> Self {
        Tm::default().into()
    }
}

impl<T: Into<Tm>> From<T> for RcTm {
    fn from(value: T) -> Self {
        RcTm(Rc::new(value.into()))
    }
}

impl From<Var> for RcTm {
    fn from(var: Var) -> Self {
        Tm::Var(var).into()
    }
}

impl From<Sig> for RcTm {
    fn from(sig: Sig) -> Self {
        Self(Rc::new(Tm::Rel(
            sig.0
                .into_iter()
                .map(|sym| (*sym, Tm::Sym(*sym).into()))
                .collect(),
        )))
    }
}

impl From<String> for RcTm {
    fn from(s: String) -> Self {
        Self(Rc::new(Tm::Txt(PartialTxt::from(s))))
    }
}

impl From<Rel> for RcTm {
    fn from(rel: Rel) -> Self {
        Self(Rc::new(Tm::Rel(rel)))
    }
}

impl From<&Tok> for RcTm {
    fn from(value: &Tok) -> Self {
        Self(Rc::new(Tm::Sym(value.to_string().into())))
    }
}

impl fmt::Display for RcTm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", TmDisplayer::default().with_tm(self.as_ref()))
    }
}

impl ClassifyTerm<Var> for RcTm {
    fn classify_term(&self) -> unifier_set::TermKind<&Var> {
        match self.as_ref() {
            Tm::Var(var) => TermKind::Var(var),
            _ => TermKind::NonVar,
        }
    }

    fn superficially_unifiable(&self, other: &Self) -> bool {
        match (self.as_ref(), other.as_ref()) {
            (Tm::Sym(s1), Tm::Sym(s2)) => s1 == s2,
            (Tm::Var(_), Tm::Var(_)) => true,
            (Tm::Int(n1), Tm::Int(n2)) => n1 == n2,
            (Tm::Txt(txt1), Tm::Txt(txt2)) => {
                // We don't *need* to walk the segments because this is supposed
                // to be a superficial check.
                let s1 = txt1.segment_as_str();
                let s2 = txt2.segment_as_str();
                let prefix_len = usize::min(s1.len(), s2.len());
                s1[..prefix_len] == s2[..prefix_len]
            }
            (Tm::Block(f1, _), Tm::Block(f2, _)) => f1 == f2,
            (Tm::Rel(r1), Tm::Rel(r2)) => r1.keys().eq(r2.keys()),
            (Tm::Cons(_, _), Tm::Cons(_, _)) => true,
            (Tm::Nil, Tm::Nil) => true,
            (Tm::BinOp(op1, _, _), Tm::BinOp(op2, _, _)) => op1 == op2,
            _ => false,
        }
    }
}

impl DirectChildren<Var> for RcTm {
    fn direct_children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a> {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Int(_) | Tm::Nil => Box::new(iter::empty()),
            Tm::Txt(txt) => Box::new(iter::once(txt.segment_tail())),
            Tm::Block(_, members) => Box::new(members.iter()),
            Tm::Rel(rel) => Box::new(rel.values()),
            Tm::BinOp(_, x, y) => Box::new([x, y].into_iter()),
            Tm::Cons(head, tail) => Box::new(iter::once(head).chain(iter::once(tail))),
        }
    }

    fn map_direct_children<'a>(&'a self, mut f: impl FnMut(&'a Self) -> Self + 'a) -> Self {
        match self.as_ref() {
            Tm::Sym(_) | Tm::Var(_) | Tm::Int(_) | Tm::Nil => self.clone(),
            Tm::Txt(txt) => {
                // Here we're only copying this segment's text content. Is this
                // the best way to do it?
                // Could we map the tail *first*, then see if it's still
                // "equivalent" and only copy the text segment if it's not?
                let new_tail = f(txt.segment_tail());
                let txt_clone = PartialTxt::from_string_and_tail(txt.segment_as_str(), new_tail);
                Tm::Txt(txt_clone).into()
            }
            Tm::Block(functor, members) => {
                Tm::Block(functor.clone(), members.iter().map(f).collect()).into()
            }
            Tm::Rel(rel) => Tm::Rel(rel.iter().map(|(k, v)| (*k, f(v))).collect()).into(),
            Tm::BinOp(op, x, y) => Tm::BinOp(*op, f(x), f(y)).into(),
            Tm::Cons(head, tail) => Tm::Cons(f(head), f(tail)).into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpSymbol {
    PathSep,
    Equal,
    Tilde,
    Semicolon,
}

impl BinOpSymbol {
    pub fn deference_lvl(&self) -> usize {
        DEFERENCE_TABLE
            .iter()
            .enumerate()
            .find(|(_, op)| **op == *self)
            .map(|(idx, _)| idx)
            .expect("All cases handled")
    }

    pub fn to_tok(&self) -> Tok {
        match self {
            BinOpSymbol::PathSep => Tok::PathSep,
            BinOpSymbol::Equal => Tok::Equal,
            BinOpSymbol::Tilde => Tok::Tilde,
            BinOpSymbol::Semicolon => Tok::Semicolon,
        }
    }
}

/// Operator deference is the inverse of operator precedence.
pub const DEFERENCE_TABLE: &[BinOpSymbol] = &[
    BinOpSymbol::PathSep, // Lowest deference (highest precedence).
    BinOpSymbol::Equal,
    BinOpSymbol::Tilde,
    BinOpSymbol::Semicolon, // Highest deference (lowest precedence).
];

impl From<BinOpSymbol> for Tm {
    fn from(value: BinOpSymbol) -> Self {
        Tm::Sym(IStr::from(value.to_string()))
    }
}

impl fmt::Display for BinOpSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            BinOpSymbol::Equal => "=",
            BinOpSymbol::Tilde => "~",
            BinOpSymbol::PathSep => "::",
            BinOpSymbol::Semicolon => ";",
        })
    }
}

/// A top-level item.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    /// A command to the compiler/runtime system.
    Directive(Rel),

    /// The definition of a relation.
    RelDef(Rel, Option<RcTm>),
}

impl fmt::Display for Item {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Item::Directive(rel) => {
                write!(f, "[")?;
                TmDisplayer::default().fmt_rel(rel, f)?;
                write!(f, "]")?;
                Ok(())
            }
            Item::RelDef(rel, body) => {
                TmDisplayer::default().fmt_rel(rel, f)?;
                if let Some(body) = body {
                    let td = TmDisplayer::default().indenting(body.as_ref());
                    write!(f, " {td}")?;
                }
                Ok(())
            }
        }
    }
}

/// Contains a *sorted* `Vector` of keys.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Sig(Vector<Sym>);

impl Sig {
    pub fn arity(&self) -> usize {
        self.0.len()
    }

    pub fn from_rel(rel: &Rel) -> Self {
        Self::from_iter(rel.keys().cloned())
    }
}

impl FromIterator<Sym> for Sig {
    fn from_iter<I: IntoIterator<Item = Sym>>(iter: I) -> Self {
        let mut v: Vec<_> = iter.into_iter().collect();
        v.sort();
        Self(v.into_iter().collect())
    }
}

impl<I> From<I> for Sig
where
    I: IntoIterator<Item = Sym>,
{
    fn from(it: I) -> Self {
        it.into_iter().collect()
    }
}

impl From<&Sig> for Tm {
    fn from(sig: &Sig) -> Self {
        let rel = sig
            .0
            .iter()
            .copied()
            .map(|sym| {
                let pascal = sym.to_str().to_pascal_case();
                (sym, RcTm::from(Tm::Var(Var::from_source(pascal, None))))
            })
            .collect();
        Tm::Rel(rel)
    }
}

impl fmt::Display for Sig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for key in &self.0 {
            write!(f, "[{}]", key.to_str().to_pascal_case())?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Clause {
    pub head: Rel,
    pub body: Option<RcTm>,
}

impl Dup for Clause {
    fn dup(&self, duper: &mut TmDuplicator) -> Self {
        let head = self
            .head
            .iter()
            .map(|(name, tm)| (*name, tm.dup(duper)))
            .collect();

        let body = self.body.as_ref().map(|body| body.dup(duper));

        Self { head, body }
    }
}

impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        TmDisplayer::default().fmt_rel(&self.head, f)?;
        if let Some(body) = &self.body {
            write!(f, "{}", TmDisplayer::default().indenting(body.as_ref()))?;
        }
        Ok(())
    }
}

/// A representation of the syntax tree of a single source file.
#[derive(Debug, Default, Clone)]
pub struct Module {
    /// It's important that the order of the definitions be kept at this point
    /// so that a macro invocation (directive) can be told about the `Item` that
    /// follows it.
    pub items: Vec<Item>,
}

impl Module {
    pub fn parse(
        src: impl AsRef<str>,
        filename: PathBuf,
        token_buf: &mut Vec<At<Tok>>,
    ) -> Result<Module, Error<'_>> {
        let tokens = lex::tokenize_into(token_buf, LocatedSpan::new(src.as_ref()), filename)?;
        parse::entire_module(tokens)
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for item in &self.items {
            writeln!(f, "{item}\n")?;
        }
        Ok(())
    }
}
