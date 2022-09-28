use std::{collections::BTreeMap, iter};

use rpds::Vector;

use crate::{
    ast::{RcTm, Rel, Sig, Tm},
    rt::{SolnStream, UnifierSet},
    tm,
};

pub struct Intrinsic {
    signature: Sig,
    func: Box<dyn Fn(UnifierSet, Rel) -> Box<dyn SolnStream>>,
}

impl Intrinsic {
    pub fn apply(&self, u: UnifierSet, rel: Rel) -> Box<dyn SolnStream> {
        if self.signature != rel.clone().into() {
            return Box::new(iter::empty());
        }

        (self.func)(u, rel)
    }
}

macro_rules! def_intrinsic {
    ($intrs:expr, |$u:ident, $([$name:ident])+| $body:expr) => {
        let sig = [$(stringify!($name),)+];
        $intrs.def(&sig, move |$u, rel| {
            $(
            let $name = match rel.get(&stringify!($name).into()) {
                Some(x) => x,
                None => return empty_soln_stream(),
            };
            )+

            $body
        });
    };
}

pub struct IntrinsicsMap(BTreeMap<Sig, Intrinsic>);

impl IntrinsicsMap {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Sig, &Intrinsic)> {
        self.0.iter()
    }

    fn def(
        &mut self,
        sig: &[&str],
        func: impl Fn(UnifierSet, Rel) -> Box<dyn SolnStream> + 'static,
    ) {
        let sig: Sig = sig
            .into_iter()
            .map(|s| s.into())
            .collect::<Vector<_>>()
            .into();

        self.0.insert(
            sig.clone(),
            Intrinsic {
                signature: sig,
                func: Box::new(func),
            },
        );
    }

    pub(crate) fn initialize() -> Self {
        let mut intrs = Self::new();

        def_intrinsic!(intrs, |u, [eq1][eq2]| {
            Box::new(u.unify(eq1, eq2).into_iter().map(Ok))
        });

        def_intrinsic!(intrs, |u, [rel][attrs]| {
            match (rel.as_ref(), attrs.as_ref()) {
                (Tm::Var(_), Tm::Cons(_, _)) => {
                    let var = rel;
                    let attrs: Vector<RcTm> = match attrs.as_list().unwrap() {
                        (vec, None) => vec,
                        (_vec, Some(_tail_var)) => todo!("What happpens when list is partial?")
                    };

                    let rel: Rel = attrs.into_iter()
                        .map(|attr| match attr.as_ref() {
                            Tm::Sym(s) => s.clone(),
                            Tm::Var(_) => todo!("throw instantiation error"),
                            _ => todo!("throw type error"),
                        })
                        .map(|sym| (sym, Tm::Var(sym.into()).into()))
                        .collect();

                    let rel = RcTm::from(Tm::Rel(rel));

                    Box::new(u.unify(var, &rel).into_iter().map(Ok))
                }
                (Tm::Var(_), Tm::Var(_)) => todo!("throw instantiation error"),
                (_, _) => todo!("throw type error"),
            }
        });

        let builtin_rel_sigs = {
            let mut builtin_rel_sigs = intrs
                .iter()
                .map(|(sig, _)| {
                    let sig_tm: Tm = sig.into();
                    RcTm::from(sig_tm)
                })
                .collect::<Vec<_>>();
            builtin_rel_sigs.push(RcTm::from(tm!([builtins])));
            RcTm::list_from_iter(builtin_rel_sigs.into_iter())
        };

        def_intrinsic!(intrs, |u, [builtins]| {
            let builtin_rel_sigs = builtin_rel_sigs.clone();
            Box::new(u.unify(builtins, &builtin_rel_sigs).into_iter().map(Ok))
        });

        intrs
    }

    pub(crate) fn index_match(&self, rel: &Rel) -> Option<&Intrinsic> {
        self.0.get(&rel.into())
    }
}

fn empty_soln_stream() -> Box<dyn SolnStream> {
    Box::new(iter::empty())
}
