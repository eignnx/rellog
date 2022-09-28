use std::{collections::BTreeMap, iter};

use rpds::Vector;

use crate::{
    ast::{Rel, Sig},
    rt::{SolnStream, UnifierSet},
};

pub struct Intrinsic {
    signature: Sig,
    func: &'static dyn Fn(UnifierSet, Rel) -> Box<dyn SolnStream>,
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
        $intrs.def(&sig, &|$u, rel| {
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

    fn def(&mut self, sig: &[&str], func: &'static dyn Fn(UnifierSet, Rel) -> Box<dyn SolnStream>) {
        let sig: Sig = sig
            .into_iter()
            .map(|s| s.into())
            .collect::<Vector<_>>()
            .into();

        self.0.insert(
            sig.clone(),
            Intrinsic {
                signature: sig,
                func,
            },
        );
    }

    pub(crate) fn initialize() -> Self {
        let mut intrs = Self::new();

        def_intrinsic!(intrs, |u, [eq1][eq2]| {
            Box::new(u.unify(eq1, eq2).into_iter().map(Ok))
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
