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
                    let attrs: Vector<RcTm> = match attrs.try_as_list().unwrap() {
                        (vec, None) => vec,
                        (_vec, Some(_tail_var)) => todo!("What happens when list is partial?")
                    };

                    let rel: Rel = attrs.into_iter()
                        .map(|attr| match attr.as_ref() {
                            Tm::Rel(r) if r.size() == 1 => r.clone(),
                            Tm::Rel(_) => todo!("type error: only size-1 attributes accepted"),
                            Tm::Var(_) => todo!("throw instantiation error"),
                            _ => todo!("throw type error"),
                        })
                        .map(|attr| attr
                            .iter()
                            .map(|(k, v)| (k.clone(), v.clone()))
                            .next()
                            .expect("There's exactly one key-value pair in here"))
                        .collect();

                    let rel = RcTm::from(Tm::Rel(rel));

                    Box::new(u.unify(var, &rel).into_iter().map(Ok))
                }

                (Tm::Rel(rel), Tm::Var(_)) => {
                    let list = RcTm::list_from_iter(rel.iter().map(|(k, v)| Tm::Rel(Rel::new().insert(*k, v.clone())).into()));
                    Box::new(u.unify(attrs, &list).into_iter().map(Ok))
                }

                (Tm::Var(_), Tm::Var(_)) => todo!("throw instantiation error"),

                (_, _) => todo!("throw type error"),
            }
        });

        def_intrinsic!(intrs, |u, [attr][key][value]| {
            match (attr.as_ref(), key.as_ref(), value.as_ref()) {

                // [[mode [attr in][key inout][value inout]]]
                (Tm::Rel(attr), _, _) if attr.size() == 1 => {
                    let (k, v) = attr
                        .iter()
                        .map(|(k, v)| (Tm::Sym(*k).into(), v.clone()))
                        .next()
                        .expect("There's exactly one key-value pair in here");
                    let u_opt = u.unify(key, &k).and_then(|u| u.unify(value, &v));
                    Box::new(u_opt.into_iter().map(Ok))
                }

                // [[mode [attr inout][key in][value inout]]]
                (_, Tm::Sym(key), _) => {
                    let a = Tm::Rel(Rel::new().insert(*key, value.clone())).into();
                    Box::new(u.unify(attr, &a).into_iter().map(Ok))
                }

                (Tm::Var(_), Tm::Var(_), _) => todo!("instantiation error"),

                _ => todo!("type error"),
            }
            // match attr.as_ref() {
            //                 Tm::Rel(r) if r.size() == 1 => r.clone(),
            //                 Tm::Rel(_) => todo!("type error: only size-1 attributes accepted"),
            //                 Tm::Var(_) => todo!("throw instantiation error"),
            //                 _ => todo!("throw type error"),
            //             }

        });

        def_intrinsic!(intrs, |u, [is_var]| {
            match is_var.as_ref() {
                Tm::Var(_) => Box::new(iter::once(Ok(u))),
                _ => empty_soln_stream(),
            }
        });

        def_intrinsic!(intrs, |u, [rel][key][value]| {
            let rel = match rel.as_ref() {
                Tm::Rel(rel) => rel,
                Tm::Var(_) => todo!("instantiation error"),
                _ => todo!("type error"),
            };

            let key = match key.as_ref() {
                Tm::Sym(key) => key,
                Tm::Var(_) => todo!("instantiation error"),
                _ => todo!("type error"),
            };

            if let Some(found) = rel.get(&key) {
                Box::new(u.unify(value, found).into_iter().map(Ok))
            }else{
                empty_soln_stream()
            }
        });

        def_intrinsic!(intrs, |u, [txt_prefix][txt_suffix][txt_compound]| {
            match (txt_prefix.as_ref(), txt_suffix.as_ref()) {
                (Tm::Txt(prefix), Tm::Txt(suffix)) => {
                    let compound = prefix.to_owned() + suffix;
                    let compound = Tm::Txt(compound).into();
                    Box::new(u.unify(txt_compound, &compound).into_iter().map(Ok))
                }
                _ => todo!("only mode supported: [[mode txt.[prefix in][suffix in][compound out]]]"),
            }
        });

        def_intrinsic!(intrs, |u, [yes]| {
            if yes.as_ref() == &tm!(yes) {
                Box::new(iter::once(Ok(u)))
            } else {
                empty_soln_stream()
            }
        });

        def_intrinsic!(intrs, |u, [no]| {
            if no.as_ref() == &tm!(no) {
                empty_soln_stream()
            } else {
                Box::new(iter::once(Ok(u)))
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
        self.0.get(&rel.clone().into())
    }
}

fn empty_soln_stream() -> Box<dyn SolnStream> {
    Box::new(iter::empty())
}
