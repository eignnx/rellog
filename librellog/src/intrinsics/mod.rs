use std::{collections::BTreeMap, iter};

use rpds::Vector;

use crate::{
    ast::ast::{RcTm, Rel, Sig, Tm},
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
        $intrs.def(&sig, move |u: UnifierSet, rel: Rel| {
            $(
            let $name = match rel.get(&stringify!($name).into()) {
                Some(x) => u.reify_term(x),
                None => return soln_stream::failure(),
            };
            let $name = &$name;
            )+

            let $u = u;

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
            soln_stream::unifying(u, eq1, eq2)
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

                    soln_stream::unifying(u, var, &rel)
                }

                (Tm::Rel(rel), Tm::Var(_)) => {
                    let list = RcTm::list_from_iter(rel.iter().map(|(k, v)| Tm::Rel(Rel::new().insert(*k, v.clone())).into()));
                    soln_stream::unifying(u, attrs, &list)
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

                (Tm::Rel(_non_attr_rel), _, _) => {
                    // Is this good advice?
                    todo!("type error: use [rel][attr] instead")
                }

                // [[mode [attr inout][key in][value inout]]]
                (_, Tm::Sym(key), _) => {
                    let a = Tm::Rel(Rel::new().insert(*key, value.clone())).into();
                    soln_stream::unifying(u, attr, &a)
                }

                (Tm::Var(_), Tm::Var(_), _) => todo!("instantiation error"),

                _ => todo!("type error"),
            }
        });

        def_intrinsic!(intrs, |u, [gt][lt]| {
            match (gt.as_ref(), lt.as_ref()) {
                (Tm::Num(gt), Tm::Num(lt)) => {
                    if gt > lt {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => todo!("type error: expected two numbers")
            }
        });

        def_intrinsic!(intrs, |u, [gte][lte]| {
            match (gte.as_ref(), lte.as_ref()) {
                (Tm::Num(gte), Tm::Num(lte)) => {
                    if gte >= lte {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => todo!("type error: expected two numbers")
            }
        });

        def_intrinsic!(intrs, |u, [is_var]| {
            match u.reify_term(is_var).as_ref() {
                Tm::Var(_) => soln_stream::success(u),
                _ => soln_stream::failure(),
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
                soln_stream::unifying(u, value, found)
            }else{
                soln_stream::failure()
            }
        });

        def_intrinsic!(intrs, |u, [txt_prefix][txt_suffix][txt_compound]| {
            match (txt_prefix.as_ref(), txt_suffix.as_ref()) {
                (Tm::Txt(prefix), Tm::Txt(suffix)) => {
                    let compound = prefix.to_owned() + suffix;
                    let compound = Tm::Txt(compound).into();
                    soln_stream::unifying(u, txt_compound, &compound)
                }
                _ => todo!("only mode supported: [[mode txt.[prefix in][suffix in][compound out]]]"),
            }
        });

        def_intrinsic!(intrs, |u, [pred][succ]| {
            match (pred.as_ref(), succ.as_ref()) {
                (Tm::Var(_), Tm::Var(_)) => todo!("instantiation error"),
                (Tm::Var(_), Tm::Num(s)) => {
                    let p = Tm::Num(s - 1).into();
                    soln_stream::unifying(u, pred, &p)
                }
                (Tm::Num(p), Tm::Var(_)) => {
                    let s = Tm::Num(p + 1).into();
                    soln_stream::unifying(u, succ, &s)
                }
                (Tm::Num(p), Tm::Num(s)) => {
                    if p + 1 == *s {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => todo!("type error"),
            }
        });

        def_intrinsic!(intrs, |u, [yes]| {
            if yes.as_ref() == &tm!(yes) {
                soln_stream::success(u)
            } else {
                soln_stream::failure()
            }
        });

        def_intrinsic!(intrs, |u, [no]| {
            if no.as_ref() == &tm!(no) {
                soln_stream::failure()
            } else {
                soln_stream::success(u)
            }
        });

        def_intrinsic!(intrs, |u, [io_writeln]| {
            let arg = io_writeln;
            match arg.as_ref() {
                Tm::Txt(txt) => println!("{}", txt),
                _ => todo!("type error"),
            }
            soln_stream::success(u)
        });

        def_intrinsic!(intrs, |u, [term][text]| {
            match (term.as_ref(), text.as_ref()) {
                (Tm::Var(_), _) => todo!("instantiation error"),
                (_, Tm::Var(_) | Tm::Txt(_)) => {
                    let term = Tm::Txt(term.to_string()).into();
                    soln_stream::unifying(u, &term, text)
                }
                _ => todo!("type error")
            }
        });

        ////////////////////// Define `[builtins]` //////////////////////

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
            soln_stream::unifying(u, builtins, &builtin_rel_sigs)
        });

        intrs
    }

    pub(crate) fn index_match(&self, rel: &Rel) -> Option<&Intrinsic> {
        self.0.get(&rel.clone().into())
    }
}

mod soln_stream {
    use super::*;

    pub fn success(u: UnifierSet) -> Box<dyn SolnStream> {
        Box::new(iter::once(Ok(u)))
    }
    pub fn failure() -> Box<dyn SolnStream> {
        Box::new(iter::empty())
    }

    pub fn unifying(u: UnifierSet, x: &RcTm, y: &RcTm) -> Box<dyn SolnStream> {
        Box::new(u.unify(x, y).into_iter().map(Ok))
    }
}
