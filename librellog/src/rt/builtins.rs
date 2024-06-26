use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    io::{stderr, stdout, Write},
};

use nu_ansi_term::Color;
use num::{Integer, ToPrimitive, Zero};
use rpds::Vector;
use unifier_set::DirectChildren;

use crate::{
    ast::{
        dup::{Dup, TmDuplicator},
        txt::{Segment, TxtErr},
        Clause, RcTm, Rel, Sig, Tm,
    },
    data_structures::{Int, Sym, Var},
    lex::{self, tok::Tok},
    parse,
    rt::{
        kb::KnowledgeBase,
        soln_stream::{self, SolnStream},
        Err, UnifierSet,
    },
    tm,
    utils::int_counter::IntCounter,
};

use super::Rt;

mod txt_append;

pub struct StateForBuiltin<'a> {
    rt: &'a Rt,
    td: &'a RefCell<TmDuplicator>,
}

type BuiltinMethod = Box<
    dyn for<'state> Fn(
            &mut StateForBuiltin<'state>,
            UnifierSet,
            Rel,
        ) -> Box<dyn SolnStream + 'state>
        + 'static,
>;

pub struct Builtin {
    signature: Sig,
    method: BuiltinMethod,
}

impl Builtin {
    pub fn apply<'arg>(
        &self,
        rt: &'arg Rt,
        td: &'arg RefCell<TmDuplicator>,
        u: UnifierSet,
        rel: Rel,
    ) -> Box<dyn SolnStream + 'arg> {
        if self.signature != rel.keys().cloned().collect() {
            return soln_stream::failure();
        }

        let mut state = StateForBuiltin { rt, td };

        (self.method)(&mut state, u, rel)
    }
}

macro_rules! name_of_binding {
    ($ident:ident as $name:literal) => {
        $name
    };

    ($ident:ident) => {
        stringify!($ident)
    };
}

macro_rules! ident_of_binding {
    ($ident:ident as $name:literal) => {
        $ident
    };

    ($ident:ident) => {
        $ident
    };
}

macro_rules! def_builtin {
    ($intrs:expr, |$state:ident, $u:ident, $([$ident:ident $(as $name:literal)?])+ as $rel:ident| $body:expr) => {
        let sig = [$(name_of_binding!($ident $(as $name)?),)+];
        $intrs.def(&sig, move |state, u, rel| {
            $(
            let ident_of_binding!($ident $(as $name)?) = match rel.get(&name_of_binding!($ident $(as $name)?).into()) {
                Some(x) => u.reify_term(x),
                None => return soln_stream::failure(),
            };
            let ident_of_binding!($ident $(as $name)?) = &ident_of_binding!($ident $(as $name)?);
            )+

            let $state = state;
            let $u = u;
            let $rel = RcTm::from(rel);

            $body
        });
    };
}

pub struct BuiltinsMap(BTreeMap<Sig, Builtin>);

impl Debug for BuiltinsMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "BuiltinsMap(")?;
        for sig in self.0.keys() {
            writeln!(f, "\t{sig},")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

impl BuiltinsMap {
    fn new() -> Self {
        Self(BTreeMap::new())
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Sig, &Builtin)> {
        self.0.iter()
    }

    fn def(
        &mut self,
        sig: &[&str],
        func: impl for<'state> Fn(
                &mut StateForBuiltin<'state>,
                UnifierSet,
                Rel,
            ) -> Box<dyn SolnStream + 'state>
            + 'static,
    ) {
        let sig: Sig = sig.iter().map(|s| Sym::from(*s)).collect();

        self.0.insert(
            sig.clone(),
            Builtin {
                signature: sig,
                method: Box::new(func),
            },
        );
    }

    pub(crate) fn initialize() -> Self {
        let mut intrs = Self::new();

        def_builtin!(intrs, |_rt, u, [terms as "eq"] as _rel| {
            let Some((list, None)) = terms.try_as_list() else {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: "[eq]".into(),
                    key: "eq".into(),
                    expected_ty: "finite list".into(),
                    recieved_tm: terms.to_string()
                });
            };

            let mut it = list.into_iter();
            let Some(first) = it.next() else {
                return soln_stream::success(u);
            };

            let u = it.try_fold(u, |u, tm| {
                u.unify(first, tm)
            });

            if let Some(u) = u {
                soln_stream::success(u)
            } else {
                soln_stream::failure()
            }
        });

        def_builtin!(intrs, |_rt, u, [rel][attrs] as rel_called| {
            match (rel.as_ref(), attrs.as_ref()) {
                (Tm::Var(_), Tm::Cons(_, _)) => {
                    let var = rel;
                    let attrs: Vector<RcTm> = match attrs.try_as_list().unwrap() {
                        (vec, None) => vec,
                        (_vec, Some(_tail_var)) => return soln_stream::error(Err::InstantiationError {
                            rel: rel_called.to_string(),
                            tm: attrs.clone()
                        })
                    };

                    let rel: Result<Rel, Err> = attrs.into_iter()
                        .map(|attr| match attr.as_ref() {
                            Tm::Rel(r) if r.contains_key(&"key".into()) && r.contains_key(&"value".into()) => {
                                let key = r[&"key".into()].try_as_sym().ok_or_else(|| Err::ArgumentTypeError {
                                    rel: rel_called.to_string(),
                                    key: "attrs".into(),
                                    expected_ty: "[Key][Value]".into(),
                                    recieved_tm: attr.to_string(),
                                })?;
                                let value = r[&"value".into()].clone();
                                Ok((key, value))
                            }
                            Tm::Var(_) => Err(Err::InstantiationError {
                                rel: rel_called.to_string(),
                                tm: attr.clone(),
                            }),
                            _ => Err(Err::ArgumentTypeError {
                                rel: rel_called.to_string(),
                                key: "attrs".into(),
                                expected_ty: "size-1 attribute relation".into(),
                                recieved_tm: attr.to_string()
                            }),
                        })
                        .collect();

                    let Ok(rel) = rel else {
                        return soln_stream::error(rel.unwrap_err());
                    };

                    let rel = RcTm::from(Tm::Rel(rel));

                    soln_stream::unifying(u, var, &rel)
                }

                (Tm::Rel(rel), _) => {
                    let list = RcTm::list_from_iter(rel.iter().map(|(k, v)| {
                        tm!([key Tm::Sym(*k).into()][value v.clone()]).into()
                    }));
                    soln_stream::unifying(u, attrs, &list)
                }

                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError {
                    rel: rel_called.to_string(),
                    tm: rel.clone(),
                }.into(),

                (_, _) => Err::GenericError {
                    rel: rel_called.to_string(),
                    msg: "[rel][attrs] takes a relation and a list of `[Key][Value]` attributes".into()
                }.into(),
            }
        });

        def_builtin!(intrs, |_rt, u, [attr][key][value] as rel| {
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
                    soln_stream::error(Err::GenericError {
                        rel: rel.to_string(),
                        msg: "use [rel][attr] instead".into()
                    })
                }

                // [[mode [attr inout][key in][value inout]]]
                (_, Tm::Sym(key), _) => {
                    let a = Tm::Rel(Rel::new().insert(*key, value.clone())).into();
                    soln_stream::unifying(u, attr, &a)
                }

                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: key.clone()
                }.into(),

                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "bad arguments to [attr][key][value]".into()
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [rel][sig] as r| {
            match (rel.as_ref(), sig.as_ref()) {
                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError {
                    rel: r.to_string(),
                    tm: rel.clone()
                }.into(),

                (Tm::Rel(rel), Tm::Var(..)) => {
                    let sig_val = rel.iter().map(|(k, _)| {
                        let v = heck::ToPascalCase::to_pascal_case(&*k.to_str());
                        (*k, Tm::Var(Var::from_source(v.as_ref(), None)).into())
                    }).collect::<Rel>();
                    let sig_val = Tm::Rel(sig_val).into();
                    soln_stream::unifying(u, &sig_val, sig)
                }

                (Tm::Var(..), Tm::Rel(..))
                | (Tm::Rel(..), Tm::Rel(..)) => soln_stream::unifying(u, rel, sig),

                _ => Err::GenericError {
                    rel: r.to_string(),
                    msg: format!(
                        "[rel][sig] requires two rel terms, received `{}` and `{}`.",
                        rel,
                        sig,
                    )
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [gt][lt] as rel| {
            match (gt.as_ref(), lt.as_ref()) {
                (Tm::Int(gt), Tm::Int(lt)) => {
                    if gt > lt {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                (v @ Tm::Var(..), _) | (_, v @ Tm::Var(..)) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: RcTm::from(v.clone()),
                }.into(),
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[gt][lt] accepts two concrete numbers".into()
                }.into(),
            }
        });

        def_builtin!(intrs, |_rt, u, [gte][lte] as rel| {
            match (gte.as_ref(), lte.as_ref()) {
                (Tm::Int(gte), Tm::Int(lte)) => {
                    if gte >= lte {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                (v @ Tm::Var(..), _) | (_, v @ Tm::Var(..)) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: RcTm::from(v.clone()),
                }.into(),
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[gte][lte] accepts two concrete numbers".into()
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [tm as "must_be_var"] as _rel| {
            match u.reify_term(tm).as_ref() {
                Tm::Var(_) => soln_stream::success(u),
                _ => soln_stream::failure(),
            }
        });

        def_builtin!(intrs, |_rt, u, [tm as "must_be_num"] as rel| {
            match u.reify_term(tm).as_ref() {
                Tm::Int(..) => soln_stream::success(u),
                Tm::Var(..) => soln_stream::error(Err::InstantiationError{
                    rel: rel.to_string(),
                    tm: tm.clone()
                }),
                _ => soln_stream::failure(),
            }
        });

        def_builtin!(intrs, |_rt, u, [tm as "must_be_sym"] as rel| {
            match u.reify_term(tm).as_ref() {
                Tm::Sym(..) => soln_stream::success(u),
                Tm::Var(..) => soln_stream::error(Err::InstantiationError{
                    rel: rel.to_string(),
                    tm: tm.clone()
                }),
                _ => soln_stream::failure(),
            }
        });

        def_builtin!(intrs, |_rt, u, [tm as "must_be_txt"] as rel| {
            match u.reify_term(tm).as_ref() {
                Tm::TxtSeg(..) | Tm::TxtCons(..) | Tm::Nil => soln_stream::success(u),
                Tm::Var(..) => soln_stream::error(Err::InstantiationError{
                    rel: rel.to_string(),
                    tm: tm.clone()
                }),
                _ => soln_stream::failure(),
            }
        });

        def_builtin!(intrs, |_rt, u, [tm as "must_be_rel"] as rel_called| {
            match u.reify_term(tm).as_ref() {
                Tm::Rel(..) => soln_stream::success(u),
                Tm::Var(..) => soln_stream::error(Err::InstantiationError{
                    rel: rel_called.to_string(),
                    tm: tm.clone()
                }),
                _ => soln_stream::failure(),
            }
        });

        def_builtin!(intrs, |_rt, u, [rel][key][value] as rel_called| {
            let rel = match rel.as_ref() {
                Tm::Rel(rel) => rel,
                Tm::Var(_) => return Err::InstantiationError{
                    rel: rel_called.to_string(),
                    tm: rel.clone()
                }.into(),
                _ => return Err::GenericError {
                    rel: rel_called.to_string(),
                    msg: "[Rel][key][value] requires a relation for `Rel`.".into()
                }.into()
            };

            let key = match key.as_ref() {
                Tm::Sym(key) => key,
                Tm::Var(_) => return Err::InstantiationError{
                    rel: rel_called.to_string(),
                    tm: key.clone()
                }.into(),
                _ => return Err::GenericError {
                    rel: rel_called.to_string(),
                    msg: "[rel][Key][value] requires a ground term for `Key`.".into()
                }.into()
            };

            if let Some(found) = rel.get(key) {
                soln_stream::unifying(u, value, found)
            }else{
                soln_stream::failure()
            }
        });

        def_builtin!(intrs, |_state, u, [term][variables] as _rel| {
            let vars: BTreeSet<Var> = term.variables().collect();
            let vars = RcTm::list_from_iter(vars.into_iter().map(RcTm::from));
            soln_stream::unifying(u, &vars, variables)
        });

        def_builtin!(intrs, |state, u, [original][duplicate] as _rel| {
            let new = original.dup(&mut state.td.borrow_mut());
            soln_stream::unifying(u, &new, duplicate)
        });

        def_builtin!(intrs, |state, u, [original][duplicate][renaming][renamed] as rel| {
            let Some((renaming_set, None)) = renaming.try_as_set_from_list() else {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "renaming_vars".to_string(),
                    expected_ty: "list".to_string(),
                    recieved_tm: renaming.to_string(),
                });
            };

            let new = state.td.borrow_mut()
                .dup_conditionally(original, Box::new(move |var| {
                    renaming_set.contains(&RcTm::from(var))
                }));

            let Some((renaming_list, None)) = renaming.try_as_list() else {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "renaming_vars".to_string(),
                    expected_ty: "list".to_string(),
                    recieved_tm: renaming.to_string(),
                });
            };

            let Some(renaming_list_vars) = renaming_list.iter()
                .map(RcTm::try_as_var)
                .collect::<Option<Vec<_>>>() else {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "renaming_vars".to_string(),
                    expected_ty: "list of variables".to_string(),
                    recieved_tm: renaming.to_string(),
                });
            };

            let renamed_vec = renaming_list_vars.iter()
                .map(|var| {
                    let td_borrow = state.td.borrow();
                    td_borrow.substs().get(var).unwrap_or(var).clone()
                })
                .map(RcTm::from)
                .collect::<Vec<RcTm>>();

            let renamed_tm = RcTm::list_from_iter(renamed_vec.into_iter());

            let Some(u) = u.unify(renamed, &renamed_tm) else {
                return soln_stream::failure()
            };

            soln_stream::unifying(u, &new, duplicate)
        });

        def_builtin!(intrs, |_rt, u, [pascal_case][snake_case] as rel| {
            match (pascal_case.as_ref(), snake_case.as_ref()) {
                (Tm::Sym(pc), _) => {
                    let lower = format!("{}", heck::AsSnakeCase(&pc.to_str()[..]));
                    let lower_sym = Tm::Sym(Sym::from(lower.as_ref())).into();
                    soln_stream::unifying(u, &lower_sym, snake_case)
                }
                (_, Tm::Sym(sc)) => {
                    let lower = format!("{}", heck::AsPascalCase(&sc.to_str()[..]));
                    let lower_sym = Tm::Sym(Sym::from(lower.as_ref())).into();
                    soln_stream::unifying(u, &lower_sym, pascal_case)
                }
                _ => soln_stream::error(Err::GenericError {
                    rel: rel.to_string(),
                    msg: "`[snake_case][pascal_case]` requires either one or two \
                          ground symbols as arguments.".into()
                }),
            }
        });

        // def_builtin!(intrs, |_rt, u, [txt_car][txt_cdr][txt_cons] as rel| {
        //     let txt_car_opt = match txt_car.as_ref() {
        //         Tm::Sym(sym) => {
        //             let sym_str = sym.to_str();
        //             let mut it = sym_str.chars();
        //             let Some(first) = it.next() else {
        //                  return soln_stream::error(Err::ArgumentTypeError {
        //                     rel: rel.to_string(),
        //                     key: "txt_car".to_string(),
        //                     expected_ty: "single character symbol".to_string(),
        //                     recieved_tm: txt_car.to_string()
        //                 });
        //             };
        //             let None = it.next() else {
        //                  return soln_stream::error(Err::ArgumentTypeError {
        //                     rel: rel.to_string(),
        //                     key: "txt_car".to_string(),
        //                     expected_ty: "single character symbol".to_string(),
        //                     recieved_tm: txt_car.to_string()
        //                 });
        //             };
        //             Some(first)
        //         }
        //         Tm::Var(_) => None,
        //         _ => return soln_stream::error(Err::ArgumentTypeError {
        //             rel: rel.to_string(),
        //             key: "txt_car".to_string(),
        //             expected_ty: "single character symbol".to_string(),
        //             recieved_tm: txt_car.to_string()
        //         })
        //     };

        //     match (txt_car_opt, txt_cdr.as_ref(), txt_cons.as_ref()) {
        //         (_, _, Tm::Txt(txt)) => {
        //             match txt.car_cdr() {
        //                 Err(e) => soln_stream::error(e.into()),
        //                 Ok(None) => soln_stream::failure(),
        //                 Ok(Some((c, cs))) => {
        //                     let buf = String::from(c);
        //                     let c_sym = Tm::Sym(Sym::from(&buf)).into();
        //                     let Some(u) = u.unify(txt_car, &c_sym) else {
        //                         return soln_stream::failure();
        //                     };
        //                     let cs = Tm::Txt(cs).into();
        //                     soln_stream::unifying(u, txt_cdr, &cs)
        //                 }
        //             }
        //         }
        //         (Some(first), Tm::Txt(cdr), _) => {
        //             let new_cons = Tm::Txt(cdr.cons(first)).into();
        //             soln_stream::unifying(u, txt_cons, &new_cons)
        //         }

        //         (Some(first), Tm::Var(..), _) => {
        //             let new_txt = TxtBlock::from_string_and_tail(first, txt_cdr.clone());
        //             let new_txt = Tm::Txt(new_txt).into();
        //             soln_stream::unifying(u, txt_cons, &new_txt)
        //         }
        //         (None, Tm::Var(..), Tm::Var(..)) => {
        //             soln_stream::error(Err::InstantiationError {
        //                 rel: rel.to_string(),
        //                 tm: txt_cons.clone()
        //             })
        //         }
        //         _ => soln_stream::error(Err::GenericError {
        //             rel: rel.to_string(),
        //             msg:
        //                 "`[txt_car][txt_cdr][txt_cons]` requires either a symbol or a variable for \
        //                 `txt_car`, a text or variable for `txt_cdr`, and a text object or variable \
        //                 for `txt_cons`.".to_string()
        //         })
        //     }
        // });

        // def_builtin!(intrs, |_rt, u, [txt_prefix][txt_suffix][txt_compound] as rel| {
        //     txt_append::prefix_suffix_compound(&rel, u, txt_prefix, txt_suffix, txt_compound)
        // });

        def_builtin!(intrs, |_rt, u, [sum][x][y] as rel| {
            match (sum.as_ref(), x.as_ref(), y.as_ref()) {
                (_, Tm::Int(x), Tm::Int(y)) => {
                    let res = Tm::Int(x + y).into();
                    soln_stream::unifying(u, sum, &res)
                }
                // sum = X + y
                // <=>
                // X = sum - y
                (Tm::Int(sum), _, Tm::Int(y)) => {
                    let res = Tm::Int(sum - y).into();
                    soln_stream::unifying(u, x, &res)
                }
                // sum = x + Y
                // <=>
                // Y = sum - x
                (Tm::Int(sum), Tm::Int(x), _) => {
                    let res = Tm::Int(sum - x).into();
                    soln_stream::unifying(u, y, &res)
                }
                (_, Tm::Var(_), Tm::Var(_)) => Err::InstantiationError{
                    rel: rel.to_string(),
                    tm: x.clone(),
                }.into(),
                (Tm::Var(_), _, Tm::Var(_)) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: y.clone(),
                }.into(),
                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: x.clone(),
                }.into(),
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[sum][x][y] only relates numbers.".into()
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [product][x][y] as rel| {
            match (product.as_ref(), x.as_ref(), y.as_ref()) {
                (_, Tm::Int(x), Tm::Int(y)) => {
                    let res = Tm::Int(x * y).into();
                    soln_stream::unifying(u, product, &res)
                }
                // product = X * y
                // <=>
                // X = product / y
                (Tm::Int(product), _, Tm::Int(y)) => {
                    if y.is_zero() {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: "Division by zero required to solve query [product _][X][y 0].".into()
                        }.into();
                    }
                    let res = Tm::Int(product / y).into();
                    soln_stream::unifying(u, x, &res)
                }
                // product = x * Y
                // <=>
                // Y = product / x
                (Tm::Int(product), Tm::Int(x), _) => {
                    if x.is_zero() {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: "Division by zero required to solve query [product #][x 0][Y].".into()
                        }.into();
                    }
                    let res = Tm::Int(product / x).into();
                    soln_stream::unifying(u, y, &res)
                }
                (_, Tm::Var(_), Tm::Var(_)) => Err::InstantiationError{
                    rel: rel.to_string(),
                    tm: x.clone(),
                }.into(),
                (Tm::Var(_), _, Tm::Var(_)) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: y.clone(),
                }.into(),
                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: x.clone(),
                }.into(),
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[product][x][y] only relates numbers.".into()
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [difference][minuend][subtrahend] as rel| {
            match (difference.as_ref(), minuend.as_ref(), subtrahend.as_ref()) {
                (a, b, c) if ![a, b, c].into_iter().all(|tm| matches!(*tm, Tm::Int(_) | Tm::Var(_))) => {
                    Err::GenericError {
                        rel: rel.to_string(),
                        msg: "The arguments to [difference][minuend][subtrahend] must all be unifyable with integers.".into()
                    }.into()
                }
                // difference = minuend - subtrahend
                (_, Tm::Int(min), Tm::Int(sub)) => {
                    let diff = Tm::Int(min - sub).into();
                    soln_stream::unifying(u, difference, &diff)
                }
                // minuend = difference + subtrahend
                (Tm::Int(diff), _, Tm::Int(sub)) => {
                    let min = Tm::Int(diff + sub).into();
                    soln_stream::unifying(u, minuend, &min)
                }
                // subtrahend = minuend - difference
                (Tm::Int(diff), Tm::Int(min), _) => {
                    let sub = Tm::Int(min - diff).into();
                    soln_stream::unifying(u, subtrahend, &sub)
                }
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[difference][minuend][subtrahend] is not implemented for that mode.".into()
                }.into(),
            }
        });

        def_builtin!(intrs, |_rt, u, [quotient][remainder][numerator][denominator] as rel| {
            match (quotient.as_ref(), remainder.as_ref(), numerator.as_ref(), denominator.as_ref()) {
                (a, b, c, d) if ![a, b, c, d].into_iter().all(|tm| matches!(*tm, Tm::Int(_) | Tm::Var(_))) => {
                    Err::GenericError {
                        rel: rel.to_string(),
                        msg: "The arguments to [numerator][denominator][quotient][remainder] must all be unifyable with integers.".into()
                    }.into()
                }
                // quotient = numerator / denominator
                // remainder = numerator % denominator
                (_, _, Tm::Int(numer), Tm::Int(denom)) => {
                    if denom.is_zero() {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: "Division by zero required to solve query [numerator #][denominator 0][Quotient][Remainder].".into()
                        }.into();
                    }
                    let (q, r) = numer.div_mod_floor(denom);
                    u.unify(quotient, &Tm::Int(q).into()).and_then(|u| {
                        u.unify(remainder, &Tm::Int(r).into())
                    }).map(|u| {
                        soln_stream::success(u)
                    }).unwrap_or_else(soln_stream::failure)
                }
                // numerator = quotient * denominator + remainder
                (Tm::Int(quot), Tm::Int(rem), _, Tm::Int(denom)) => {
                    let numer = quot * denom + rem;
                    soln_stream::unifying(u, numerator, &Tm::Int(numer).into())
                }
                // denominator = (numerator - remainder) / quotient
                (Tm::Int(quot), Tm::Int(rem), Tm::Int(numer), _) => {
                    if quot.is_zero() {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: "Division by zero required to solve query [quotient 0][remainder #][numerator #][Denominator].".into()
                        }.into();
                    }
                    let denom = (numer - rem).div_floor(quot);
                    if denom.is_zero() {
                        return soln_stream::failure();
                    }
                    soln_stream::unifying(u, denominator, &Tm::Int(denom).into())
                }
                // forall quotient: int . numerator = quotient * denominator + remainder
                // (iterate through integer quotients)
                (_, Tm::Int(rem), _, Tm::Int(den)) => {
                    let numerator = numerator.clone();
                    let quotient = quotient.clone();
                    let den = den.clone();
                    let rem = rem.clone();
                    Box::new(IntCounter::default().flat_map(move |i| {
                        let quo = Int::from(i);
                        let num = &quo * &den + &rem;
                        let Some(u) = u.unify(&quotient, &Tm::Int(quo).into()) else {
                            return soln_stream::failure();
                        };
                        let Some(u) = u.unify(&numerator, &Tm::Int(num).into()) else {
                            return soln_stream::failure();
                        };
                        soln_stream::success(u)
                    }))
                }
                _ => {
                    let mode: RcTm = tm!{
                        [
                            numerator numerator.clone()
                        ][
                            denominator denominator.clone()
                        ][
                            quotient quotient.clone()
                        ][
                            remainder remainder.clone()
                        ]
                    }.into();

                    Err::GenericError {
                        rel: rel.to_string(),
                        msg: format!(
                            "[numerator][denominator][quotient][remainder] is not \
                            implemented for mode `{mode}`.")
                    }.into()
                }
            }
        });

        def_builtin!(intrs, |_rt, u, [pred][succ] as rel| {
            match (pred.as_ref(), succ.as_ref()) {
                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError {
                    rel: rel.to_string(),
                    tm: pred.clone()
                }.into(),
                (Tm::Var(_), Tm::Int(s)) => {
                    let p = Tm::Int(s - 1).into();
                    soln_stream::unifying(u, pred, &p)
                }
                (Tm::Int(p), Tm::Var(_)) => {
                    let s = Tm::Int(p + 1).into();
                    soln_stream::unifying(u, succ, &s)
                }
                (Tm::Int(p), Tm::Int(s)) => {
                    if p + 1 == *s {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => Err::GenericError {
                    rel: rel.to_string(),
                    msg: "[pred][succ] only relates numbers.".into()
                }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [_yes as "true"] as _rel| {
            soln_stream::success(u)
        });

        def_builtin!(intrs, |_rt, _u, [_no as "false"] as _rel| {
            soln_stream::failure()
        });

        def_builtin!(intrs, |state, u, [goal as "not"] as _rel| {
            let mut solns = state.rt.solve_query_impl(goal.clone(), u.clone(), state.td);
            match solns.next() {
                Some(Err(e)) => soln_stream::error(e),
                Some(Ok(_)) => soln_stream::failure(),
                None => soln_stream::success(u),
            }
        });

        // https://www.swi-prolog.org/pldoc/doc_for?object=catch/3
        def_builtin!(intrs, |state, u, [goal][truth] as _rel| {
            let solns = state.rt.solve_query_impl(goal.clone(), u.clone(), state.td);

            let has_errored = Cell::new(false);

            let truth_cpy = truth.clone();
            let u_cpy = u.clone();
            let mut solns = solns.map_while(move |res| {
                let truth = truth_cpy.clone();
                let u = u_cpy.clone();

                if has_errored.get() {
                    return None;
                }

                match res {
                    Ok(u) => u.unify(&truth, &RcTm::sym_true()).map(Ok),
                    Err(e) => {
                        has_errored.set(true);
                        let error = tm!([error Tm::from(e).into()]).into();
                        u.unify(&truth, &error).map(Ok)
                    },
                }
            }).peekable();

            if solns.peek().is_none() {
                return soln_stream::unifying(u, truth, &RcTm::sym_false());
            }

            Box::new(solns)
        });

        #[derive(Clone, Copy)]
        #[allow(clippy::enum_variant_names)]
        enum StdStream {
            StdIn,
            StdOut,
            StdErr,
        }

        impl Display for StdStream {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    StdStream::StdIn => write!(f, "stdin"),
                    StdStream::StdOut => write!(f, "stdout"),
                    StdStream::StdErr => write!(f, "stderr"),
                }
            }
        }

        impl TryFrom<&RcTm> for StdStream {
            type Error = ();
            fn try_from(value: &RcTm) -> Result<Self, Self::Error> {
                match value.as_ref() {
                    Tm::Sym(s) if &*s.to_str() == "stdin" => Ok(Self::StdIn),
                    Tm::Sym(s) if &*s.to_str() == "stdout" => Ok(Self::StdOut),
                    Tm::Sym(s) if &*s.to_str() == "stderr" => Ok(Self::StdErr),
                    Tm::Int(i) if *i == 0.into() => Ok(Self::StdIn),
                    Tm::Int(i) if *i == 1.into() => Ok(Self::StdOut),
                    Tm::Int(i) if *i == 2.into() => Ok(Self::StdErr),
                    _ => Err(()),
                }
            }
        }

        fn io_write_impl(u: UnifierSet, text: &RcTm, stream: StdStream) -> Box<dyn SolnStream> {
            if let StdStream::StdIn = stream {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: "[io_write][stream]".into(),
                    key: "stream".into(),
                    expected_ty: "oneof {stdout stderr}".into(),
                    recieved_tm: stream.to_string(),
                });
            }

            let mut buf = String::new();
            match text.try_collect_txt_to_string(&mut buf) {
                Ok(()) => {}
                Err(e) => return soln_stream::error(e.into()),
            };

            let to_print = Color::LightGray.italic().paint(buf);
            match stream {
                StdStream::StdOut => print!("{to_print}"),
                StdStream::StdErr => eprint!("{to_print}"),
                _ => unreachable!(),
            }

            match stream {
                StdStream::StdOut => stdout().flush().unwrap(),
                StdStream::StdErr => stderr().flush().unwrap(),
                _ => {}
            }

            soln_stream::success(u)
        }

        def_builtin!(intrs, |_rt, u, [text as "io_write"][stream as "stream"] as rel| {
            let Ok(stream) = StdStream::try_from(stream) else {
                return Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "stream".into(),
                    expected_ty: "stream indicator".into(),
                    recieved_tm: stream.to_string()
                }.into()
            };
            io_write_impl(u, text, stream)
        });

        def_builtin!(intrs, |_rt, u, [text as "io_writeln"][stream as "stream"] as rel| {
            let Ok(stream) = StdStream::try_from(stream) else {
                return Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "stream".into(),
                    expected_ty: "stream indicator".into(),
                    recieved_tm: stream.to_string()
                }.into()
            };
            let soln_stream = io_write_impl(u, text, stream);
            match stream { // Print the '\n'.
                StdStream::StdOut => println!(),
                StdStream::StdErr => eprintln!(),
                _ => unreachable!()
            }
            soln_stream
        });

        def_builtin!(intrs, |_rt, u, [term][text] as rel| {
            match (term.as_ref(), text.as_ref()) {
                // Parsing:
                (Tm::Var(..), _) => {
                    let term_var = term;
                    let mut src_buf = String::new();
                    let Ok(()) = text.try_collect_txt_to_string(&mut src_buf) else {
                        return soln_stream::failure()
                    };

                    let mut tok_buf = Vec::new();
                    let tokens = match lex::tokenize_into(&mut tok_buf, &src_buf[..], "<user input>".into()) {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            println!("Tokenization error: {e}");
                            return soln_stream::failure();
                        }
                    };

                    let term = match parse::entire_term(tokens) {
                        Ok(q) => q,
                        Err(e) => {
                            println!("Parse error: {e}");
                            return soln_stream::failure();
                        }
                    };

                    soln_stream::unifying(u, &term, term_var)
                }

                // Outputting string repr:
                (_, Tm::Var(..) | Tm::TxtSeg(..) | Tm::TxtCons(..) | Tm::Nil) => {
                    let repr = term.to_string();
                    let term = Tm::TxtSeg(Segment::from(repr)).into();
                    soln_stream::unifying(u, &term, text)
                }

                _ => Err::ArgumentTypeError {
                        rel: rel.to_string(),
                        key: "text".into(),
                        expected_ty: "text".into(),
                        recieved_tm: text.to_string(),
                    }.into()
            }
        });

        def_builtin!(intrs, |_rt, u, [block][functor][members] as rel| {
            match (block.as_ref(), functor.as_ref(), members.as_ref()) {
                (Tm::Block(f, ms), _, _) => {
                    let f = f.into();
                    let ms = RcTm::list_from_iter(ms.iter().cloned());
                    let Some(u) = u.unify(functor, &f).and_then(|u| u.unify(members, &ms)) else {
                        return soln_stream::failure();
                    };
                    soln_stream::success(u)
                }
                (_, Tm::Sym(f), Tm::Cons(..) | Tm::Nil) => {
                    let tok = match f.to_str().as_ref() {
                        "-" => Tok::Dash,
                        "|" => Tok::Pipe,
                        _ => return soln_stream::error(Err::ArgumentTypeError {
                            rel: rel.to_string(),
                            key: "functor".to_string(),
                            expected_ty: "either the symbol `'-'` or the symbol `'|'`".to_string(),
                            recieved_tm: functor.to_string()
                        }),
                    };

                    let Some((members, None)) = members.try_as_list() else {
                        return soln_stream::error(Err::UnexpectedPartialList {
                            rel: rel.to_string(),
                            key: "members".to_string(),
                            partial: members.try_as_list().unwrap().1.unwrap().into(),
                        });
                    };
                    let b = Tm::Block(tok, members).into();
                    soln_stream::unifying(u, block, &b)
                }
                (Tm::Var(..), Tm::Var(..), _) | (Tm::Var(..), _, Tm::Var(..)) => {
                    soln_stream::error(Err::InstantiationError {
                        rel: rel.to_string(),
                        tm: block.clone(),
                    })
                }
                _ => soln_stream::failure()
            }
        });

        def_builtin!(intrs, |_rt, u, [cwd] as _rel| {
            let dir: String = std::env::current_dir()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .into_owned();
            soln_stream::unifying(u, cwd, &Tm::TxtSeg(dir.into()).into())
        });

        def_builtin!(intrs, |_rt, u, [cd] as rel| {
            if !cd.is_txt() {
                return Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "cd".into(),
                    expected_ty: "text".into(),
                    recieved_tm: cd.to_string(),
                }
                .into();
            }

            let mut path = String::new();
            match cd.try_collect_txt_to_string(&mut path) {
                Ok(()) => {}
                Err(TxtErr::UninstantiatedTail(_)) =>
                    return Err::UnexpectedPartialList {
                        rel: rel.to_string(),
                        key: "cd".into(),
                        partial: cd.clone(),
                    }
                    .into(),
                Err(e) => return soln_stream::error(e.into()),
            }

            if let Err(e) = std::env::set_current_dir(path) {
                return soln_stream::error(e.into());
            }

            soln_stream::success(u)
        });

        def_builtin!(intrs, |_rt, u, [output as "ls"] as _rel| {
            let read_dir = match std::fs::read_dir(".") {
                Ok(rd) => rd,
                Err(e) => return soln_stream::error(e.into())
            };

            let entries: Vec<_> = match read_dir.collect() {
                Ok(v) => v,
                Err(e) => return soln_stream::error(e.into()),
            };

            let list = RcTm::list_from_iter(entries.into_iter().map(|ent| {
                RcTm::sym(ent.file_name().to_string_lossy())
            }));

            soln_stream::unifying(u, output, &list)
        });

        def_builtin!(intrs, |state, u, [recursion_limit] as rel| {
            match recursion_limit.as_ref() {
                Tm::Int(i) => {
                    if i <= &Zero::zero() {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: "Recursion limit must be positive".into(),
                        }
                        .into();
                    }
                    if i >= &Int::from(usize::MAX) {
                        return Err::GenericError {
                            rel: rel.to_string(),
                            msg: format!("Recursion limit must be less than {}", usize::MAX),
                        }
                        .into();
                    }
                    state.rt.max_recursion_depth.set(i.to_usize().unwrap());
                    soln_stream::success(u)
                }
                Tm::Var(..) => {
                    let limit = Tm::Int(state.rt.max_recursion_depth.get().into()).into();
                    soln_stream::unifying(u, &limit, recursion_limit)
                }
                _ => Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "recursion_limit".into(),
                    expected_ty: "int or var".into(),
                    recieved_tm: format!("{recursion_limit}"),
                }
                .into(),
            }
        });

        def_builtin!(intrs, |state, u, [directive] as _rel| {
            let directives_clone = state.rt.db.directives.clone();
            let directive = directive.clone();
            Box::new(directives_clone
                .into_iter()
                .flat_map(move |dir| {
                    u.unify(&Tm::Rel(dir.query).into(), &directive)
                })
                .map(Ok))
        });

        def_builtin!(intrs, |state, u, [directives] as _rel| {
            let it = state
                .rt
                .db
                .directives
                .iter()
                .cloned()
                .map(|dir| Tm::Rel(dir.query).into());
            let ds = RcTm::list_from_iter(it);
            soln_stream::unifying(u, directives, &ds)
        });

        def_builtin!(intrs, |state, u, [clause_head][clause_body] as rel| {
            let clause_head = clause_head.clone();
            let clause_body = clause_body.clone();

            if let Tm::Var(..) = clause_head.as_ref() {
                let clauses = state.rt.db.relations.iter().flat_map(|(_sig, clauses)| clauses);
                return Box::new(clauses
                    .into_iter()
                    .flat_map(move |Clause { head: h, body: b }| {
                        let head = Tm::Rel(h.clone()).into();
                        u.unify(&clause_head, &head).and_then(|u| {
                            u.unify(&clause_body, &if let Some(b) = b {
                                b.clone()
                            } else {
                                tm!([true]).into()
                            })
                        })
                    })
                    .map(Ok));
            }

            let Some(clause_head_rel) = clause_head.try_as_rel() else {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "clause_head".to_string(),
                    expected_ty: "relation".to_string(),
                    recieved_tm: clause_head.to_string(),
                });
            };

            let Some(clauses) = state.rt.db.index_match(clause_head_rel, &u, state.rt.debug_mode.get()) else {
                return soln_stream::failure();
            };

            Box::new(clauses
                .into_iter()
                .flat_map(move |Clause { head: h, body: b }| {
                    let head = Tm::Rel(h.clone()).into();
                    u.unify(&clause_head, &head).and_then(|u| {
                        u.unify(&clause_body, &if let Some(b) = b {
                            b.clone()
                        } else {
                            tm!([true]).into()
                        })
                    })
                })
                .map(Ok))
        });

        def_builtin!(intrs, |_state, u, [tm][class] as _rel| {
            let cls = match tm.as_ref() {
                Tm::Nil  => tm.clone(),
                Tm::Cons(..) => tm.clone(),
                Tm::Var(v) => tm!([var
                    tm!([name
                        Tm::Sym(v.name).into()
                    ][suffix
                        v.suffix.map(|s| Tm::Sym(s).into()).unwrap_or(Tm::Nil.into())
                    ][gen
                        v.gen.into()
                    ]).into()
                ]).into(),
                Tm::Int(..) => Tm::Sym("int".into()).into(),
                Tm::Sym(..) => Tm::Sym("sym".into()).into(),
                Tm::TxtSeg(..) => Tm::Sym("txt".into()).into(),
                Tm::TxtCons(..) => Tm::Sym("txt".into()).into(),
                Tm::BinOp(f, ..) => tm!([binop (*f).into()]).into(),
                Tm::Block(f, members) => tm!([block
                    tm!([functor
                            Tm::Sym(Sym::from(f.to_string().as_ref())).into()
                        ][members
                            RcTm::list_from_iter(members.iter().cloned())
                        ]).into()
                    ]).into(),
                Tm::Rel(rel) => tm!([rel
                        Tm::Rel(rel.iter().map(|(k, _)| {
                            let k_sym: RcTm = Tm::Sym(*k).into();
                            (*k, k_sym) // TODO: use key-variable pairs instead of key-symbol pairs (make it a Sig).
                        }).collect()).into()
                    ]).into(),
            };

            soln_stream::unifying(u, class, &cls)
        });

        def_builtin!(intrs, |_state, u, [tm_in][reified_tm_out] as _rel| {

            fn reify(tm: &RcTm) -> RcTm {
                match tm.as_ref() {
                    Tm::Nil  => tm.clone(),
                    Tm::Cons(head, tail) => Tm::Cons(reify(head), reify(tail)).into(),
                    Tm::Var(v) => tm!([var
                        tm!([name
                            Tm::Sym(v.name).into()
                        ][suffix
                            v.suffix.map(|s| Tm::Sym(s).into()).unwrap_or(Tm::Nil.into())
                        ][gen
                            v.gen.into()
                        ]).into()
                    ]).into(),
                    Tm::Int(..) => tm!([int tm.clone()]).into(),
                    Tm::Sym(..) => tm!([sym tm.clone()]).into(),
                    Tm::TxtSeg(seg) => {
                        tm!([txt
                            tm!([seg
                                Tm::Sym(Sym::from(seg.segment_as_str())).into()
                            ][tail
                                reify(seg.segment_tail())
                            ]).into()
                        ]).into()
                    }
                    Tm::TxtCons(car, cdr) => {
                        tm!([txt
                            tm!([car
                                reify(car)
                            ][cdr
                                reify(cdr)
                            ]).into()
                        ]).into()
                    }
                    Tm::BinOp(f, lhs, rhs) => tm!([binop
                        tm!([functor
                                (*f).into()
                            ][lhs
                                reify(lhs)
                            ][rhs
                                reify(rhs)
                            ]).into()
                        ]).into(),
                    Tm::Block(f, members) => tm!([block
                        tm!([functor
                                Tm::Sym(Sym::from(f.to_string().as_ref())).into()
                            ][members
                                RcTm::list_from_iter(members.iter().map(reify))
                            ]).into()
                        ]).into(),
                    Tm::Rel(rel) => tm!([rel
                        tm!([attrs
                                RcTm::list_from_iter(rel.iter().map(|(k, v)| {
                                    tm!([key
                                        Tm::Sym(*k).into()
                                    ][value
                                        reify(v)
                                    ]).into()
                                }))
                            ]).into()
                        ]).into(),
                }
            }

            soln_stream::unifying(u, reified_tm_out, &reify(tm_in))
        });

        def_builtin!(intrs, |_state, u, [file_path][clauses][directives] as rel| {
            match (file_path.as_ref(), clauses.as_ref(), directives.as_ref()) {
                (txt, _, _) if txt.is_txt() => {
                    let mut path_buf = String::new();
                    if file_path.try_collect_txt_to_string(&mut path_buf).is_err() {
                        return soln_stream::error(Err::UnexpectedPartialList {
                            rel: rel.to_string(),
                            key: "file_path".into(),
                            partial: file_path.clone(),
                        });
                    }

                    let mut kb = KnowledgeBase::default();
                    if let Err(e) = kb.include(path_buf) {
                        return soln_stream::error(e);
                    }

                    let KnowledgeBase { directives: ds, relations: rs } = kb;

                    let ds = RcTm::list_from_iter(ds.into_iter().map(|dir| {
                        tm!([directive Tm::Rel(dir.query).into()]).into()
                    }));

                    let cs = RcTm::list_from_iter(rs.into_iter().flat_map(|(_sig, clauses)| {
                        clauses.into_iter().map(|clause| {
                            let Clause { head, body } = clause;
                            match body {
                                Some(body) => tm!([head head.into()][body body]),
                                None => tm!([fact head.into()]),
                            }.into()
                        })
                    }));

                    let Some(u) = u.unify(directives, &ds) else {
                        return soln_stream::failure();
                    };

                    let Some(u) = u.unify(clauses, &cs) else {
                        return soln_stream::failure();
                    };

                    soln_stream::success(u)
                }
                _ => todo!()
            }
        });

        def_builtin!(intrs, |_state, _u, [raises] as rel| {
            if let Some(msg) = raises.try_as_sym() {
                soln_stream::error(Err::GenericError {
                    rel: rel.to_string(),
                    msg: msg.to_string(),
                })
            } else {
                soln_stream::error(Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "raises".into(),
                    expected_ty: "symbol".into(),
                    recieved_tm: raises.to_string(),
                })
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

        // TODO: #3 once prolog's `forall` is implemented, this can become just
        // `[builtin]`, i.e. a multi-deterministic relation.
        def_builtin!(intrs, |_rt, u, [builtins] as _rel| {
            let builtin_rel_sigs = builtin_rel_sigs.clone();
            soln_stream::unifying(u, builtins, &builtin_rel_sigs)
        });

        intrs
    }

    pub(crate) fn index_match(&self, rel: &Rel) -> Option<&Builtin> {
        self.0.get(&rel.keys().cloned().collect())
    }
}
