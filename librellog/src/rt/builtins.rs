use std::{
    cell::{Cell, RefCell},
    collections::{BTreeMap, BTreeSet},
    fmt::{Debug, Display},
    io::{stderr, stdout, Write},
};

use num::{Integer, ToPrimitive, Zero};
use rpds::Vector;
use unifier_set::DirectChildren;

use crate::{
    ast::{dup::TmDuplicator, Clause, RcTm, Rel, Sig, Tm},
    data_structures::{Int, Var},
    lex::{self, tok::Tok},
    parse,
    rt::{kb::KnowledgeBase, Err},
    rt::{
        soln_stream::{self, SolnStream},
        UnifierSet,
    },
    tm,
    utils::int_counter::IntCounter,
};

use super::Rt;

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
        let sig: Sig = sig.iter().map(|s| s.into()).collect();

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
                            tm:attrs.clone()
                        })
                    };

                    let rel: Result<Rel, Err> = attrs.into_iter()
                        .map(|attr| match attr.as_ref() {
                            Tm::Rel(r) if r.size() == 1 => {
                                Ok(r
                                    .iter()
                                    .map(|(k, v)| (*k, v.clone()))
                                    .next()
                                    .expect("There's exactly one key-value pair in here"))
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

                (Tm::Rel(rel), Tm::Var(_)) => {
                    let list = RcTm::list_from_iter(rel.iter().map(|(k, v)| Tm::Rel(Rel::new().insert(*k, v.clone())).into()));
                    soln_stream::unifying(u, attrs, &list)
                }

                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError {
                    rel: rel_called.to_string(),
                    tm: rel.clone(),
                }.into(),

                (_, _) => Err::GenericError {
                    rel: rel_called.to_string(),
                    msg: "[rel][attrs] takes a relation and a list of attributes".into()
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

        def_builtin!(intrs, |_rt, u, [gt][lt] as rel| {
            match (gt.as_ref(), lt.as_ref()) {
                (Tm::Int(gt), Tm::Int(lt)) => {
                    if gt > lt {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
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
                Tm::Txt(..) => soln_stream::success(u),
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
            let vars: BTreeSet<Var> = term.variables().cloned().collect();
            let vars = RcTm::list_from_iter(vars.into_iter().map(RcTm::from));
            soln_stream::unifying(u, &vars, variables)
        });

        def_builtin!(intrs, |state, u, [original][duplicate] as _rel| {
            let new = state.td.borrow_mut().duplicate(original);
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
                .duplicate_conditionally(original, Box::new(move |var| {
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
                    let lower_sym = Tm::Sym(lower.into()).into();
                    soln_stream::unifying(u, &lower_sym, snake_case)
                }
                (_, Tm::Sym(sc)) => {
                    let lower = format!("{}", heck::AsPascalCase(&sc.to_str()[..]));
                    let lower_sym = Tm::Sym(lower.into()).into();
                    soln_stream::unifying(u, &lower_sym, pascal_case)
                }
                _ => soln_stream::error(Err::GenericError {
                    rel: rel.to_string(),
                    msg: "`[snake_case][pascal_case]` requires either one or two \
                          ground symbols as arguments.".into()
                }),
            }
        });

        def_builtin!(intrs, |_rt, u, [txt_prefix][txt_suffix][txt_compound] as rel| {
            use Tm::{Txt, Var};
            match (txt_prefix.as_ref(), txt_suffix.as_ref(), txt_suffix.as_ref()) {
                (Txt(ref prefix_head, ref prefix_tail), Txt(suffix_head, suffix_tail), _) => {
                    let mut prefix_tail = prefix_tail;

                    let mut segments = vec![prefix_head.clone()];

                    while let Tm::Txt(prefix_hd, prefix_tl) = prefix_tail.as_ref() {
                        segments.push(prefix_hd.clone());
                        prefix_tail = prefix_tl;
                    }

                    let mut compound = suffix_head.clone();

                    for segment in segments.into_iter().rev() {
                        compound = compound.cons_str(segment);
                    }

                    let compound = Txt(compound, suffix_tail.clone()).into();
                    soln_stream::unifying(u, txt_compound, &compound)
                }

                // -- [prefix "abc"][Suffix][Compound]
                //  - Compound = "abc[..Suffix]"
                (Txt(cl, tl), Var(_), _) => {
                    let Some(u) = u.unify(tl, txt_suffix) else {
                        return soln_stream::failure();
                    };

                    let consed = Tm::Txt(cl.clone(), txt_suffix.clone()).into();

                    let Some(u) = u.unify(&consed, txt_compound) else {
                        return soln_stream::failure();
                    };

                    soln_stream::success(u)
                }

                _ => Err::GenericError {
                        rel: rel.to_string(),
                        msg: "only modes supported for `[txt_prefix][txt_suffix][txt_compound]`:\n\
                            \t[[mode txt_[prefix in][suffix  in][compound out]]]\n\
                            \t[[mode txt_[prefix in][suffix out][compound out]]]\n\
                            ".into()
                    }.into(),
            }
        });

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
        // catch(:Goal, +ExceptionTerm, :RecoveryGoal)
        def_builtin!(intrs, |state, u, [goal][error] as _rel| {
            let solns = state.rt.solve_query_impl(goal.clone(), u.clone(), state.td);
            let has_errored = Cell::new(false);
            let error = error.clone();
            Box::new(solns.map_while(move |res| {
                let error = error.clone();
                if has_errored.get() {
                    return None;
                }
                match res {
                    Ok(u) => Some(Ok(u)),
                    Err(e) => {
                        has_errored.set(true);
                        u.unify(&error, &Tm::Sym(format!("{e}").into()).into()).map(Ok)
                    },
                }
            }))
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

        fn io_write_impl(u: UnifierSet, mut text: &RcTm, stream: StdStream) -> Box<dyn SolnStream> {
            if let StdStream::StdIn = stream {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: "[io_write][stream]".into(),
                    key: "stream".into(),
                    expected_ty: "oneof {stdout stderr}".into(),
                    recieved_tm: stream.to_string(),
                });
            }

            while let Tm::Txt(cl, tl) = text.as_ref() {
                use nu_ansi_term::Color;
                let to_print = Color::LightGray.italic().paint(cl.as_str());
                match stream {
                    StdStream::StdOut => print!("{to_print}"),
                    StdStream::StdErr => eprint!("{to_print}"),
                    _ => unreachable!(),
                }
                text = tl;
            }

            if !matches!(text.as_ref(), Tm::Nil) {
                return soln_stream::error(Err::ArgumentTypeError {
                    rel: "[io_write][stream]".into(),
                    key: "io_write".into(),
                    expected_ty: "text".into(),
                    recieved_tm: "a partial string".into(),
                });
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
                (_, Tm::Var(..) | Tm::Txt(..)) => {
                    let term = Tm::Txt(term.to_string().into(), Tm::Nil.into()).into();
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
            soln_stream::unifying(u, cwd, &Tm::Txt(dir.into(), Tm::Nil.into()).into())
        });

        def_builtin!(intrs, |_rt, u, [cd] as rel| {
            if !matches!(cd.as_ref(), Tm::Txt(_, _)) {
                return Err::ArgumentTypeError {
                    rel: rel.to_string(),
                    key: "cd".into(),
                    expected_ty: "text".into(),
                    recieved_tm: cd.to_string(),
                }
                .into();
            }

            let mut path = String::new();
            let mut it = cd;

            while let Tm::Txt(head, tail) = it.as_ref() {
                path.push_str(head.as_str());
                it = tail;
            }

            let &Tm::Nil = it.as_ref() else {
                return Err::UnexpectedPartialList {
                    rel: rel.to_string(),
                    key: "cd".into(),
                    partial: cd.clone(),
                }
                .into();
            };

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
                .flat_map(move |rel| u.unify(&Tm::Rel(rel).into(), &directive))
                .map(Ok))
        });

        def_builtin!(intrs, |state, u, [directives] as _rel| {
            let it = state
                .rt
                .db
                .directives
                .iter()
                .cloned()
                .map(|rel| Tm::Rel(rel).into());
            let ds = RcTm::list_from_iter(it);
            soln_stream::unifying(u, directives, &ds)
        });

        def_builtin!(intrs, |state, u, [clause_head][clause_body] as _rel| {
            let relations_clone = state.rt.db.relations.clone();
            let clause_head = clause_head.clone();
            let clause_body = clause_body.clone();
            Box::new(relations_clone
                .into_iter()
                // TODO: use `_sig` to trim search space via signature indexing.
                .flat_map(move |(_sig, clauses)| clauses)
                .flat_map(move |Clause { head: h, body: b }| {
                    u.unify(&clause_head, &RcTm::from(h)).and_then(|u| {
                        u.unify(&clause_body, &if let Some(b) = b {
                            b
                        } else {
                            tm!([true]).into()
                        })
                    })
                })
                .map(Ok))
        });

        def_builtin!(intrs, |_state, u, [file_path][clauses][directives] as rel| {
            match (file_path.as_ref(), clauses.as_ref(), directives.as_ref()) {
                (Tm::Txt(..), _, _) => {
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

                    let ds = RcTm::list_from_iter(ds.into_iter().map(|rel| Tm::Rel(rel).into()));
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
