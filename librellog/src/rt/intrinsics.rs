mod syscalls;

use std::{collections::BTreeMap, fmt::Debug};

use num::{Integer, ToPrimitive, Zero};
use rpds::Vector;

use crate::{
    ast::{RcTm, Rel, Sig, Tm},
    data_structures::Int,
    lex, parse,
    rt::{intrinsics::syscalls::def_syscall_intrinsics, Err},
    rt::{
        soln_stream::{self, SolnStream},
        UnifierSet,
    },
    tm,
    utils::int_counter::IntCounter,
};

use super::Rt;

pub struct Intrinsic {
    signature: Sig,
    func: Box<dyn Fn(&Rt, UnifierSet, Rel) -> Box<dyn SolnStream>>,
}

impl Intrinsic {
    pub fn apply(&self, rt: &Rt, u: UnifierSet, rel: Rel) -> Box<dyn SolnStream> {
        if self.signature != Sig::from(rel.clone()) {
            return soln_stream::failure();
        }

        (self.func)(rt, u, rel)
    }
}

#[macro_export]
macro_rules! name_of_binding {
    ($ident:ident as $name:literal) => {
        $name
    };

    ($ident:ident) => {
        stringify!($ident)
    };
}

#[macro_export]
macro_rules! ident_of_binding {
    ($ident:ident as $name:literal) => {
        $ident
    };

    ($ident:ident) => {
        $ident
    };
}

#[macro_export]
macro_rules! def_intrinsic {
    ($intrs:expr, |$rt:ident, $u:ident, $([$ident:ident $(as $name:literal)?])+| $body:expr) => {
        let sig = [$(name_of_binding!($ident $(as $name)?),)+];
        $intrs.def(&sig, move |rt, u, rel| {
            $(
            let ident_of_binding!($ident $(as $name)?) = match rel.get(&name_of_binding!($ident $(as $name)?).into()) {
                Some(x) => u.reify_term(x),
                None => return soln_stream::failure(),
            };
            let ident_of_binding!($ident $(as $name)?) = &ident_of_binding!($ident $(as $name)?);
            )+

            let $rt = rt;
            let $u = u;

            $body
        });
    };
}

pub struct IntrinsicsMap(BTreeMap<Sig, Intrinsic>);

impl Debug for IntrinsicsMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "IntrinsicsMap(")?;
        for sig in self.0.keys() {
            writeln!(f, "\t{sig},")?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

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
        func: impl Fn(&Rt, UnifierSet, Rel) -> Box<dyn SolnStream> + 'static,
    ) {
        let sig: Sig = sig.iter().map(|s| s.into()).collect::<Vector<_>>().into();

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

        def_intrinsic!(intrs, |_rt, u, [eq1][eq2]| {
            soln_stream::unifying(u, eq1, eq2)
        });

        def_intrinsic!(intrs, |_rt, u, [rel][attrs]| {
            match (rel.as_ref(), attrs.as_ref()) {
                (Tm::Var(_), Tm::Cons(_, _)) => {
                    let var = rel;
                    let attrs: Vector<RcTm> = match attrs.try_as_list().unwrap() {
                        (vec, None) => vec,
                        (_vec, Some(_tail_var)) => return soln_stream::error(Err::InstantiationError(attrs.clone()))
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
                            Tm::Var(_) => Err(Err::InstantiationError(attr.clone())),
                            _ => Err(Err::ArgumentTypeError {
                                rel: "[rel][attrs]".into(),
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

                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(rel.clone()).into(),

                (_, _) => Err::GenericError { msg: "[rel][attrs] takes a relation and a list of attributes".into() }.into(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [attr][key][value]| {
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
                    soln_stream::error(Err::GenericError { msg: "use [rel][attr] instead".into() })
                }

                // [[mode [attr inout][key in][value inout]]]
                (_, Tm::Sym(key), _) => {
                    let a = Tm::Rel(Rel::new().insert(*key, value.clone())).into();
                    soln_stream::unifying(u, attr, &a)
                }

                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError(key.clone()).into(),

                _ => Err::GenericError { msg: "bad arguments to [attr][key][value]".into() }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [gt][lt]| {
            match (gt.as_ref(), lt.as_ref()) {
                (Tm::Int(gt), Tm::Int(lt)) => {
                    if gt > lt {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => Err::GenericError { msg: "[gt][lt] accepts two concrete numbers".into() }.into(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [gte][lte]| {
            match (gte.as_ref(), lte.as_ref()) {
                (Tm::Int(gte), Tm::Int(lte)) => {
                    if gte >= lte {
                        soln_stream::success(u)
                    } else {
                        soln_stream::failure()
                    }
                }
                _ => Err::GenericError { msg: "[gte][lte] accepts two concrete numbers".into() }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [tm as "is_var"]| {
            match u.reify_term(tm).as_ref() {
                Tm::Var(_) => soln_stream::success(u),
                _ => soln_stream::failure(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [tm as "is_num"]| {
            match u.reify_term(tm).as_ref() {
                Tm::Int(_) => soln_stream::success(u),
                _ => soln_stream::failure(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [rel][key][value]| {
            let rel = match rel.as_ref() {
                Tm::Rel(rel) => rel,
                Tm::Var(_) => return Err::InstantiationError(rel.clone()).into(),
                _ => return Err::GenericError { msg: "[Rel][key][value] requires a relation for `Rel`.".into() }.into()
            };

            let key = match key.as_ref() {
                Tm::Sym(key) => key,
                Tm::Var(_) => return Err::InstantiationError(key.clone()).into(),
                _ => return Err::GenericError { msg: "[rel][Key][value] requires a ground term for `Key`.".into() }.into()
            };

            if let Some(found) = rel.get(key) {
                soln_stream::unifying(u, value, found)
            }else{
                soln_stream::failure()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [pascal_case][snake_case]| {
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
                    msg: "`[snake_case][pascal_case]` requires either one or two \
                          ground symbols as arguments.".into()
                }),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [txt_prefix][txt_suffix][txt_compound]| {
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
                        msg: "only modes supported for `[txt_prefix][txt_suffix][txt_compound]`:\n\
                            \t[[mode txt_[prefix in][suffix  in][compound out]]]\n\
                            \t[[mode txt_[prefix in][suffix out][compound out]]]\n\
                            ".into()
                    }.into(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [sum][x][y]| {
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
                (_, Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(x.clone()).into(),
                (Tm::Var(_), _, Tm::Var(_)) => Err::InstantiationError(y.clone()).into(),
                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError(x.clone()).into(),
                _ => Err::GenericError { msg: "[sum][x][y] only relates numbers.".into() }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [product][x][y]| {
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
                            msg: "Division by zero required to solve query [product #][x 0][Y].".into()
                        }.into();
                    }
                    let res = Tm::Int(product / x).into();
                    soln_stream::unifying(u, y, &res)
                }
                (_, Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(x.clone()).into(),
                (Tm::Var(_), _, Tm::Var(_)) => Err::InstantiationError(y.clone()).into(),
                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError(x.clone()).into(),
                _ => Err::GenericError { msg: "[product][x][y] only relates numbers.".into() }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [difference][minuend][subtrahend]| {
            match (difference.as_ref(), minuend.as_ref(), subtrahend.as_ref()) {
                (a, b, c) if ![a, b, c].into_iter().all(|tm| matches!(*tm, Tm::Int(_) | Tm::Var(_))) => {
                    Err::GenericError {
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
                    msg: "[difference][minuend][subtrahend] is not implemented for that mode.".into()
                }.into(),
            }
        });

        def_intrinsic!(intrs, |_rt, u, [quotient][remainder][numerator][denominator]| {
            match (quotient.as_ref(), remainder.as_ref(), numerator.as_ref(), denominator.as_ref()) {
                (a, b, c, d) if ![a, b, c, d].into_iter().all(|tm| matches!(*tm, Tm::Int(_) | Tm::Var(_))) => {
                    Err::GenericError {
                        msg: "The arguments to [numerator][denominator][quotient][remainder] must all be unifyable with integers.".into()
                    }.into()
                }
                // quotient = numerator / denominator
                // remainder = numerator % denominator
                (_, _, Tm::Int(numer), Tm::Int(denom)) => {
                    if denom.is_zero() {
                        return Err::GenericError {
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
                        msg: format!(
                            "[numerator][denominator][quotient][remainder] is not \
                            implemented for mode `{mode}`.")
                    }.into()
                }
            }
        });

        def_intrinsic!(intrs, |_rt, u, [pred][succ]| {
            match (pred.as_ref(), succ.as_ref()) {
                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(pred.clone()).into(),
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
                _ => Err::GenericError { msg: "[pred][succ] only relates numbers.".into() }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [_yes as "true"]| {
            soln_stream::success(u)
        });

        def_intrinsic!(intrs, |_rt, _u, [_no as "false"]| {
            soln_stream::failure()
        });

        def_syscall_intrinsics(&mut intrs);

        def_intrinsic!(intrs, |_rt, u, [term][text]| {
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
                        rel: "[term][text]".into(),
                        key: "text".into(),
                        expected_ty: "text".into(),
                        recieved_tm: text.to_string(),
                    }.into()
            }
        });

        def_intrinsic!(intrs, |_rt, u, [cwd]| {
            let dir: String = std::env::current_dir()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .into_owned();
            soln_stream::unifying(u, cwd, &Tm::Txt(dir.into(), Tm::Nil.into()).into())
        });

        def_intrinsic!(intrs, |_rt, u, [cd]| {
            if !matches!(cd.as_ref(), Tm::Txt(_, _)) {
                return Err::ArgumentTypeError {
                    rel: "[cd]".into(),
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
                    rel: "[cd]".into(),
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

        def_intrinsic!(intrs, |_rt, u, [output as "ls"]| {
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

        def_intrinsic!(intrs, |rt, u, [recursion_limit]| {
            match recursion_limit.as_ref() {
                Tm::Int(i) => {
                    if i <= &Zero::zero() {
                        return Err::GenericError {
                            msg: "Recursion limit must be positive".into(),
                        }
                        .into();
                    }
                    if i >= &Int::from(usize::MAX) {
                        return Err::GenericError {
                            msg: format!("Recursion limit must be less than {}", usize::MAX),
                        }
                        .into();
                    }
                    rt.max_recursion_depth.set(i.to_usize().unwrap());
                    soln_stream::success(u)
                }
                Tm::Var(..) => {
                    let limit = Tm::Int(rt.max_recursion_depth.get().into()).into();
                    soln_stream::unifying(u, &limit, recursion_limit)
                }
                _ => Err::ArgumentTypeError {
                    rel: "[recursion_limit]".into(),
                    key: "recursion_limit".into(),
                    expected_ty: "int or var".into(),
                    recieved_tm: format!("{recursion_limit}"),
                }
                .into(),
            }
        });

        // TODO: once prolog's `forall` is implemented, this can become just
        // `[directive]`, i.e. a multi-deterministic relation.
        def_intrinsic!(intrs, |rt, u, [directives]| {
            let it = rt
                .db
                .directives
                .iter()
                .cloned()
                .map(|rel| Tm::Rel(rel).into());
            let ds = RcTm::list_from_iter(it);
            soln_stream::unifying(u, directives, &ds)
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
        def_intrinsic!(intrs, |_rt, u, [builtins]| {
            let builtin_rel_sigs = builtin_rel_sigs.clone();
            soln_stream::unifying(u, builtins, &builtin_rel_sigs)
        });

        intrs
    }

    pub(crate) fn index_match(&self, rel: &Rel) -> Option<&Intrinsic> {
        self.0.get(&rel.clone().into())
    }
}
