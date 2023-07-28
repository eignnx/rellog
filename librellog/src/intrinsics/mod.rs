use std::{
    collections::BTreeMap,
    fmt::{write, Debug},
    io::{stderr, stdout, Write},
    iter,
};

use rpds::Vector;

use crate::{
    ast::{RcTm, Rel, Sig, Tm},
    lex, parse,
    rt::{
        soln_stream::{self, SolnStream},
        UnifierSet,
    },
    tm,
};

pub struct Intrinsic {
    signature: Sig,
    func: Box<dyn Fn(UnifierSet, Rel) -> Box<dyn SolnStream>>,
}

impl Intrinsic {
    pub fn apply(&self, u: UnifierSet, rel: Rel) -> Box<dyn SolnStream> {
        if self.signature != Sig::from(rel.clone()) {
            return soln_stream::failure();
        }

        (self.func)(u, rel)
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

macro_rules! def_intrinsic {
    ($intrs:expr, |$u:ident, $([$ident:ident $(as $name:literal)?])+| $body:expr) => {
        let sig = [$(name_of_binding!($ident $(as $name)?),)+];
        $intrs.def(&sig, move |u, rel| {
            $(
            let ident_of_binding!($ident $(as $name)?) = match rel.get(&name_of_binding!($ident $(as $name)?).into()) {
                Some(x) => u.reify_term(x),
                None => return soln_stream::failure(),
            };
            let ident_of_binding!($ident $(as $name)?) = &ident_of_binding!($ident $(as $name)?);
            )+

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
        func: impl Fn(UnifierSet, Rel) -> Box<dyn SolnStream> + 'static,
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
                            .map(|(k, v)| (*k, v.clone()))
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

            if let Some(found) = rel.get(key) {
                soln_stream::unifying(u, value, found)
            }else{
                soln_stream::failure()
            }
        });

        def_intrinsic!(intrs, |u, [txt_prefix][txt_suffix][txt_compound]| {
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

                _ => todo!("only modes supported:\n\
                            \t[[mode txt.[prefix in][suffix  in][compound out]]]\n\
                            \t[[mode txt.[prefix in][suffix out][compound out]]]\n\
                            "),
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

        def_intrinsic!(intrs, |u, [_yes as "true"]| {
            soln_stream::success(u)
        });

        def_intrinsic!(intrs, |_u, [_no as "false"]| {
            soln_stream::failure()
        });

        #[derive(Clone, Copy)]
        enum StdStream {
            StdIn,
            StdOut,
            StdErr,
        }

        impl From<&RcTm> for StdStream {
            fn from(value: &RcTm) -> Self {
                match value.as_ref() {
                    Tm::Sym(s) if &*s.to_str() == "stdin" => Self::StdIn,
                    Tm::Sym(s) if &*s.to_str() == "stdout" => Self::StdOut,
                    Tm::Sym(s) if &*s.to_str() == "stderr" => Self::StdErr,
                    Tm::Num(0) => Self::StdIn,
                    Tm::Num(1) => Self::StdOut,
                    Tm::Num(2) => Self::StdErr,
                    _ => todo!(
                        "argument error: `Stream` must be one of `stdin`, `stdout`, `stderr`, or `0`, `1`, or `2`."
                    )
                }
            }
        }

        fn io_write_impl(u: UnifierSet, mut text: &RcTm, stream: StdStream) -> Box<dyn SolnStream> {
            if let StdStream::StdIn = stream {
                todo!("argument error: in [io_write][Stream], cannot write to `stdin`.")
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
                todo!("type error: Partial string passed to `[io_write][stream]`");
            }

            match stream {
                StdStream::StdOut => stdout().flush().unwrap(),
                StdStream::StdErr => stderr().flush().unwrap(),
                _ => {}
            }

            soln_stream::success(u)
        }

        def_intrinsic!(intrs, |u, [text as "io_write"][stream as "stream"]| {
            let stream = StdStream::from(stream);
            io_write_impl(u, text, stream)
        });

        def_intrinsic!(intrs, |u, [text as "io_writeln"][stream as "stream"]| {
            let stream = StdStream::from(stream);
            let soln_stream = io_write_impl(u, text, stream);
            match stream { // Print the '\n'.
                StdStream::StdOut => println!(),
                StdStream::StdErr => eprintln!(),
                _ => unreachable!()
            }
            soln_stream
        });

        def_intrinsic!(intrs, |u, [term][text]| {
            match (term.as_ref(), text.as_ref()) {
                (Tm::Var(..), _) => {
                    let term_var = term;
                    let mut src_buf = String::new();
                    let Ok(()) = text.try_collect_txt_to_string(&mut src_buf) else {
                        return soln_stream::failure()
                    };

                    let mut tok_buf = Vec::new();
                    let tokens = lex::tokenize_into(&mut tok_buf, &src_buf[..]);

                    let term = match parse::entire_term(tokens) {
                        Ok(q) => q,
                        Err(e) => {
                            println!("Parse error:");
                            parse::display_parse_err(&e);
                            return soln_stream::failure();
                        }
                    };

                    soln_stream::unifying(u, &term, term_var)
                }
                (_, Tm::Var(..) | Tm::Txt(..)) => {
                    let term = Tm::Txt(term.to_string().into(), Tm::Nil.into()).into();
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
