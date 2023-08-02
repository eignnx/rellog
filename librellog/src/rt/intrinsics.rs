use std::{
    collections::BTreeMap,
    fmt::{Debug, Display},
    io::{stderr, stdout, Write},
};

use rpds::Vector;

use crate::{
    ast::{RcTm, Rel, Sig, Tm},
    lex, parse,
    rt::Err,
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

                (_, _) => Err::TypeError { msg: "[rel][attrs] takes a relation and a list of attributes".into() }.into(),
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
                    soln_stream::error(Err::TypeError { msg: "use [rel][attr] instead".into() })
                }

                // [[mode [attr inout][key in][value inout]]]
                (_, Tm::Sym(key), _) => {
                    let a = Tm::Rel(Rel::new().insert(*key, value.clone())).into();
                    soln_stream::unifying(u, attr, &a)
                }

                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError(key.clone()).into(),

                _ => Err::TypeError { msg: "bad arguments to [attr][key][value]".into() }.into()
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
                _ => Err::TypeError { msg: "[gt][lt] accepts two concrete numbers".into() }.into(),
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
                _ => Err::TypeError { msg: "[gte][lte] accepts two concrete numbers".into() }.into()
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
                Tm::Var(_) => return Err::InstantiationError(rel.clone()).into(),
                _ => return Err::TypeError { msg: "[Rel][key][value] requires a relation for `Rel`.".into() }.into()
            };

            let key = match key.as_ref() {
                Tm::Sym(key) => key,
                Tm::Var(_) => return Err::InstantiationError(key.clone()).into(),
                _ => return Err::TypeError { msg: "[rel][Key][value] requires a ground term for `Key`.".into() }.into()
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

                _ => Err::TypeError {
                        msg: "only modes supported for `[txt_prefix][txt_suffix][txt_compound]`:\n\
                            \t[[mode txt_[prefix in][suffix  in][compound out]]]\n\
                            \t[[mode txt_[prefix in][suffix out][compound out]]]\n\
                            ".into()
                    }.into(),
            }
        });

        def_intrinsic!(intrs, |u, [product][x][y]| {
            match (product.as_ref(), x.as_ref(), y.as_ref()) {
                (_, Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(x.clone()).into(),
                (Tm::Var(_), _, Tm::Var(_)) => Err::InstantiationError(y.clone()).into(),
                (Tm::Var(_), Tm::Var(_), _) => Err::InstantiationError(x.clone()).into(),
                (_, Tm::Num(x), Tm::Num(y)) => {
                    let p = Tm::Num(x * y).into();
                    soln_stream::unifying(u, product, &p)
                }
                _ => Err::TypeError { msg: "[product][x][y] only relates numbers.".into() }.into()
            }
        });

        def_intrinsic!(intrs, |u, [pred][succ]| {
            match (pred.as_ref(), succ.as_ref()) {
                (Tm::Var(_), Tm::Var(_)) => Err::InstantiationError(pred.clone()).into(),
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
                _ => Err::TypeError { msg: "[pred][succ] only relates numbers.".into() }.into()
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
                    Tm::Num(0) => Ok(Self::StdIn),
                    Tm::Num(1) => Ok(Self::StdOut),
                    Tm::Num(2) => Ok(Self::StdErr),
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

        def_intrinsic!(intrs, |u, [text as "io_write"][stream as "stream"]| {
            let Ok(stream) = StdStream::try_from(stream) else {
                return Err::ArgumentTypeError {
                    rel: "[io_write][stream]".into(),
                    key: "stream".into(),
                    expected_ty: "stream indicator".into(),
                    recieved_tm: stream.to_string()
                }.into()
            };
            io_write_impl(u, text, stream)
        });

        def_intrinsic!(intrs, |u, [text as "io_writeln"][stream as "stream"]| {
            let Ok(stream) = StdStream::try_from(stream) else {
                return Err::ArgumentTypeError {
                    rel: "[io_writeln][stream]".into(),
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

        def_intrinsic!(intrs, |u, [cwd]| {
            let dir: String = std::env::current_dir()
                .unwrap()
                .as_os_str()
                .to_string_lossy()
                .into_owned();
            soln_stream::unifying(u, cwd, &Tm::Txt(dir.into(), Tm::Nil.into()).into())
        });

        def_intrinsic!(intrs, |u, [cd]| {
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

            match std::env::set_current_dir(path) {
                Err(e) => return soln_stream::error(e.into()),
                Ok(_) => {}
            };

            soln_stream::success(u)
        });

        def_intrinsic!(intrs, |u, [output as "ls"]| {
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
