use std::io::{stderr, stdout, Write};

use crate::{
    ast::{RcTm, Tm},
    def_intrinsic, ident_of_binding, name_of_binding,
    rt::{
        err::Err,
        soln_stream::{self, SolnStream},
        UnifierSet,
    },
};

use super::IntrinsicsMap;

#[derive(Clone, Copy)]
#[allow(clippy::enum_variant_names)]
enum StdStream {
    StdIn,
    StdOut,
    StdErr,
    Fd(std::os::raw::c_int),
}

impl std::fmt::Display for StdStream {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StdStream::StdIn => write!(f, "stdin"),
            StdStream::StdOut => write!(f, "stdout"),
            StdStream::StdErr => write!(f, "stderr"),
            StdStream::Fd(fd) => write!(f, "{fd}"),
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

pub fn def_syscall_intrinsics(intrs: &mut IntrinsicsMap) {
    def_intrinsic!(intrs, |_rt, u, [text as "io_write"][stream as "stream"]| {
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

    def_intrinsic!(intrs, |_rt, u, [text as "io_writeln"][stream as "stream"]| {
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
}
