use std::io;

use librellog::{
    ast::{txt::TxtErr, BinOpSymbol, RcTm, Tm},
    data_structures::Var,
};

use crate::{Compile, SwiProlog};

impl Compile<SwiProlog> for RcTm {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        match &**self {
            Tm::Nil => write!(f, "[]")?,
            Tm::Cons(..) => match self.try_as_list() {
                Some((init, None)) => {
                    write!(f, "[")?;
                    for (i, item) in init.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        item.compile(f, compiler)?;
                    }
                    write!(f, "]")?;
                }
                Some((init, Some(tail))) => {
                    write!(f, "[")?;
                    for (i, item) in init.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        item.compile(f, compiler)?;
                    }
                    write!(f, "|")?;
                    tail.compile(f, compiler)?;
                    write!(f, "]")?;
                }
                None => todo!("non-var non-cons non-nil list tail"),
            },
            Tm::Rel(rel) => {
                compiler.compile_rel(f, rel)?;
            }
            Tm::Sym(sym) => write!(f, "'{}'", sym)?,
            Tm::Block(functor, members) => {
                writeln!(f, "'$block'('{functor}', [")?;
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ",")?;
                    }
                    write!(f, "\t\t")?;
                    member.compile(f, compiler)?;
                }
                write!(f, "\n\t])")?;
            }
            Tm::Var(var) => var.compile(f, compiler)?,
            Tm::Int(i) => write!(f, "{i}")?,
            Tm::TxtSeg(..) | Tm::TxtCons(..) => compiler.compile_txt(f, self)?,
            Tm::BinOp(bsym, lhs, rhs) => match bsym {
                BinOpSymbol::Equal => {
                    write!(f, "(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, " = ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
                BinOpSymbol::PathSep => {
                    write!(f, "'::'(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, ", ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
                BinOpSymbol::Tilde => {
                    write!(f, "'~'(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, ", ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
                BinOpSymbol::Semicolon => {
                    write!(f, "(")?;
                    lhs.compile(f, compiler)?;
                    write!(f, ", ")?;
                    rhs.compile(f, compiler)?;
                    write!(f, ")")?;
                }
            },
        }
        Ok(())
    }
}

impl SwiProlog {
    fn compile_txt(&mut self, f: &mut dyn io::Write, txt: &RcTm) -> io::Result<()> {
        let mut buf = String::new();
        if txt.try_collect_txt_to_string(&mut buf).is_ok() {
            write!(f, "\"{buf}\"")?;
            return Ok(());
        }

        let mut tail = txt;
        let mut first = true;
        write!(f, "[")?;
        loop {
            if !first {
                write!(f, ", ")?;
            }

            match tail.as_ref() {
                Tm::Nil => break,
                Tm::TxtCons(ch, rest) => {
                    ch.compile(f, self)?;
                    tail = rest;
                }
                Tm::TxtSeg(seg) => {
                    for ch in seg.segment_as_str().chars() {
                        if !first {
                            write!(f, ", ")?;
                        }
                        ch.compile(f, self)?;
                        first = false;
                    }
                    tail = seg.segment_tail();
                }
                _ => {
                    write!(f, " | ")?;
                    tail.compile(f, self)?;
                    break;
                }
            }
            first = false;
        }
        write!(f, "]")?;
        Ok(())
    }
}

impl Compile<SwiProlog> for char {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        match self {
            '\\' => write!(f, r"'\\'")?,
            '\'' => write!(f, r"'\''")?,
            '\n' => write!(f, r"'\n'")?,
            '\r' => write!(f, r"'\r'")?,
            'a'..='z' => write!(f, "{}", self)?,
            ch => write!(f, "'{ch}'")?,
        }
        Ok(())
    }
}

impl Compile<SwiProlog> for Var {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        let Var { name, suffix, .. } = self.clone();
        match suffix {
            None => write!(f, "{name}"),
            Some(suffix) => write!(f, "{name}_{suffix}"),
        }
    }
}
