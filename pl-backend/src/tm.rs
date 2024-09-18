use std::io;

use librellog::{
    ast::{BinOpSymbol, RcTm, Tm},
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
            Tm::TxtCons(..) | Tm::TxtSeg(..) => {
                let mut buf = String::new();
                match self.try_collect_txt_to_string(&mut buf) {
                    Ok(()) => write!(f, "\"{}\"", buf)?,
                    _ => write!(f, "\"<ERROR>\"")?,
                }
            }
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

impl Compile<SwiProlog> for Var {
    fn compile(&self, f: &mut dyn std::io::Write, _: &mut SwiProlog) -> io::Result<()> {
        let Var { name, suffix, .. } = self.clone();
        match suffix {
            None => write!(f, "{name}"),
            Some(suffix) => write!(f, "{name}_{suffix}"),
        }
    }
}
