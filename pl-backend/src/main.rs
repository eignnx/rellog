use core::fmt;
use std::io::Read;
use std::{char, io};
use std::{fmt::Formatter, io::Write};

use librellog::ast::txt::{Segment, TxtErr};
use librellog::ast::BinOpSymbol;
use librellog::init_interner;
use librellog::{
    ast::{Clause, Item, Module, RcTm, Rel, Tm},
    data_structures::Var,
    interner::IStr,
    lex::tok::Tok,
};
use nom::{Finish, Parser};

pub trait Compile<Compiler> {
    fn compile(&self, f: &mut dyn std::io::Write, compiler: &mut Compiler) -> io::Result<()>;
}

pub struct SwiProlog {}

impl Compile<SwiProlog> for Module {
    fn compile(&self, f: &mut dyn std::io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        for item in &self.items {
            match item {
                Item::RelDef(head, opt_body) => {
                    let clause = Clause {
                        head: head.clone(),
                        body: opt_body.clone(),
                    };
                    clause.compile(f, compiler)?;
                    writeln!(f)?;
                    writeln!(f)?;
                }
                Item::Directive(directive) => {
                    write!(f, "/* :- ")?;
                    compiler.compile_rel(f, directive)?;
                    writeln!(f, ". */")?;
                }
            }
        }
        Ok(())
    }
}

impl Compile<SwiProlog> for Clause {
    fn compile(&self, f: &mut dyn Write, compiler: &mut SwiProlog) -> io::Result<()> {
        compiler.compile_rel(f, &self.head)?;

        match self.body {
            None => {}
            Some(ref body) => {
                writeln!(f, " :-")?;
                write!(f, "\t")?;
                body.compile(f, compiler)?;
            }
        }

        write!(f, ".")
    }
}

impl Compile<SwiProlog> for RcTm {
    fn compile(&self, f: &mut dyn Write, compiler: &mut SwiProlog) -> io::Result<()> {
        match &**self {
            Tm::Nil => write!(f, "[]")?,
            Tm::Cons(head, tail) => {
                write!(f, "[")?;
                head.compile(f, compiler)?;
                write!(f, " | ")?;
                tail.compile(f, compiler)?;
                write!(f, "]")?;
            }
            Tm::Rel(rel) => {
                compiler.compile_rel(f, rel)?;
            }
            Tm::Sym(sym) => write!(f, "'{}'", sym)?,
            Tm::Block(Tok::Dash, members) => {
                // conjunction of members
                for (i, member) in members.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ",")?;
                        write!(f, "\t")?;
                    }
                    member.compile(f, compiler)?;
                }
            }
            Tm::Block(..) => unimplemented!(),
            Tm::Var(var) => {
                let mangled = compiler.mangle_var(var);
                write!(f, "{mangled}")?;
            }
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

impl SwiProlog {
    fn mangle_sig<'a>(&'a self, iter: impl Iterator<Item = &'a IStr>) -> String {
        use std::fmt::Write;
        let mut v: Vec<_> = iter.collect();
        v.sort();
        let mut buf = String::new();
        for key in v {
            write!(buf, "[{key}]").unwrap();
        }
        buf
    }

    fn compile_rel(&mut self, f: &mut dyn Write, rel: &Rel) -> io::Result<()> {
        // First sort by keys.
        let rel = {
            let mut rel = rel.iter().collect::<Vec<_>>();
            rel.sort_by_key(|(key, _)| *key);
            rel
        };
        let pred_name = self.mangle_sig(rel.iter().map(|(key, _)| *key));
        write!(f, "'{pred_name}'(")?;
        for (i, value) in rel.into_iter().map(|pair| pair.1).enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            value.compile(f, self)?;
        }
        write!(f, ")")
    }

    fn mangle_var(&self, var: &Var) -> String {
        let Var { name, suffix, .. } = var.clone();
        match suffix {
            None => format!("{name}"),
            Some(suffix) => format!("{name}_{suffix}"),
        }
    }
}

fn main() {
    init_interner();
    // get path from argv[1]
    let path = std::env::args().nth(1).unwrap();
    // Read the input file
    let mut buf = String::new();
    std::fs::File::open(&path)
        .unwrap()
        .read_to_string(&mut buf)
        .unwrap();
    // tokenize
    let src = nom_locate::LocatedSpan::new(&buf[..]);
    let toks = librellog::lex::tokenize(src, path.into()).unwrap();
    // parse
    let (rest, module) = librellog::parse::module
        .parse(toks[..].into())
        .finish()
        .unwrap();

    assert!(rest.is_empty(), "Could not parse entire input: {:?}", rest);

    module
        .compile(&mut std::io::stdout().lock(), &mut SwiProlog {})
        .unwrap();
}
