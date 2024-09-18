use std::io::{self, Read, Write};

use nom::{Finish, Parser};

use librellog::{ast::Rel, init_interner, interner::IStr};

mod clause;
mod module;
mod tm;

pub trait Compile<Compiler> {
    fn compile(&self, f: &mut dyn std::io::Write, compiler: &mut Compiler) -> io::Result<()>;
}

pub struct SwiProlog {}

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
