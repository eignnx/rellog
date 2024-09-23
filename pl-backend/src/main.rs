use std::{
    collections::{BTreeMap, HashMap},
    io::{self, Read, Write},
};

use nom::{Finish, Parser};

use librellog::{
    ast::{Rel, Sig},
    data_structures::Sym,
    init_interner,
    interner::IStr,
};

mod clause;
mod module;
mod tm;

pub trait Compile<Compiler> {
    fn compile(&self, f: &mut dyn std::io::Write, compiler: &mut Compiler) -> io::Result<()>;
}

pub struct SwiProlog {
    rel_map: HashMap<RelId, RelInfo>,
    tm_is_callable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RelId(Sig);

impl RelId {
    pub fn from_sig(sig: &Sig) -> Self {
        Self(sig.clone())
    }

    pub fn to_sym(&self, arg_order: &ArgOrder) -> Sym {
        use std::fmt::Write;
        match arg_order {
            ArgOrder::RellogOrder => {
                let mut s = String::new();
                s.push('\'');
                for key in self.0.keys() {
                    write!(s, "[{}]", key).unwrap();
                }
                s.push('\'');
                Sym::from(&s[..])
            }
            ArgOrder::Translated { pred_name, .. } => *pred_name,
        }
    }
}

impl Compile<SwiProlog> for RelId {
    fn compile(&self, f: &mut dyn Write, _: &mut SwiProlog) -> io::Result<()> {
        write!(f, "'")?;
        for key in self.0.keys() {
            write!(f, "[{}]", key)?;
        }
        write!(f, "'")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum ArgOrder {
    RellogOrder,
    Translated { pred_name: Sym, arg_order: Vec<Sym> },
}

struct RelInfo {
    sig: Sig,
    pred_arg_order: ArgOrder,
}

impl RelInfo {
    pub fn pred_name(&self) -> Sym {
        RelId(self.sig.clone()).to_sym(&self.pred_arg_order)
    }
}

impl SwiProlog {
    fn mangle_sig<'a>(&'a self, iter: impl Iterator<Item = &'a IStr>) -> RelId {
        RelId::from_sig(&Sig::from(iter.cloned()))
    }

    fn compile_rel(&mut self, f: &mut dyn Write, rel: &Rel) -> io::Result<()> {
        // First sort by keys.
        let rel = {
            let mut rel = rel.iter().collect::<Vec<_>>();
            rel.sort_by_key(|(key, _)| *key);
            rel
        };
        let pred_name = self.mangle_sig(rel.iter().map(|(key, _)| *key));
        pred_name.compile(f, self)?;
        write!(f, "(")?;
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

    let mut compiler = SwiProlog {
        rel_map: HashMap::new(),
        tm_is_callable: false,
    };

    module
        .compile(&mut std::io::stdout().lock(), &mut compiler)
        .unwrap();
}
