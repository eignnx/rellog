use std::io;

use librellog::ast::Clause;

use crate::{Compile, SwiProlog};

impl Compile<SwiProlog> for Clause {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
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
