use std::io;

use librellog::ast::{Clause, Sig};

use crate::{ArgOrder, Compile, RelId, RelInfo, SwiProlog};

impl Compile<SwiProlog> for Clause {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        compiler.compile_rel(f, &self.head)?;

        let sig = Sig::from(self.head.keys().cloned());
        let rel_id = RelId::from_sig(&sig);
        compiler.rel_map.insert(
            rel_id,
            RelInfo {
                sig,
                pred_arg_order: ArgOrder::RellogOrder,
            },
        );

        match self.body {
            None => {}
            Some(ref body) => {
                writeln!(f, " :-")?;
                write!(f, "\t")?;
                compiler.tm_is_callable = true;
                body.compile(f, compiler)?;
                compiler.tm_is_callable = false;
            }
        }

        write!(f, ".")?;

        Ok(())
    }
}
