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

        if let Some(ref body) = self.body {
            if compiler
                .dcg_rules
                .contains(&RelId(Sig::from_rel(&self.head)))
            {
                writeln!(f, " -->")?;
            } else {
                writeln!(f, " :-")?;
            }

            write!(f, "\t")?;
            compiler.tm_is_callable = true;
            body.compile(f, compiler)?;
            compiler.tm_is_callable = false;
        }

        write!(f, ".")?;

        Ok(())
    }
}
