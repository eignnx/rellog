use std::io;

use librellog::ast::{Clause, Item, Module, Tm};

use crate::{Compile, SwiProlog};

impl Compile<SwiProlog> for Module {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        let mut modnames = self.items.iter().filter_map(|item| match item {
            Item::Directive(dir) => match dir.iter().collect::<Vec<_>>().as_slice() {
                [(key, value)] if &key.to_str()[..] == "mod" => Some((*value).clone()),
                _ => None,
            },
            _ => None,
        });

        if let Some(modname) = modnames.next() {
            if let Some(modname) = modnames.next() {
                panic!("Multiple module names found: {:?}", modname);
            } else if let Tm::Sym(modname) = modname.as_ref() {
                writeln!(f, ":- module({modname}, []).")?;
                writeln!(f)?;
            } else {
                panic!("Module name must be a symbol");
            }
        }

        writeln!(f, ":- set_prolog_flag(double_quotes, chars).")?;
        writeln!(f, ":- use_module(shim/lang_shim).")?;
        writeln!(f)?;

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
