use std::io;

use librellog::{
    ast::{BinOpSymbol, Clause, Item, Module, RcTm, Rel, Tm},
    data_structures::Sym,
    lex::tok::Tok,
    rel_match,
};

use crate::{Compile, SwiProlog};

impl Compile<SwiProlog> for Module {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        for (item_idx, item) in self.items.iter().enumerate() {
            match item {
                Item::RelDef(..) => {}
                Item::Directive(rel) => {
                    Directive {
                        rel: rel.clone(),
                        following_clause: self.items.iter().skip(item_idx + 1).find_map(|item| {
                            match item {
                                Item::RelDef(head, opt_body) => Some(Clause {
                                    head: head.clone(),
                                    body: opt_body.clone(),
                                }),
                                _ => None,
                            }
                        }),
                    }
                    .compile(f, compiler)?;
                    writeln!(f)?;
                }
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
                Item::Directive(directive) => {}
            }
        }
        Ok(())
    }
}

struct Directive {
    rel: Rel,
    following_clause: Option<Clause>,
}

impl Compile<SwiProlog> for Directive {
    fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
        rel_match!(self.rel, {
            [modname = modname] => {
                impl_mod(modname, f, compiler)?;
            },
            [deps = deps] => {
                impl_deps(deps, f, compiler)?;
            },
            else => {
                write!(f, "/* :- ")?;
                compiler.compile_rel(f, &self.rel)?;
                writeln!(f, ". */")?;
            }
        });
        Ok(())
    }
}

/// Example
/// ```text
/// [[mod my_module_name]]
/// ```
fn impl_mod(modname: &RcTm, f: &mut dyn io::Write, _compiler: &mut SwiProlog) -> io::Result<()> {
    if let Tm::Sym(modname) = modname.as_ref() {
        writeln!(f, ":- module({modname}, []).")?;
    } else {
        panic!("Module name must be a symbol");
    }

    Ok(())
}

/// # Example
/// ```text
/// [[deps
///     - prolog::dcgs::basics::
///         - [pred list_to_set][args {list set}]
///         - [pred numlist][args {low high range_list}]
///     - my_mod::my_submod::
///         - [My][Rel]
/// ]]
/// ```
fn impl_deps(deps: &RcTm, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
    type Path = Vec<Sym>;

    struct DepTree {
        path: Path,
        imports: Vec<Import>,
    }

    enum Import {
        Pred {
            pred_name: Sym,
            arg_bindings: Vec<Sym>,
        },
        Rel(Rel),
    }

    fn extract_prolog_library_specifier(mut raw_dep: &RcTm) -> Result<DepTree, String> {
        let mut path: Path = Vec::new();
        loop {
            match raw_dep.as_ref() {
                Tm::Sym(libname) => {
                    path.push(*libname);
                    return Ok(DepTree {
                        path,
                        imports: Vec::new(),
                    });
                }
                Tm::BinOp(BinOpSymbol::PathSep, parent, child) => {
                    let Tm::Sym(parent) = parent.as_ref() else {
                        return Err(format!("Path segment must be a symbol: {}", parent));
                    };
                    path.push(*parent);
                    raw_dep = child;
                }
                Tm::Block(Tok::Dash, imports) => {
                    let mut dep_tree = DepTree {
                        path,
                        imports: Vec::new(),
                    };
                    for import in imports {
                        let Tm::Rel(rel) = import.as_ref() else {
                            return Err(format!("Import must be a relation: {}", import));
                        };
                        rel_match!(rel, {
                            [pred = pred_name][args = args] => {
                                let Tm::Sym(pred_name) = pred_name.as_ref() else {
                                    return Err(format!("Predicate name must be a symbol: {}", pred_name));
                                };
                                let Some((args, None)) = args.try_as_list() else {
                                    return Err(format!("Arguments must be a list: {}", args));
                                };
                                let Some(args) = args.into_iter().map(RcTm::try_as_sym).collect::<Option<Vec<_>>>() else {
                                    return Err(format!("Arguments must be symbols: {}", args));
                                };
                                dep_tree.imports.push(Import::Pred {
                                    pred_name: *pred_name,
                                    arg_bindings: args,
                                });
                            },
                            else => {
                                dep_tree.imports.push(Import::Rel(rel.clone()));
                            }
                        });
                    }
                    return Ok(dep_tree);
                }
                _ => {
                    return Err(format!("Invalid library dependency term {}", raw_dep));
                }
            }
        }
    }

    impl Compile<SwiProlog> for DepTree {
        fn compile(&self, f: &mut dyn io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
            if self.path.first() == Some(&Sym::from("prolog")) {
                write!(f, ":- use_module(library(")?;
                for (i, path_segment) in self.path.iter().skip(1).enumerate() {
                    if i > 0 {
                        write!(f, "/")?;
                    }
                    write!(f, "{}", path_segment)?;
                }
                write!(f, ")")?;
            } else {
                write!(f, ":- use_module(")?;
                for (i, path_segment) in self.path.iter().enumerate() {
                    if i > 0 {
                        write!(f, "/")?;
                    }
                    write!(f, "{}", path_segment)?;
                }
            }

            if self.imports.is_empty() {
                writeln!(f, ").")?;
            } else {
                writeln!(f, ", [")?;
                for (i, import) in self.imports.iter().enumerate() {
                    if i > 0 {
                        writeln!(f, ",")?;
                    }
                    write!(f, "\t")?;
                    import.compile(f, compiler)?;
                }
                writeln!(f, "\n]).")?;
            }

            Ok(())
        }
    }

    impl Compile<SwiProlog> for Import {
        fn compile(&self, f: &mut dyn std::io::Write, compiler: &mut SwiProlog) -> io::Result<()> {
            match self {
                Import::Rel(rel) => {
                    compiler.compile_rel(f, rel)?;
                }
                Import::Pred {
                    pred_name,
                    arg_bindings,
                } => {
                    let arity = arg_bindings.len();
                    let mangled = compiler.mangle_sig(arg_bindings.iter());
                    write!(f, "{pred_name}/{arity} as '{mangled}'")?;
                }
            }
            Ok(())
        }
    }

    match deps.as_ref() {
        Tm::Block(Tok::Dash, deps) => {
            for dep in deps {
                let dep_tree = match extract_prolog_library_specifier(dep) {
                    Ok(dep_tree) => dep_tree,
                    Err(err) => {
                        panic!("Error parsing dependency: {}", err);
                    }
                };
                dep_tree.compile(f, compiler).unwrap();
            }
        }
        _ => {
            panic!("Dependencies must be a `-` block of imports");
        }
    }

    Ok(())
}
