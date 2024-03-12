use std::{
    cell::RefCell,
    collections::BTreeSet,
    path::{Path, PathBuf},
};

use crate::{
    ast::{dup::TmDuplicator, tm_displayer::TmDisplayer, BinOpSymbol, Clause, RcTm, Sig, Tm},
    data_structures::{Sym, Var},
    rel_match,
    rt::{self, kb::KnowledgeBase, soln_stream::SolnStream, Rt, UnifierSet},
    tm,
};

pub struct Session {
    pub rt: Rt,
    pub macro_rt: Rt,
    pub fs_root: PathBuf,
    pub loaded_files: BTreeSet<PathBuf>,
}

impl Session {
    /// Create a new session with the given file system root.
    pub fn new(fs_root: impl AsRef<Path>) -> rt::Res<Self> {
        let mut macro_kb = KnowledgeBase::default();
        for file in crate::STD_LIB_DEPS_NO_MACROS {
            macro_kb.include(file)?;
        }
        let macro_rt = Rt::new(macro_kb);

        Ok(Self {
            rt: Rt::new(KnowledgeBase::default()),
            macro_rt,
            loaded_files: BTreeSet::new(),
            fs_root: PathBuf::from(fs_root.as_ref()),
        })
    }

    pub fn solve_query<'sess>(
        &'sess self,
        query: RcTm,
        u: UnifierSet,
        td: &'sess RefCell<TmDuplicator>,
    ) -> impl SolnStream + 'sess {
        self.rt.solve_query(query, u, td)
    }

    const DEBUG_MACROS: bool = false;

    pub fn load_file(&mut self, path: PathBuf) -> rt::Res<(usize, usize)> {
        if Self::DEBUG_MACROS {
            self.macro_rt.debug_mode.set(true);
            self.macro_rt
                .debugger
                .borrow_mut()
                .replace(Box::new(MacroDebugger::new()));
        }

        let mut new_kb = KnowledgeBase::default();
        new_kb.include(path.as_path())?;

        let dirs_to_process = new_kb.directives.clone();
        for dir in dirs_to_process {
            let dir_tm: RcTm = Tm::Rel(dir.query.clone()).into();
            let directive_info = tm!([next_rel_def
                match dir.next_rel_def.clone() {
                    Some(Clause { head, body}) => {
                        tm!([clause_head
                            RcTm::from(head)
                        ][clause_body
                            body.unwrap_or(RcTm::rel_true())
                        ]).into()
                    }
                    None => Tm::Nil.into()
                }
            ])
            .into();

            let rule = Tm::BinOp(
                BinOpSymbol::Tilde,
                dir_tm.clone(),
                tm!([macro directive_info]).into(),
            )
            .into();

            let before: RcTm = Tm::Nil.into();
            let after: RcTm = Tm::Var(Var::from_source("After", None)).into();
            let r#impl = Tm::Sym(Sym::from("impl")).into();

            // Treat each directive as a DCG rule in the context of the macro knowledge base.
            let query: RcTm = tm!(
                [impl r#impl][rule rule][before before][after after.clone()]
            )
            .into();

            let u = UnifierSet::new();
            let td = RefCell::new(TmDuplicator::default());
            let solns = self.macro_rt.solve_query(query, u, &td);

            let mut no_solns_found = true;
            for soln in solns {
                no_solns_found = false;
                process_soln(soln, &mut new_kb, &dir_tm, &after)?;
            }
            if no_solns_found {
                return Err(rt::Err::MacroExpansionError {
                    macro_name: dir_tm.to_string(),
                    err: Box::new(rt::Err::NoSuchMacro(Sig::from_rel(&dir.query))),
                });
            }
        }

        let new_rels = new_kb.relations.len();
        let new_dirs = new_kb.directives.len();

        self.rt.db.import(new_kb);
        self.loaded_files.insert(path);

        Ok((new_rels, new_dirs))
    }
}

fn process_soln(
    soln: rt::Res<UnifierSet>,
    new_kb: &mut KnowledgeBase,
    dir: &RcTm,
    after: &RcTm,
) -> rt::Res<()> {
    let u = soln.map_err(|e| rt::Err::MacroExpansionError {
        macro_name: dir.to_string(),
        err: Box::new(e),
    })?;

    let after = u.reify_term(after);
    let Some((clauses, _tail)) = after.try_as_list() else {
        return Err(rt::Err::MacroImplemenationError {
            macro_name: dir.to_string(),
            msg: format!("Macro implementation returned a non-list term: {}", after),
        });
    };

    for clause in clauses.iter() {
        let clause_as_rel =
            clause
                .try_as_rel()
                .ok_or_else(|| rt::Err::MacroImplemenationError {
                    macro_name: dir.to_string(),
                    msg: format!(
                        "Macro implementation returned an invalid clause: {}",
                        clause
                    ),
                })?;

        rel_match!(clause_as_rel, {
            [fact = rel_to_assert] => {
                assert_fact_clause(dir, new_kb, rel_to_assert)?;
                Ok(())
            },
            [head = head_tm][body = body_tm] => {
                assert_head_body_clause(dir, new_kb, head_tm, body_tm)?;
                Ok(())
            },
            else => {
                Err(rt::Err::MacroImplemenationError {
                    macro_name: dir.to_string(),
                    msg: format!(
                        "Macro implementation returned an invalid clause: {}",
                        clause_as_rel
                    ),
                })
            }
        })?;
    }

    Ok(())
}

fn assert_fact_clause(macro_name: &RcTm, kb: &mut KnowledgeBase, rel_tm: &RcTm) -> rt::Res<()> {
    let rel = rel_tm
        .try_as_rel()
        .ok_or_else(|| rt::Err::MacroImplemenationError {
            macro_name: macro_name.to_string(),
            msg: format!("[Fact] contained the non-rel term {}", rel_tm),
        })?;

    kb.relations
        .entry(Sig::from_rel(rel))
        .or_default()
        .push(Clause {
            head: rel.clone(),
            body: None,
        });

    Ok(())
}

fn assert_head_body_clause(
    macro_name: &RcTm,
    kb: &mut KnowledgeBase,
    head_tm: &RcTm,
    body: &RcTm,
) -> rt::Res<()> {
    let head = head_tm
        .try_as_rel()
        .ok_or_else(|| rt::Err::MacroImplemenationError {
            macro_name: macro_name.to_string(),
            msg: format!("[Head] contained the non-rel term {}", head_tm),
        })?;

    kb.relations
        .entry(Sig::from_rel(head))
        .or_default()
        .push(Clause {
            head: head.clone(),
            body: Some(body.clone()),
        });

    Ok(())
}

struct MacroDebugger;

impl MacroDebugger {
    fn new() -> Self {
        Self
    }
}

impl rt::breakpoint::Breakpoint for MacroDebugger {
    fn breakpoint(&mut self, rt: &Rt, title: rt::breakpoint::Event) {
        if !rt.debug_mode.get() {
            return;
        }

        let mut src_buf = String::new();
        loop {
            eprint!("[macro debugger]=> ");
            std::io::stdin().read_line(&mut src_buf).unwrap();
            let src_buf = src_buf.trim();

            match src_buf {
                "" => {
                    // Step.
                    break;
                }
                "q" => {
                    eprintln!("Exiting debugger...");
                    rt.debug_mode.set(false);
                    break;
                }
                "x" => {
                    eprintln!("Exiting...");
                    std::process::exit(0);
                }
                _ => {
                    eprintln!("Command not recognized: {}", src_buf);
                }
            }
        }

        let query = rt.current_query();
        eprintln!(">>>>>>>>> Macro Breakpoint <<<<<<<<<");
        eprintln!("Event type: {title}");
        eprintln!("Query: {}", TmDisplayer::default().indenting(&query));
        eprintln!(">>>>>>>>> End Macro Breakpoint <<<<<<<<<");
        eprintln!();
    }
}
