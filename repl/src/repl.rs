use std::{
    cell::RefCell,
    io::{self, Read},
    path::PathBuf,
};

use librellog::{
    ast::{self, dup::TmDuplicator},
    lex::{
        self,
        tok::{At, Tok},
    },
    parse,
    rt::{Rt, UnifierSet},
    utils::display_unifier_set::DisplayUnifierSet,
};
use nu_ansi_term::Color;
use reedline::{Reedline, Signal};

use crate::{
    app_err::{AppErr, AppRes},
    line_editor_config::{RellogReplConfigHandle, ReplMode},
};

pub struct Repl {
    current_file: String,
    // module: KnowledgeBase,
    config: RellogReplConfigHandle,
    line_editor: Reedline,
    rt: Rt,
}

impl Repl {
    pub fn loading_std_lib() -> Self {
        Self::loading_file("../librellog/src/std.rellog")
    }

    pub fn loading_file(fname: &str) -> Self {
        let config = RellogReplConfigHandle::default();
        let mut tok_buf = Vec::new();

        let module = match load_module_from_file(&mut tok_buf, fname) {
            Ok(module) => module,
            Err(e) => {
                println!("Error loading module `{fname}`:");
                println!("    {e}");
                ast::Module::default()
            }
        };
        let rt = Rt::new(module.clone());

        Self {
            current_file: fname.to_owned(),
            // module,
            line_editor: config.create_editor(),
            config,
            rt,
        }
    }

    pub fn load_file<'ts>(
        &mut self,
        tok_buf: &'ts mut Vec<At<Tok>>,
        fname: impl AsRef<str>,
    ) -> AppRes<'ts, ()> {
        let loaded_module = load_module_from_file(tok_buf, fname)?;
        self.rt.db.import(loaded_module);
        Ok(())
    }

    pub fn run(&mut self) {
        let mut tok_buf = Vec::new();

        'outer: loop {
            self.config.set_repl_mode(ReplMode::TopLevel);
            let query_buf = match self.line_editor.read_line(&self.config) {
                Ok(Signal::Success(s)) => s,
                Ok(Signal::CtrlC | Signal::CtrlD) => break 'outer,
                Err(e) => {
                    println!("Unable to read line from terminal: {e}");
                    println!("Exiting...");
                    break 'outer;
                }
            };

            let query_parts: Vec<&str> = query_buf.trim().split_ascii_whitespace().collect();

            match query_parts[..] {
                ["help" | ":help" | ":h" | "?"] => {
                    println!("Enter a RELLOG TERM or one of these REPL COMMANDS:");
                    println!("  :h | :help | ?    Displays this help text.");
                    println!("  :r | :reload      Reloads the source file.");
                    println!("  [Builtins]        Show a list of builtin relations.");
                    println!("  [Sig][Help]       Show help text for a relation given by `Sig`.");
                    continue 'outer;
                }
                [":quit" | ":q" | ":exit" | ":wq"] => {
                    println!("Exiting Rellog REPL...");
                    break 'outer;
                }
                [":reload" | ":r"] => {
                    println!(
                        "{}",
                        Color::Yellow
                            .paint(format!("# Reloading source from {}...", self.current_file))
                    );
                    self.rt.db = match load_module_from_file(&mut tok_buf, &self.current_file) {
                        Ok(m) => {
                            println!(
                                "{}",
                                Color::Yellow.paint(format!(
                                    "# {} relation definitions loaded.",
                                    m.relations.len()
                                ))
                            );
                            m.into()
                        }
                        Err(e) => {
                            println!("{}", Color::Red.paint(format!("# Error loading file: {e}")));
                            println!("{}", Color::Red.paint("# Defaulting to empty module."));
                            Default::default()
                        }
                    };
                    continue 'outer;
                }
                [":load" | ":l"] => {
                    println!(
                        "# Which file would you like to load? Please specify `{query_buf} FILENAME`"
                    );
                    continue 'outer;
                }
                [":load" | ":l", fname] => {
                    let mut tok_buf = Vec::new();
                    match self.load_file(&mut tok_buf, fname) {
                        Ok(()) => println!("# Loaded `{fname}`."),
                        Err(e) => {
                            println!("# Could not load file `{fname}`: {e}");
                        }
                    }
                    continue 'outer;
                }
                _ => {}
            }

            let mut buf = Vec::new();
            let tokens = match lex::tokenize_into(&mut buf, &query_buf[..], "<user input>".into()) {
                Ok(ts) => ts,
                Err(le) => {
                    println!("{}", Color::Red.paint(format!("Tokenization error: {le}")));
                    continue 'outer;
                }
            };

            let mut no_solns_yet_found = true;

            let query = match parse::entire_term(tokens) {
                Ok(q) => q,
                Err(e) => {
                    println!("Parse error: {e}");
                    continue 'outer;
                }
            };

            let u = UnifierSet::new();
            let td = RefCell::new(TmDuplicator::default());
            let mut solns = self.rt.solve_query(query, u, &td);
            let mut or_bar_printed = false;

            self.config.set_repl_mode(ReplMode::PrintingSolns);

            while let Some(soln) = solns.next() {
                let soln = match soln {
                    Ok(soln) => {
                        let disp = DisplayUnifierSet {
                            u: soln.clone(),
                            display_or_bar: or_bar_printed,
                        };
                        print!("{disp}");
                        no_solns_yet_found = false;
                        soln
                    }
                    Err(e) => {
                        println!("{}", Color::Red.paint(format!("Exception: {e}")));
                        continue 'outer;
                    }
                };

                // If we *know* there are no more solutions...
                match solns.size_hint() {
                    (_, Some(0)) if soln.is_empty() => {
                        let msg = Color::Yellow.paint("# The query holds unconditionally.");
                        println!(" {msg}");
                        continue 'outer;
                    }
                    (_, Some(0)) => {
                        let msg = Color::Yellow.paint("# Exactly 1 solution found.");
                        println!(" {msg}");
                        continue 'outer;
                    }
                    (lo, Some(hi)) if lo == hi => {
                        let msg = Color::Yellow.paint(format!("# {lo} solution(s) remain."));
                        println!(" {msg}");
                    }
                    (_, Some(hi)) => {
                        let msg =
                            Color::Yellow.paint(format!("# At most {hi} solution(s) remain."));
                        println!(" {msg}");
                    }
                    _ => {}
                }

                match self.line_editor.read_line(&self.config).unwrap() {
                    Signal::Success(_) => {}
                    Signal::CtrlC => {
                        println!("...");
                        continue 'outer;
                    }
                    Signal::CtrlD => break 'outer,
                }

                print!("\n|"); // Print an "or"; more solns incoming.
                or_bar_printed = true;
            }

            if !or_bar_printed {
                print!(" ");
            }

            if no_solns_yet_found {
                println!(
                    "   - [false] {}",
                    Color::Yellow.paint("# The query has no solutions.")
                );
            } else {
                println!(
                    "   - [false] {}",
                    Color::Yellow.paint("# No additional solutions found.")
                );
            }
        }
    }
}

fn load_module_from_file(
    tok_buf: &mut Vec<At<Tok>>,
    fname: impl AsRef<str>,
) -> AppRes<ast::Module> {
    let f = std::fs::File::open(fname.as_ref())
        .map_err(|e| AppErr::FileOpen(fname.as_ref().into(), e))?;
    let mut r = io::BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src)
        .map_err(|e| AppErr::FileRead(fname.as_ref().into(), e))?;
    load_module_from_string(tok_buf, src, fname.as_ref().into())
}

fn load_module_from_string(
    tok_buf: &mut Vec<At<Tok>>,
    src: impl AsRef<str>,
    filename: PathBuf,
) -> AppRes<ast::Module> {
    let tokens = lex::tokenize_into(tok_buf, src.as_ref(), filename)?;

    let m = match parse::entire_module(tokens) {
        Ok(m) => m,
        Err(verbose_err) => {
            println!("{verbose_err}");
            return Err(AppErr::Parse(verbose_err));
        }
    };

    Ok(m)
}
