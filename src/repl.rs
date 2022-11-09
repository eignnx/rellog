use std::{
    cell::RefCell,
    io::{self, Read},
    process::exit,
};

use librellog::{
    ast::{self, dup::TmDuplicator},
    lex::{
        self,
        tok::{At, Tok},
    },
    parse,
    rt::{self, UnifierSet},
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
    module: ast::Module,
    config: RellogReplConfigHandle,
    line_editor: Reedline,
}

impl Repl {
    pub fn loading_file(fname: &str) -> Self {
        let config = RellogReplConfigHandle::default();
        let mut tok_buf = Vec::new();

        let module = match load_module_from_file(&mut tok_buf, fname) {
            Ok(module) => module,
            Err(e) => {
                println!("{e}");
                println!("Loading default module.");
                ast::Module::default()
            }
        };

        Self {
            current_file: fname.to_owned(),
            module,
            line_editor: config.create_editor(),
            config,
        }
    }

    pub fn run(&mut self) -> ! {
        let mut tok_buf = Vec::new();

        'outer: loop {
            self.config.set_repl_mode(ReplMode::TopLevel);
            let query_buf = match self.line_editor.read_line(&self.config) {
                Ok(Signal::Success(s)) => s,
                Ok(Signal::CtrlC | Signal::CtrlD) => exit(0),
                Err(e) => {
                    println!("Unable to read line from terminal: {e}");
                    println!("Exiting...");
                    exit(0);
                }
            };

            match query_buf.trim() {
                "help" | ":help" | ":h" | "?" => {
                    println!("Enter a RELLOG TERM or one of these REPL COMMANDS:");
                    println!("  :h | :help | ?    Displays this help text.");
                    println!("  :r | :reload      Reloads the source file.");
                    continue 'outer;
                }
                ":reload" | ":r" => {
                    println!("Reloading source from {}...", self.current_file);
                    self.module = match load_module_from_file(&mut tok_buf, &self.current_file) {
                        Ok(m) => {
                            println!("{} relation definitions loaded.", m.relations.len());
                            m
                        }
                        Err(e) => {
                            println!("Error loading file: {e}");
                            println!("Defaulting to empty module.");
                            Default::default()
                        }
                    };
                    continue 'outer;
                }
                _ => {}
            }

            let mut buf = Vec::new();
            let tokens = lex::tokenize_into(&mut buf, &query_buf[..]);

            let query = match parse::entire_term(tokens) {
                Ok(q) => q,
                Err(e) => {
                    println!("Parse error:");
                    parse::display_parse_err(&e);
                    continue 'outer;
                }
            };

            let rt = rt::Rt::new(&self.module);
            let u = UnifierSet::new();
            let td = RefCell::new(TmDuplicator::default());
            let solns = rt.solve_query(query, u, &td);

            self.config.set_repl_mode(ReplMode::PrintingSolns);
            for soln in solns {
                match soln {
                    Ok(soln) => print!("{}", DisplayUnifierSet(soln)),
                    Err(e) => {
                        println!("{}", Color::Red.paint(format!("Exception: {e}")));
                        continue 'outer;
                    }
                }

                match self.line_editor.read_line(&self.config).unwrap() {
                    Signal::Success(_) => {}
                    Signal::CtrlC => {
                        println!("...");
                        continue 'outer;
                    }
                    Signal::CtrlD => exit(0),
                };
            }

            println!("    - [false]");
        }
    }
}

fn load_module_from_file<'ts>(
    tok_buf: &'ts mut Vec<At<Tok>>,
    fname: &str,
) -> AppRes<'ts, ast::Module> {
    let f = std::fs::File::open(fname).map_err(|e| AppErr::FileOpen(fname.into(), e))?;
    let mut r = io::BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src)
        .map_err(|e| AppErr::FileRead(fname.into(), e))?;

    let tokens = lex::tokenize_into(tok_buf, &*src);

    let m = match parse::entire_module(tokens) {
        Ok(m) => m,
        Err(verbose_err) => {
            parse::display_parse_err(&verbose_err);
            return Err(AppErr::Parse(verbose_err));
        }
    };

    Ok(m)
}
