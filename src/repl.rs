use std::{
    cell::RefCell,
    io::{self, Read},
    process::exit,
};

use librellog::{
    ast::{self, dup::TmDuplicator, Module},
    lex::{
        self,
        tok::{At, Tok},
    },
    parse,
    rt::{self, Rt, UnifierSet},
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
    rt: Rt,
}

impl Repl {
    pub fn loading_std_lib() -> Self {
        let mut tok_buf = Vec::new();
        let std_lib =
            load_module_from_string(&mut tok_buf, include_str!("../librellog/src/std.rellog"))
                .expect("Could not parse `std.rellog`!");
        let config = RellogReplConfigHandle::default();
        let rt = Rt::new(std_lib.clone());
        Self {
            current_file: "<repl>".to_owned(),
            module: std_lib,
            line_editor: config.create_editor(),
            config,
            rt,
        }
    }

    #[allow(dead_code)]
    pub fn without_loading_file() -> Self {
        let config = RellogReplConfigHandle::default();
        let rt = Rt::new(Module::default());
        Self {
            current_file: "<repl>".to_owned(),
            module: Default::default(),
            line_editor: config.create_editor(),
            config,
            rt,
        }
    }

    #[allow(dead_code)]
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
        let rt = Rt::new(module.clone());

        Self {
            current_file: fname.to_owned(),
            module,
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
        self.module.import(loaded_module);
        Ok(())
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

            let query_parts: Vec<&str> = query_buf.trim().split_ascii_whitespace().collect();

            match &query_parts[..] {
                &["help" | ":help" | ":h" | "?"] => {
                    println!("Enter a RELLOG TERM or one of these REPL COMMANDS:");
                    println!("  :h | :help | ?    Displays this help text.");
                    println!("  :r | :reload      Reloads the source file.");
                    println!("  [Builtins]        Show a list of builtin relations.");
                    println!("  [Sig][Help]       Show help text for a relation given by `Sig`.");
                    continue 'outer;
                }
                &[":reload" | ":r"] => {
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
                    self.rt = Rt::new(self.module.clone());
                    continue 'outer;
                }
                &[":load" | ":l"] => {
                    println!(
                        "Which file would you like to load? Please specify `{query_buf} FILENAME`"
                    );
                    continue 'outer;
                }
                &[":load" | ":l", fname] => {
                    let mut tok_buf = Vec::new();
                    match self.load_file(&mut tok_buf, fname) {
                        Ok(()) => println!("Loaded `{fname}`."),
                        Err(e) => {
                            println!("Could not load file `{fname}`: {e}");
                        }
                    }
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

            let u = UnifierSet::new();
            let td = RefCell::new(TmDuplicator::default());
            let solns = self.rt.solve_query(query, u, &td);

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
    fname: impl AsRef<str>,
) -> AppRes<'ts, ast::Module> {
    let f = std::fs::File::open(fname.as_ref())
        .map_err(|e| AppErr::FileOpen(fname.as_ref().into(), e))?;
    let mut r = io::BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src)
        .map_err(|e| AppErr::FileRead(fname.as_ref().into(), e))?;
    load_module_from_string(tok_buf, src)
}

fn load_module_from_string<'ts>(
    tok_buf: &'ts mut Vec<At<Tok>>,
    src: impl AsRef<str>,
) -> AppRes<'ts, ast::Module> {
    let tokens = lex::tokenize_into(tok_buf, src.as_ref());

    let m = match parse::entire_module(tokens) {
        Ok(m) => m,
        Err(verbose_err) => {
            parse::display_parse_err(&verbose_err);
            return Err(AppErr::Parse(verbose_err));
        }
    };

    Ok(m)
}
