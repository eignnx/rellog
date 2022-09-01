use std::{
    io::{self, Read},
    process::exit,
};

use librellog::{
    ast, lex,
    my_nom::Span,
    parse,
    rt::{self, UnifierSet},
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

        let module = match load_module_from_file(fname) {
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
        'outer: loop {
            self.config.set_repl_mode(ReplMode::TopLevel);
            let query_buf = match self.line_editor.read_line(&self.config) {
                Ok(Signal::Success(s)) => s,
                Ok(Signal::CtrlC | Signal::CtrlD) => exit(0),
                Err(e) => {
                    println!("{e}");
                    exit(0);
                }
            };

            match query_buf.trim() {
                "help" | ":help" | ":h" | "?" => {
                    println!("Enter a RELLOG TERM or one of these REPL COMMANDS:");
                    println!("  :h | :help | ?    Displays this help text.");
                    println!("  :r | :reload      Reloads the source file.");
                    continue;
                }
                ":reload" | ":r" => {
                    println!("Reloading source file...");
                    self.module = match load_module_from_file(&self.current_file) {
                        Ok(m) => {
                            println!("{} relation definitions loaded.", m.rel_defs().count());
                            m
                        }
                        Err(e) => {
                            println!("Error loading file: {e}");
                            println!("Defaulting to empty module.");
                            Default::default()
                        }
                    };
                    continue;
                }
                _ => {}
            }

            let tokens = lex::tokenize(&query_buf[..]);

            let query = match parse::entire_term(&tokens) {
                Ok(q) => q.into(),
                Err(e) => {
                    println!("Parse error:");
                    parse::display_parse_err(&e);
                    continue 'outer;
                }
            };

            let rt = rt::Rt::new(&self.module);
            let solns = rt.solve_query(&query, UnifierSet::new());

            self.config.set_repl_mode(ReplMode::PrintingSolns);
            for soln in solns {
                match soln {
                    Ok(soln) => println!("{soln}"),
                    Err(e) => println!("{}", Color::Red.paint(format!("Exception: {e}"))),
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

            println!("false.");
        }
    }
}

fn load_module_from_file(fname: &str) -> AppRes<ast::Module> {
    let f = std::fs::File::open(fname).map_err(|e| AppErr::FileOpenErr(fname.into(), e))?;
    let mut r = io::BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src)
        .map_err(|e| AppErr::FileReadErr(fname.into(), e))?;

    let tokens = lex::tokenize(Span::new(&src));

    let m = match parse::entire_module(&tokens) {
        Ok(m) => m,
        Err(verbose_err) => {
            parse::display_parse_err(&verbose_err);
            return Err(AppErr::ParseErr(verbose_err.into()));
        }
    };

    Ok(m)
}
