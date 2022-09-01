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
use reedline::{Reedline, Signal};

use crate::{
    app_err::{AppErr, AppRes},
    line_editor_config::{default_line_editor, PROMPT},
};

pub struct Repl {
    module: ast::Module,
    line_editor: Reedline,
}

impl Repl {
    pub fn loading_file(fname: &str) -> Self {
        match load_module_from_file(fname) {
            Ok(module) => Self {
                module,
                line_editor: default_line_editor(),
            },
            Err(e) => {
                println!("{e}");
                println!("Loading default module.");
                Self {
                    module: ast::Module::default(),
                    line_editor: default_line_editor(),
                }
            }
        }
    }

    pub fn run(&mut self) -> ! {
        'outer: loop {
            let query_buf = match self.line_editor.read_line(&PROMPT) {
                Ok(Signal::Success(s)) => s,
                Ok(Signal::CtrlC) => continue 'outer,
                Ok(Signal::CtrlD) => exit(0),
                Err(e) => {
                    println!("{e}");
                    exit(0);
                }
            };

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

            'soln_loop: for soln in solns {
                if let Ok(soln) = soln {
                    println!("{soln}");
                }

                'inner_input_loop: loop {
                    let buf = match self.line_editor.read_line(&PROMPT).unwrap() {
                        Signal::Success(s) => s,
                        Signal::CtrlC => break 'soln_loop,
                        Signal::CtrlD => exit(0),
                    };

                    match buf.chars().nth(0).unwrap() {
                        ' ' => continue 'soln_loop,
                        '\n' => break 'soln_loop,
                        _ => {
                            println!("Unknown input. Type SPACE to view more solutions, ENTER to return to query prompt.");
                            continue 'inner_input_loop;
                        }
                    }
                }
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
