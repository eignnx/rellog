use std::io::{self, Read, Write};

use librellog::{
    ast, lex,
    my_nom::Span,
    parse,
    rt::{self, UnifierSet},
};

use crate::app_err::{AppErr, AppRes};

pub struct Repl {
    module: ast::Module,
}

impl Repl {
    pub(crate) fn loading_file(fname: &str) -> Self {
        match Self::load_file(fname) {
            Ok(module) => Self { module },
            Err(e) => {
                eprintln!("{e}");
                Self {
                    module: ast::Module::default(),
                }
            }
        }
    }

    fn load_file(fname: &str) -> AppRes<ast::Module> {
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

    pub fn run(&mut self) {
        let rt = rt::Rt::new(&self.module);
        let mut query_buf = String::with_capacity(80);
        'outer: loop {
            print!("\n-- ");
            io::stdout().flush().unwrap();

            query_buf.clear();
            io::stdin().read_line(&mut query_buf).unwrap();

            let tokens = lex::tokenize(&query_buf[..]);

            let query = match parse::entire_term(&tokens) {
                Ok(tm) => tm.into(),
                Err(e) => {
                    parse::display_parse_err(&e);
                    continue 'outer;
                }
            };

            let solns = rt.solve_query(&query, UnifierSet::new());

            'soln_loop: for soln in solns {
                if let Ok(soln) = soln {
                    println!("{soln}");
                }

                'inner_input_loop: loop {
                    io::stdout().flush().unwrap();
                    let mut buf = String::new();
                    io::stdin().read_line(&mut buf).unwrap();

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
