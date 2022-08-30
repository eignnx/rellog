use std::io::{self, Write};

use nom::Finish;

use crate::{
    ast, lex, parse,
    rt::{self, UnifierSet},
};

pub fn repl(m: ast::Module) {
    let rt = rt::Rt::new(m);
    let mut query_buf = String::with_capacity(80);
    'outer: loop {
        print!("\n-- ");
        io::stdout().flush().unwrap();

        query_buf.clear();
        io::stdin().read_line(&mut query_buf).unwrap();

        let tokens = lex::tokenize(&query_buf[..]);

        let query = match parse::tm(&tokens).finish() {
            Ok((_, tm)) => tm.into(),
            Err(e) => {
                parse::display_parse_err(e);
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
