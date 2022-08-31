#![deny(unused_must_use)]

use std::io::{BufReader, Read};

mod repl;

use librellog::{lex, my_nom::Span, parse};

fn main() {
    librellog::init_interner();

    let fname = std::env::args().nth(1).expect("Please provide a filename");
    let f = std::fs::File::open(fname).unwrap();
    let mut r = BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src).unwrap();

    let tokens = lex::tokenize(Span::new(&src));
    let ast = parse::entire_module(&tokens).unwrap_or_else(|verbose_err| {
        parse::display_parse_err(verbose_err);
        std::process::exit(1)
    });

    repl::repl(ast);
}
