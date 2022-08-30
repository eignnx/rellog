#![deny(unused_must_use)]

use std::io::{BufReader, Read};

use nom::Finish;

use crate::{interner::INTERNER, my_nom::Span};

mod ast;
mod data_structures;
mod interner;
mod lex;
mod my_nom;
mod parse;
mod repl;
mod rt;
mod tok;

#[magic_static::main(INTERNER)]
fn main() {
    let fname = std::env::args().nth(1).expect("Please provide a filename");
    let f = std::fs::File::open(fname).unwrap();
    let mut r = BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src).unwrap();
    let tokens = lex::tokenize(Span::new(&src));
    let (_, ast) = parse::module(&tokens)
        .finish()
        .unwrap_or_else(|verbose_err| {
            parse::display_parse_err(verbose_err);
            std::process::exit(1)
        });

    repl::repl(ast);
}
