#![deny(unused_must_use)]

use std::io::{BufReader, Read};

use crate::my_nom::Span;

mod ast;
mod data_structures;
mod lex;
mod my_nom;
mod parse;
mod tok;

fn main() {
    let fname = std::env::args().nth(1).expect("Please provide a filename");
    let f = std::fs::File::open(fname).unwrap();
    let mut r = BufReader::new(f);
    let mut src = String::new();
    r.read_to_string(&mut src).unwrap();
    let tokens = lex::tokenize(Span::new(&src));
    for tok in tokens {
        println!("{:?}", tok);
    }
}
