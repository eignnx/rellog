#![deny(unused_must_use)]

use std::io::{BufReader, Read};

use nom::Finish;

use crate::{my_nom::Span, parse::module};

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
    let (_, ast) = module(&tokens).finish().unwrap_or_else(|verbose_err| {
        eprintln!("Parse error:");
        let (last, init) = verbose_err.errors.split_last().unwrap();
        for (i, ekind) in init {
            let loc = match i {
                [t, ..] => format!("{}:{}", t.line, t.col),
                [] => "eof".into(),
            };

            eprintln!("\t[{loc}] Parser {ekind:?} failed because...");
        }
        let (i, last) = last;
        eprintln!(
            "\t...{last:?}, got {}.",
            i.first().map_or_else(
                || "end of input".into(),
                |tok| format!("token `{}` at [{}:{}]", tok.value, tok.line, tok.col)
            )
        );
        std::process::exit(1)
    });

    println!("{}", ast);
}
