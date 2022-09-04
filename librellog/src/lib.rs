#![deny(unused_must_use)]

pub mod ast;
pub mod cloning_iter;
pub mod data_structures;
pub mod dup;
pub mod interner;
pub mod lex;
pub mod my_nom;
pub mod parse;
pub mod rt;
pub mod tm_displayer;
pub mod tok;

#[magic_static::main(interner::INTERNER)]
pub fn init_interner() {}
