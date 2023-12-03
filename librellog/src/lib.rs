#![deny(unused_must_use)]
#![feature(result_option_inspect)]

pub mod ast;
pub mod data_structures;
pub mod interner;
pub mod lex;
pub mod parse;
pub mod rt;
pub mod utils;

#[magic_static::main(interner::INTERNER)]
pub fn init_interner() {}
