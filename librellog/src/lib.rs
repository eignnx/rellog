#![deny(unused_must_use)]

pub mod ast;
pub mod data_structures;
pub mod dup;
pub mod interner;
mod intrinsics;
pub mod lex;
pub mod parse;
pub mod rt;
pub mod tm_displayer;
pub mod tok;
pub mod utils;

#[magic_static::main(interner::INTERNER)]
pub fn init_interner() {}
