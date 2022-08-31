pub mod ast;
pub mod data_structures;
pub mod interner;
pub mod lex;
pub mod my_nom;
pub mod parse;
pub mod rt;
pub mod tok;

#[magic_static::main(interner::INTERNER)]
pub fn init_interner() {}
