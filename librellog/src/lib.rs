#![deny(unused_must_use)]

pub mod ast;
pub mod data_structures;
pub mod interner;
pub mod lex;
pub mod parse;
pub mod rt;
pub mod session;
pub mod utils;

#[magic_static::main(interner::INTERNER)]
pub fn init_interner() {}

pub const STD_LIB_ROOT: &str = concat!(env!("CARGO_MANIFEST_DIR"), "/src/std");

pub const STD_LIB_DEPS_NO_MACROS: &[&str] = &[
    concat!(env!("CARGO_MANIFEST_DIR"), "/src/std", "/bootstrap.rellog"),
    concat!(env!("CARGO_MANIFEST_DIR"), "/src/std", "/dcgs/list.rellog"),
    // concat!(env!("CARGO_MANIFEST_DIR"), "/src/std", "/std.rellog"),
];
