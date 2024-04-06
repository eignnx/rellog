#[allow(clippy::module_inception)]
mod lex;
#[cfg(test)]
mod tests;
pub mod tok;

pub use lex::*;
