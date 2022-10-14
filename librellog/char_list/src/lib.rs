//! Defines an efficient prependable string datatype suitable for representing
//! linked lists of characters.

mod char_list;
mod chunk;
mod chunk_buf;
#[cfg(test)]
mod tests;

pub use char_list::{CharList, FollowTail};
