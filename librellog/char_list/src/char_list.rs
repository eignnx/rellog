use core::fmt;
use std::rc::Rc;

use crate::{chunk::Chunk, chunk_buf::ChunkBuf};

type ChunkIdx = i16;
type ByteIdx = u16;

#[derive(Clone)]
pub struct CharList<Tail: FollowTail> {
    ptr: Rc<ChunkBuf<Tail>>,
    chunk_idx: ChunkIdx,
    byte_idx: ByteIdx,
}

pub trait FollowTail: Sized {
    fn follow_tail(&self) -> Option<&CharList<Self>>;
    fn make_tail(suffix: CharList<Self>) -> Self;
    fn nil() -> Self;
}

impl<Tail: FollowTail> CharList<Tail> {
    pub(crate) fn chunk_idx(&self) -> usize {
        self.chunk_idx as usize
    }

    pub(crate) fn byte_idx(&self) -> usize {
        self.byte_idx as usize
    }

    /// Returns true if `self.ptr` is the *only* `Rc` pointer pointing to the
    /// `ChunkBuf`.
    fn is_unique_ref(&self) -> bool {
        Rc::strong_count(&self.ptr) == 1
    }

    fn front_chunk(&self) -> &Chunk {
        &self.ptr.chunks[self.chunk_idx()]
    }

    /// Returns a distinct version of `self` where `ch` has been prepended.
    pub fn push_front(&self, ch: char) -> Self {
        if self.is_unique_ref() {
            if self.byte_idx() > 0 {
                // There's space to write a char.
                self.front_chunk().push_front(ch)
            } else {
                todo!()
            }
        }
    }

    /// Attempts to split the char list into a pair containing its first
    /// character and the remainder of the char list.
    pub fn split_first(&self) -> Option<(char, Self)> {
        todo!()
    }
}

impl<Tail: FollowTail> fmt::Display for CharList<Tail> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut list = Some(self);
        while let Some(char_list) = list {
            char_list.ptr.write_chunk_buf(f, char_list)?;
            list = char_list.ptr.next.follow_tail();
        }
        // TODO: what about partial lists or dotted-pair lists?
        Ok(())
    }
}

impl<S, Tail> From<S> for CharList<Tail>
where
    S: AsRef<str>,
    Tail: FollowTail,
{
    fn from(s: S) -> Self {
        Self {
            ptr: Rc::new(ChunkBuf::from_str(s)),
            chunk_idx: 0,
            byte_idx: 0,
        }
    }
}
