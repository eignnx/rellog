use std::{collections::VecDeque, fmt};

use crate::{
    char_list::FollowTail,
    chunk::{Chunk, BYTES_PER_CHUNK},
    CharList,
};

#[derive(Hash)]
pub struct ChunkBuf<Tail> {
    pub chunks: VecDeque<Chunk>,
    pub next: Tail,
}

impl<Tail: FollowTail> ChunkBuf<Tail> {
    pub fn write_chunk_buf(
        &self,
        f: &mut fmt::Formatter<'_>,
        char_list: &CharList<Tail>,
    ) -> fmt::Result {
        let mut chunks = self.chunks.iter().skip(char_list.chunk_idx());

        let chunk = chunks.next().unwrap();
        let str = &chunk.as_str()[char_list.byte_idx()..];
        f.write_str(str)?;

        for chunk in chunks {
            f.write_str(chunk.as_str())?;
        }

        Ok(())
    }

    pub(crate) fn from_str<'a>(s: impl AsRef<str> + 'a) -> Self {
        let mut s = s.as_ref();
        let cap = s.as_bytes().len() / BYTES_PER_CHUNK + 1;
        let mut chunks = VecDeque::with_capacity(cap);

        while let Some((chunk, rem)) = Chunk::from_str(s) {
            chunks.push_back(chunk);
            s = rem;
        }

        Self {
            chunks,
            next: Tail::nil(),
        }
    }
}
