pub(crate) const BYTES_PER_CHUNK: usize = 32; // TODO: What's the best constant here?

#[derive(Clone, Hash)]
pub struct Chunk {
    /// # Invariants
    /// 1. Bytes in `buf` must represent valid UTF8 characters.
    /// 2. If `buf` is not full, it will consist of leading zero bytes
    /// followed by the contained characters.
    /// 3. `buf` cannot contain the character `'\0'`. To represent a string
    /// containing a null character, a special `Chunk` is used.
    pub buf: [u8; BYTES_PER_CHUNK],
}

impl Chunk {
    fn zeroed() -> Self {
        Self {
            buf: [0; BYTES_PER_CHUNK],
        }
    }

    /// Panics if `bytes.len()` is greater than `BYTES_PER_BUF`.
    fn from_bytes(bytes: &[u8]) -> Self {
        assert!(bytes.len() <= BYTES_PER_CHUNK);
        let mut chunk = Self::zeroed();
        chunk.buf[BYTES_PER_CHUNK - bytes.len()..].copy_from_slice(bytes);
        chunk
    }

    pub(crate) fn from_str(s: &str) -> Option<(Self, &str)> {
        assert!(!s.contains('\0'));

        if s.is_empty() {
            return None;
        }

        let (prefix, suffix) = {
            let mut split_point = BYTES_PER_CHUNK.min(s.as_bytes().len());

            // Walk backwards through the *bytes* till a char boundary is found.
            while !s.is_char_boundary(split_point) {
                split_point -= 1;
            }

            s.split_at(split_point)
        };

        let chunk = Self::from_bytes(prefix.as_bytes());

        Some((chunk, suffix))
    }

    fn start_byte(&self) -> Option<usize> {
        self.buf
            .iter()
            .enumerate()
            .find(|(_, &byte)| byte != 0)
            .map(|(i, _)| i)
    }

    pub(crate) fn as_str(&self) -> &str {
        let start = match self.start_byte() {
            None => return "",
            Some(start) => start,
        };

        if cfg!(debug_assertions) {
            std::str::from_utf8(&self.buf[start..]).unwrap()
        } else {
            // Safety: An invariant of `Chunk` is that `buf` always contains a
            // (right-aligned) sequence of valid UTF8 characters.
            unsafe { std::str::from_utf8_unchecked(&self.buf[start..]) }
        }
    }

    pub(crate) fn has_space(&self, ch: char) -> bool {
        let bytes_needed = ch.len_utf8();
        match self.start_byte() {
            None => false,
            Some(space_available) => bytes_needed <= space_available,
        }
    }

    pub(crate) fn push_front(&mut self, ch: char) -> Option<Self> {
        if !self.has_space(ch) {
            return None;
        }

        todo!()
    }
}
