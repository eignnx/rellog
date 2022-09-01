use std::num::NonZeroUsize;

pub struct Incr {
    n: NonZeroUsize,
}

impl Default for Incr {
    fn default() -> Self {
        Self {
            n: unsafe { NonZeroUsize::new_unchecked(1) },
        }
    }
}

impl Incr {
    pub fn next(&mut self) -> NonZeroUsize {
        let old = self.n;
        let new = NonZeroUsize::new(old.get() + 1).unwrap();
        self.n = new;
        old
    }

    pub fn reset(&mut self) {
        self.n = unsafe { NonZeroUsize::new_unchecked(1) };
    }
}
