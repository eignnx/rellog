use std::{iter::FusedIterator, marker::PhantomData};

pub struct DeferredIter<T, F: Fn()> {
    _t: PhantomData<T>,
    func: F,
    done: bool,
}

impl<T, F: Fn()> DeferredIter<T, F> {
    pub fn new(func: F) -> Self {
        Self {
            _t: PhantomData,
            func,
            done: false,
        }
    }
}

impl<T, F: Fn()> Iterator for DeferredIter<T, F> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.done {
            (self.func)();
            self.done = true;
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, Some(0))
    }

    fn count(self) -> usize
    where
        Self: Sized,
    {
        0
    }
}

impl<T, F: Fn()> ExactSizeIterator for DeferredIter<T, F> {
    fn len(&self) -> usize {
        0
    }
}

impl<T, F: Fn()> FusedIterator for DeferredIter<T, F> {}
