use rpds::Vector;

pub struct CloningIter<T: Clone> {
    vector: Vector<T>,
    idx: usize,
}

pub trait CloningIterator {
    type Item: Clone;
    type IntoIter;
    fn cloning_iter(self) -> Self::IntoIter;
}

impl<T: Clone> CloningIterator for Vector<T> {
    type Item = T;
    type IntoIter = CloningIter<T>;

    fn cloning_iter(self) -> Self::IntoIter {
        CloningIter {
            vector: self,
            idx: 0,
        }
    }
}

impl<T: Clone> Iterator for CloningIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.vector.get(self.idx).map(|t| {
            self.idx += 1;
            t.clone()
        })
    }
}

impl<T: Clone> ExactSizeIterator for CloningIter<T> {
    fn len(&self) -> usize {
        self.vector.len()
    }
}
