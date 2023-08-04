#[derive(Clone, Copy, Debug, Default)]
pub struct IntCounter(u128);

impl Iterator for IntCounter {
    type Item = i64;
    fn next(&mut self) -> Option<Self::Item> {
        let IntCounter(n) = *self;
        self.0 += 1;
        let half = (n / 2) as i64;
        if n % 2 == 0 {
            Some(half)
        } else {
            Some(-half)
        }
    }
}
