pub trait NextTokCol<I> {
    fn next_tok_col(input: &I) -> Option<usize>;
}

pub trait StartCol {
    fn start_col(&self) -> usize;
}
