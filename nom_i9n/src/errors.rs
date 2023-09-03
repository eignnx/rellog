#[derive(Debug, Clone)]
pub struct I9nError<I> {
    pub input: I,
    pub situation: I9nErrorSituation,
    pub ctx: I9nErrorCtx,
}

#[derive(Debug, Clone)]
pub struct I9nErrorSituation {
    pub relation: I9nRelation,
    pub expected: usize,
    pub actual: usize,
}

#[derive(Debug, Clone)]
pub enum I9nRelation {
    NotGt,
    NotEq,
    Gt,
    Eq,
    Lt,
}

#[derive(Debug, Clone, Copy)]
pub enum I9nErrorCtx {
    AtNewGroup,
    WithinLine,
    AtNewLine,
    AtGroupEnd,
    WithinLineButAfterStart,
}
