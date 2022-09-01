use std::{fmt, iter};

use crate::{
    ast::{Module, RcTm, Tm},
    data_structures::Var,
    tok::Tok,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
}

impl fmt::Display for Err {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Err::AttemptToQueryNonCallable(tm) => {
                write!(f, "The term {tm} is not callable.")
            }
        }
    }
}

pub type Res<T> = Result<T, Err>;

pub type UnifierSet = unifier_set::UnifierSet<Var, RcTm>;

/// Essentially a trait alias.
pub trait SolnStream: Iterator<Item = Res<UnifierSet>> {}
impl<T> SolnStream for T where T: Iterator<Item = Res<UnifierSet>> {}

pub struct Rt<'rt> {
    db: &'rt Module,
}

impl<'rt> Rt<'rt> {
    pub fn new(db: &'rt Module) -> Self {
        Self { db }
    }

    pub fn solve_query<'rtb, 'q, 'it>(
        &'rtb self,
        query: &'q RcTm,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'q: 'it,
    {
        match query.as_ref() {
            Tm::Rel(_) => self.solve_rel(query, u),
            Tm::Block(Tok::Pipe, members) => self.solve_or_block(members, u),
            Tm::Block(Tok::Dash, members) => self.solve_and_block(members, u),
            Tm::Block(_, _) => Box::new(iter::once(Err(Err::AttemptToQueryNonCallable(
                query.clone(),
            )))),
            Tm::Sym(_) | Tm::Var(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Cons(_, _) | Tm::Nil => {
                Box::new(iter::once(Err(Err::AttemptToQueryNonCallable(
                    query.clone(),
                ))))
            }
        }
    }

    fn solve_rel<'rtb, 'q, 'it>(
        &'rtb self,
        query: &'q RcTm,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'q: 'it,
    {
        Box::new(self.db.rel_defs().flat_map(move |(head, opt_body)| {
            let head = Tm::Rel(head.clone()).into(); // TODO use rpds for Rel.
            match u.unify(&head, &query) {
                Some(u) => {
                    if let Some(body) = opt_body {
                        self.solve_query(body, u)
                    } else {
                        Box::new(iter::once(Ok(u)))
                    }
                }
                None => Box::new(iter::empty()),
            }
        }))
    }

    fn solve_or_block<'rtb, 'it, 'q: 'it>(
        &'rtb self,
        members: &'q Vec<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'q: 'it,
    {
        Box::new(
            members
                .iter()
                .flat_map(move |q| self.clone().solve_query(q, u.clone())),
        )
    }

    fn solve_and_block<'rtb, 'q, 'it>(
        &'rtb self,
        members: &'q Vec<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'q: 'it,
    {
        let init: Box<dyn SolnStream> = Box::new(iter::once(Ok(u)));
        Box::new(
            members
                .iter() //
                .fold(init, |solns, q| {
                    Box::new(
                        solns
                            .map(Res::unwrap) //
                            .flat_map(|u| self.solve_query(q, u)),
                    )
                }),
        )
    }
}
