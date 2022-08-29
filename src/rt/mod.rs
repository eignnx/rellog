use std::iter;

use crate::{
    ast::{Module, RcTm, Tm},
    data_structures::Var,
    tok::Tok,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
}

pub type Res<T> = Result<T, Err>;

pub type UnifierSet = unifier_set::UnifierSet<Var, RcTm>;

/// Essentially a trait alias.
pub trait SolnStream: Iterator<Item = Res<UnifierSet>> {}
impl<T> SolnStream for T where T: Iterator<Item = Res<UnifierSet>> {}

pub struct Rt {
    db: Module,
}

impl Rt {
    pub fn new(db: Module) -> Self {
        Self { db }
    }

    pub fn solve_query<'rt, 'q, 'it>(
        &'rt self,
        query: &'q RcTm,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
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

    fn solve_rel<'rt, 'q, 'it>(
        &'rt self,
        query: &'q RcTm,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
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

    fn solve_or_block<'rt: 'it, 'it, 'q: 'it>(
        &'rt self,
        members: &'q Vec<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
        'q: 'it,
    {
        Box::new(
            members
                .iter()
                .flat_map(move |q| self.clone().solve_query(q, u.clone())),
        )
    }

    fn solve_and_block<'rt, 'q, 'it>(
        &'rt self,
        members: &'q Vec<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
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
