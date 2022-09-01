use std::{cell::RefCell, fmt, iter};

use rpds::Vector;

use crate::{
    ast::{Module, RcTm, Tm},
    cloning_iter::CloningIterator,
    data_structures::Var,
    dup::Dup,
    incr::Incr,
    tok::Tok,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
    InstantiationError(RcTm),
}

impl fmt::Display for Err {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Err::AttemptToQueryNonCallable(tm) => {
                write!(f, "The term {tm} is not callable.")
            }
            Err::InstantiationError(tm) => {
                write!(f, "The term {tm} is not sufficiently instantiated.")
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
    incr: RefCell<Incr>,
}

impl<'rt> Rt<'rt> {
    pub fn new(db: &'rt Module) -> Self {
        Self {
            db,
            incr: RefCell::new(Incr::default()),
        }
    }

    pub fn solve_query<'rtb, 'it>(
        &'rtb self,
        query: RcTm,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
    {
        match query.as_ref() {
            Tm::Rel(_) => self.solve_rel(query.clone(), u),
            Tm::Block(Tok::Pipe, members) => self.solve_or_block(members.clone(), u),
            Tm::Block(Tok::Dash, members) => self.solve_and_block(members.clone(), u),
            Tm::Block(_, _) => Box::new(iter::once(Err(Err::AttemptToQueryNonCallable(
                query.clone(),
            )))),
            Tm::Var(_) => {
                let reified = u.reify_term(&query);
                if query == reified {
                    Box::new(iter::once(Err(Err::InstantiationError(query.clone()))))
                } else {
                    self.solve_query(reified, u)
                }
            }
            Tm::Sym(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Cons(_, _) | Tm::Nil => Box::new(
                iter::once(Err(Err::AttemptToQueryNonCallable(query.clone()))),
            ),
        }
    }

    fn solve_rel<'rtb, 'it>(&'rtb self, query: RcTm, u: UnifierSet) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
    {
        Box::new(self.db.rel_defs().flat_map(move |(head, opt_body)| {
            let head = Tm::Rel(head.clone())
                .dup(&mut *self.incr.borrow_mut())
                .into();

            match u.unify(&head, &query) {
                Some(u) => {
                    if let Some(body) = opt_body {
                        let body = body.dup(&mut *self.incr.borrow_mut());
                        self.solve_query(body, u)
                    } else {
                        Box::new(iter::once(Ok(u)))
                    }
                }
                None => Box::new(iter::empty()),
            }
        }))
    }

    fn solve_or_block<'rtb, 'it>(
        &'rtb self,
        members: Vector<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
    {
        Box::new(
            members
                .cloning_iter()
                .flat_map(move |q| self.clone().solve_query(q.clone(), u.clone())),
        )
    }

    fn solve_and_block<'rtb, 'it>(
        &'rtb self,
        members: Vector<RcTm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
    {
        let init: Box<dyn SolnStream> = Box::new(iter::once(Ok(u)));
        Box::new(members.cloning_iter().fold(init, |solns, q| {
            Box::new(
                solns
                    .map(Res::unwrap)
                    .flat_map(move |u| self.solve_query(q.clone(), u)),
            )
        }))
    }
}
