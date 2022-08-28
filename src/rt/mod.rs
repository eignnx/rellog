use std::{iter, rc::Rc};

use rpds::HashTrieMap;

use crate::{
    ast::{Module, Tm},
    data_structures::Var,
    tok::Tok,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(Rc<Tm>),
    UnificationFailure(Rc<Tm>, Rc<Tm>),
}

pub type Res<T> = Result<T, Err>;

pub type UnifierSet = HashTrieMap<Var, Rc<Tm>>;

/// Essentially a trait alias.
pub trait SolnStream: Iterator<Item = Res<UnifierSet>> {}
impl<T> SolnStream for T where T: Iterator<Item = Res<UnifierSet>> {}

trait Unify: Sized {
    fn unify(&self, a: &Rc<Tm>, b: &Rc<Tm>) -> Res<Self>;
    fn find_root(&self, var: Var) -> Option<Rc<Tm>>;
}

impl Unify for UnifierSet {
    fn unify(&self, a: &Rc<Tm>, b: &Rc<Tm>) -> Res<Self> {
        dbg!((&a, &b));
        match (a.as_ref(), b.as_ref()) {
            (tm1, tm2) if tm1 == tm2 => Ok(self.clone()),

            // Both variables.
            (Tm::Var(va), Tm::Var(vb)) => match (self.find_root(*va), self.find_root(*vb)) {
                (None, None) => Ok(self.insert(*va, b.clone())),
                (None, Some(root)) => Ok(self.insert(*va, root.clone())),
                (Some(root), None) => Ok(self.insert(*vb, root.clone())),
                (Some(ra), Some(rb)) => match (ra.as_ref(), rb.as_ref()) {
                    (Tm::Var(root_va), _) => Ok(self.insert(*root_va, rb)),
                    (_, Tm::Var(root_vb)) => Ok(self.insert(*root_vb, ra)),
                    _ => self.unify(&ra, &rb),
                },
            },

            // One variable.
            (Tm::Var(va), _non_var) => match self.find_root(*va) {
                None => Ok(self.insert(*va, b.clone())),
                Some(root_a) => self.unify(&root_a, b),
            },

            // One variable in opposite order. Just swap them and try again.
            (_non_var, Tm::Var(_)) => self.unify(b, a),
            (Tm::Rel(r1), Tm::Rel(r2)) if r1.keys().eq(r2.keys()) => {
                let mut u = self.clone();
                for (tm1, tm2) in r1.values().zip(r2.values()) {
                    u = u.unify(tm1, tm2)?;
                }
                Ok(u)
            }

            (Tm::Cons(x, xs), Tm::Cons(y, ys)) => {
                let u = self;
                let u = u.unify(x, y)?;
                let u = u.unify(xs, ys)?;
                Ok(u)
            }

            // Prolog equiv.: `(a, B, c) = (X, 2, Z).`
            (Tm::Block(f1, b1), Tm::Block(f2, b2)) if f1 == f2 && b1.len() == b2.len() => {
                let mut u = self.clone();
                for (tm1, tm2) in b1.iter().zip(b2.iter()) {
                    u = u.unify(tm1, tm2)?;
                }
                Ok(u)
            }

            _ => Err(Err::UnificationFailure(a.clone(), b.clone())),
        }
    }

    /// Looks up the representative element for this term. If the term has never
    /// been seen before, it will be associated to itself.
    fn find_root(&self, mut var: Var) -> Option<Rc<Tm>> {
        loop {
            match self.get(&var) {
                Some(rc_tm) => match rc_tm.as_ref() {
                    &Tm::Var(v) => var = v,
                    _ => return Some(rc_tm.clone()),
                },
                None => return Some(Rc::new(Tm::Var(var))),
            }
        }
    }
}

pub struct Rt {
    db: Module,
}

impl Rt {
    pub fn new(db: Module) -> Self {
        Self { db }
    }

    pub fn solve_query<'rt, 'q, 'it>(
        &'rt self,
        query: &'q Rc<Tm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
        'q: 'it,
    {
        dbg!(&query);
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
        query: &'q Rc<Tm>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
        'q: 'it,
    {
        dbg!(&query);
        Box::new(self.db.rel_defs().flat_map(move |(head, opt_body)| {
            dbg!(&(head, opt_body));
            let head = Rc::new(Tm::Rel(head.clone())); // TODO use rpds for Rel.
            match u.clone().unify(&head, &query) {
                Ok(u) => {
                    if let Some(body) = opt_body {
                        self.solve_query(body, u)
                    } else {
                        Box::new(iter::once(Ok(u)))
                    }
                }
                Err(Err::UnificationFailure(_, _)) => Box::new(iter::empty()),
                Err(e) => Box::new(iter::once(Err(e))),
            }
        }))
    }

    fn solve_or_block<'rt: 'it, 'it, 'q: 'it>(
        &'rt self,
        members: &'q Vec<Rc<Tm>>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
        'q: 'it,
    {
        dbg!(&members);
        Box::new(
            members
                .iter()
                .flat_map(move |q| self.clone().solve_query(q, u.clone())),
        )
    }

    fn solve_and_block<'rt, 'q, 'it>(
        &'rt self,
        members: &'q Vec<Rc<Tm>>,
        u: UnifierSet,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rt: 'it,
        'q: 'it,
    {
        dbg!(&members);
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
