use std::{cell::RefCell, fmt, iter};

use rpds::Vector;

use crate::{
    ast::{Module, RcTm, Sig, Tm},
    cloning_iter::CloningIterator,
    data_structures::Var,
    dup::TmDuplicator,
    tok::Tok,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
    InstantiationError(RcTm),
    NoSuchRelation(Sig),
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
            Err::NoSuchRelation(sig) => {
                write!(f, "No relation exists with signature {sig}.")
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

    pub fn solve_query<'rtb, 'td, 'it>(
        &'rtb self,
        query: RcTm,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'td: 'it,
    {
        match query.as_ref() {
            Tm::Rel(_) => self.solve_rel(query.clone(), u, td),
            Tm::Block(Tok::Pipe, members) => Box::new(self.solve_or_block(members.clone(), u, td)),
            Tm::Block(Tok::Dash, members) => Box::new(self.solve_and_block(members.clone(), u, td)),
            Tm::Block(_, _) => Box::new(iter::once(Err(Err::AttemptToQueryNonCallable(
                query.clone(),
            )))),
            Tm::Var(_) => {
                let reified = u.reify_term(&query);
                if query == reified {
                    Box::new(iter::once(Err(Err::InstantiationError(query.clone()))))
                } else {
                    self.solve_query(reified, u, td)
                }
            }
            Tm::Sym(_) | Tm::Num(_) | Tm::Txt(_) | Tm::Cons(_, _) | Tm::Nil => Box::new(
                iter::once(Err(Err::AttemptToQueryNonCallable(query.clone()))),
            ),
        }
    }

    fn solve_rel<'rtb, 'td, 'it>(
        &'rtb self,
        query: RcTm,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'td: 'it,
    {
        // Assume we always send in a relation term.
        let rel = match query.as_ref() {
            Tm::Rel(rel) => rel,
            _ => unreachable!(),
        };

        // Perform "argument indexing", get all potientially-matching clauses.
        let clauses = match self.db.index_match(rel) {
            Some(clauses) => clauses,
            None => return Box::new(iter::once(Err(Err::NoSuchRelation(rel.into())))),
        };

        Box::new(clauses.flat_map(move |clause| {
            // Make a duplicate the clause.
            let clause = td.borrow_mut().duplicate(clause);

            let head = Tm::Rel(clause.head.into()).into();

            // Attempt to unify head and query.
            match u.unify(&query, &head) {
                // If unification succeeds...
                Some(u) => {
                    if let Some(body) = clause.body {
                        println!(
                            "Unify Success:\nquery={query}\n head={head}\nbody={body}\nwhere\n{u}"
                        );
                        self.solve_query(body, u, td)
                    } else {
                        println!("Unify Success:\nquery={query}\n head={head}\nwhere\n{u}");
                        Box::new(iter::once(Ok(u)))
                    }
                }
                // Unification failed, move on to test next clause.
                None => {
                    println!("Unify Failure:\nquery={query}\n head={head}\nwhere\n{u}");
                    println!("{td:?}");
                    Box::new(iter::empty())
                }
            }
        }))
    }

    fn solve_or_block<'rtb, 'td, 'it>(
        &'rtb self,
        members: Vector<RcTm>,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> impl SolnStream + 'it
    where
        'rtb: 'it,
        'td: 'it,
    {
        members
            .cloning_iter()
            .flat_map(move |q| self.clone().solve_query(q.clone(), u.clone(), td))
    }

    fn solve_and_block<'rtb, 'td, 'it>(
        &'rtb self,
        members: Vector<RcTm>,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> impl SolnStream + 'it
    where
        'rtb: 'it,
        'td: 'it,
    {
        let init: Box<dyn SolnStream> = Box::new(iter::once(Ok(u)));

        members.cloning_iter().fold(init, |solns, q| {
            Box::new(
                solns
                    .map(Res::unwrap)
                    .flat_map(move |u| self.solve_query(q.clone(), u, td)),
            )
        })
    }
}

#[test]
fn test_runtime() {
    use crate::{lex, parse};
    crate::init_interner();

    let src = "
[prefix {}][Suffix][compound Suffix]
[prefix {A ...As}][Suffix][compound {A ...Compound}]
    - [prefix As][suffix Suffix][Compound]
";

    let tokens = lex::tokenize(src);
    let module = parse::entire_module(&tokens[..]).unwrap();

    let rt = Rt::new(&module);

    let query = {
        let tokens = lex::tokenize("[prefix {1,2,3}][suffix {4,5,6}][Compound]");
        parse::entire_term(&tokens[..]).unwrap()
    };

    let solns = rt
        .solve_query(query, UnifierSet::default(), &Default::default())
        .collect::<Vec<_>>();

    assert2::let_assert!([Ok(u)] = &solns[..]);
    let answer_var = Tm::Var("Compound".into()).into();
    assert2::check!(&u.reify_term(&answer_var).to_string() == "{1, 2, 3, 4, 5, 6}");
}
