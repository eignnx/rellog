use std::cell::{Cell, RefCell};

use rpds::Vector;

use crate::{
    ast::dup::TmDuplicator,
    ast::{RcTm, Tm},
    lex::tok::Tok,
    utils::{cloning_iter::CloningIterator, deferred_iter::DeferredIter},
};

use super::{
    err::Err,
    intrinsics::IntrinsicsMap,
    kb::KnowledgeBase,
    soln_stream::{self, SolnStream},
    UnifierSet,
};

pub static DEFAULT_MAX_RECURSION_DEPTH: usize = 256;

/// The rellog runtime engine. (`rt` stands for "run time")
pub struct Rt {
    pub db: KnowledgeBase,
    pub intrs: IntrinsicsMap,
    pub recursion_depth: Cell<usize>,
    pub max_recursion_depth: usize,
}

impl Rt {
    pub fn new(db: impl Into<KnowledgeBase>) -> Self {
        Self {
            db: db.into(),
            intrs: IntrinsicsMap::initialize(),
            recursion_depth: 0.into(),
            max_recursion_depth: DEFAULT_MAX_RECURSION_DEPTH,
        }
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
        self.recursion_depth.set(0);
        self.solve_query_impl(query, u, td)
    }

    pub fn solve_query_impl<'rtb, 'td, 'it>(
        &'rtb self,
        query: RcTm,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'td: 'it,
    {
        if self.recursion_depth.get() >= self.max_recursion_depth {
            return Err::MaxRecursionDepthExceeded {
                query: u.reify_term(&query),
                depth: self.recursion_depth.get(),
            }
            .into();
        }

        self.incr_recursion_depth();

        match query.as_ref() {
            Tm::Rel(_) => self.solve_rel(query.clone(), u, td),
            Tm::Block(Tok::Pipe, members) => Box::new(
                self.solve_or_block(members.clone(), u, td)
                    .chain(self.deferred_decr_recursion_depth()),
            ),
            Tm::Block(Tok::Dash, members) => Box::new(
                self.solve_and_block(members.clone(), u, td)
                    .chain(self.deferred_decr_recursion_depth()),
            ),
            Tm::Block(_, _) => {
                self.decr_recursion_depth();
                Err::AttemptToQueryNonCallable(query.clone()).into()
            }
            Tm::Var(_) => {
                let reified = u.reify_term(&query);
                if query == reified {
                    self.decr_recursion_depth();
                    Err::InstantiationError(query.clone()).into()
                } else {
                    Box::new(
                        self.solve_query_impl(reified, u, td)
                            .chain(self.deferred_decr_recursion_depth()),
                    )
                }
            }
            Tm::Sym(..) | Tm::Int(..) | Tm::Txt(..) | Tm::Cons(..) | Tm::Nil => {
                self.decr_recursion_depth();
                Err::AttemptToQueryNonCallable(query.clone()).into()
            }
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
        let query = u.reify_term(&query);
        let rel = match query.as_ref() {
            Tm::Rel(rel) => rel,
            _ => unreachable!("Assume we always send in a `Rel` term."),
        };

        // Perform "argument indexing", get all potientially-matching clauses.
        let clauses = match self.db.index_match(rel) {
            Some(clauses) => clauses,
            None => {
                // If it can't be found in the loaded module, check the intrinsics.
                return match self.intrs.index_match(rel) {
                    Some(intr) => intr.apply(u, rel.clone()),
                    None => Err::NoSuchRelation(rel.clone().into()).into(),
                };
            }
        };

        Box::new(
            clauses
                .flat_map(move |clause| {
                    // Make a duplicate of the clause.
                    let clause = td.borrow_mut().duplicate(clause);

                    let head = Tm::Rel(clause.head).into();

                    // Attempt to unify head and query.
                    match u.unify(&query, &head) {
                        // If unification succeeds...
                        Some(u) => {
                            if let Some(body) = clause.body {
                                self.solve_query_impl(body, u, td)
                            } else {
                                soln_stream::success(u)
                            }
                        }
                        // Unification failed, move on to test next clause.
                        None => soln_stream::failure(),
                    }
                })
                .chain(self.deferred_decr_recursion_depth()),
        )
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
            .flat_map(move |q| self.solve_query_impl(q, u.clone(), td))
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
        let init: Box<dyn SolnStream> = soln_stream::success(u);

        members.cloning_iter().fold(init, |solns, q| {
            Box::new(solns.flat_map(move |u_res| match u_res {
                Ok(u) => self.solve_query_impl(q.clone(), u, td),
                Err(e) => e.into(),
            }))
        })
    }

    fn incr_recursion_depth(&self) {
        let d = self.recursion_depth.get();
        self.recursion_depth.set(d + 1);
    }

    #[track_caller]
    fn decr_recursion_depth(&self) {
        let d = self.recursion_depth.get();
        self.recursion_depth.set(d - 1);
    }

    fn deferred_decr_recursion_depth<'rtb>(
        &'rtb self,
    ) -> impl ExactSizeIterator<Item = Result<UnifierSet, Err>> + 'rtb {
        DeferredIter::new(|| {
            self.decr_recursion_depth();
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
    let module = parse::entire_module(tokens[..].into()).unwrap();

    let rt = Rt::new(&module);

    let query = {
        let tokens = lex::tokenize("[prefix {1 2 3}][suffix {4 5 6}][Compound]");
        parse::entire_term(tokens[..].into()).unwrap()
    };

    let solns = rt
        .solve_query(query, UnifierSet::default(), &Default::default())
        .collect::<Vec<_>>();

    assert2::let_assert!([Ok(u)] = &solns[..]);
    let answer_var = Tm::Var("Compound".into()).into();
    assert2::check!(&u.reify_term(&answer_var).to_string() == "{1 2 3 4 5 6}");
}
