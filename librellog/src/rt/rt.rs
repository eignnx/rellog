use std::cell::{Cell, RefCell};

use rpds::Vector;

use crate::{
    ast::dup::TmDuplicator,
    ast::{BinOpSymbol, RcTm, Tm},
    lex::tok::Tok,
    utils::{cloning_iter::CloningIterator, deferred_iter::DeferredIter},
};

use super::{
    breakpoint::{Breakpoint, Event},
    builtins::BuiltinsMap,
    err::Err,
    kb::KnowledgeBase,
    soln_stream::{self, SolnStream},
    UnifierSet,
};

pub static DEFAULT_MAX_RECURSION_DEPTH: usize = 256;

/// The rellog runtime engine. (`rt` stands for "run time")
pub struct Rt {
    pub db: KnowledgeBase,
    pub builtins: BuiltinsMap,
    pub recursion_depth: Cell<usize>,
    pub max_recursion_depth: Cell<usize>,
    pub debug_mode: Cell<bool>,
    pub query_stack: RefCell<Vec<RcTm>>,
    pub debugger: RefCell<Option<Box<dyn Breakpoint>>>,
}

impl Rt {
    pub fn new(db: impl Into<KnowledgeBase>) -> Self {
        Self {
            db: db.into(),
            builtins: BuiltinsMap::initialize(),
            recursion_depth: 0.into(),
            max_recursion_depth: DEFAULT_MAX_RECURSION_DEPTH.into(),
            debug_mode: false.into(),
            query_stack: vec![].into(),
            debugger: None.into(),
        }
    }

    pub fn with_debugger(self, debugger: impl Breakpoint + 'static) -> Self {
        self.debugger.replace(Some(Box::new(debugger)));
        self
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
        self.query_stack.borrow_mut().clear();
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
        self.query_stack.borrow_mut().push(u.reify_term(&query));
        self.maybe_breakpoint(Event::Call);

        if self.recursion_depth.get() >= self.max_recursion_depth.get() {
            return Err::MaxRecursionDepthExceeded {
                query: u.reify_term(&query),
                depth: self.recursion_depth.get(),
            }
            .into();
        }

        self.incr_recursion_depth();

        match query.as_ref() {
            Tm::Rel(_) => self.solve_rel(query.clone(), u, td),
            Tm::Block(functor, members) => match functor {
                Tok::Pipe => Box::new(
                    self.solve_or_block(members.clone(), u, td)
                        .chain(self.deferred_decr_recursion_depth()),
                ),
                Tok::Dash => Box::new(
                    self.solve_and_block(members.clone(), u, td)
                        .chain(self.deferred_decr_recursion_depth()),
                ),
                _ => {
                    self.decr_recursion_depth();
                    Err::AttemptToQueryNonCallable(query.clone()).into()
                }
            },
            Tm::BinOp(op, x, y) => match op {
                BinOpSymbol::Equal => Box::new(
                    soln_stream::unifying(u, x, y).chain(self.deferred_decr_recursion_depth()),
                ),
                BinOpSymbol::Tilde => Box::new(
                    match u.unify(x, y) {
                        // Throw out the new unifier set, we're only testing for unifiability.
                        Some(_u) => soln_stream::success(u),
                        None => soln_stream::failure(),
                    }
                    .chain(self.deferred_decr_recursion_depth()),
                ),
                BinOpSymbol::Semicolon => self.solve_and_binop(x.clone(), y.clone(), u, td),
                BinOpSymbol::PathSep => {
                    self.decr_recursion_depth();
                    Err::AttemptToQueryNonCallable(query.clone()).into()
                }
            },
            Tm::Var(v) => {
                let reified = u.reify_term(&query);
                if query == reified {
                    self.decr_recursion_depth();
                    Err::InstantiationError {
                        rel: format!("<dynamic call of `{v}`>"),
                        tm: query.clone(),
                    }
                    .into()
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

    fn solve_and_binop<'rtb, 'td, 'it>(
        &'rtb self,
        x: RcTm,
        y: RcTm,
        u: UnifierSet,
        td: &'td RefCell<TmDuplicator>,
    ) -> Box<dyn SolnStream + 'it>
    where
        'rtb: 'it,
        'td: 'it,
    {
        Box::new(
            self.solve_query_impl(x, u, td)
                .flat_map(move |u_res| match u_res {
                    Ok(u) => self.solve_query_impl(y.clone(), u, td),
                    Err(e) => e.into(),
                })
                .chain(self.deferred_decr_recursion_depth()),
        )
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
        let clauses = match self.db.index_match(rel, &u, self.debug_mode.get()) {
            Some(clauses) => clauses,
            None => {
                // If it can't be found in the loaded module, check the intrinsics.
                return match self.builtins.index_match(rel) {
                    Some(intr) => intr.apply(self, td, u, rel.clone()),
                    None => Err::NoSuchRelation(rel.keys().cloned().into()).into(),
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

    fn deferred_decr_recursion_depth(
        &self,
    ) -> impl ExactSizeIterator<Item = Result<UnifierSet, Err>> + '_ {
        DeferredIter::new(|| {
            self.maybe_breakpoint(Event::Exit);
            let _ = self.query_stack.borrow_mut().pop();
            self.decr_recursion_depth();
        })
    }

    pub fn current_query(&self) -> RcTm {
        self.query_stack
            .borrow()
            .last()
            .expect("can't get here without init'ing qstack")
            .clone()
    }

    fn maybe_breakpoint(&self, event: Event) {
        if let Some(debugger) = self.debugger.borrow_mut().as_mut() {
            debugger.breakpoint(self, event);
        }
    }
}

#[test]
fn test_runtime() {
    use crate::{data_structures::Var, lex, parse};
    crate::init_interner();

    let src = "
[prefix {}][Suffix][compound Suffix]
[prefix {A ...As}][Suffix][compound {A ...Compound}]
    - [prefix As][suffix Suffix][Compound]
";

    let tokens = lex::tokenize(src, "".into()).unwrap();
    let module = parse::entire_module(tokens[..].into()).unwrap();

    let rt = Rt::new(module);

    let query = {
        let tokens =
            lex::tokenize("[prefix {1 2 3}][suffix {4 5 6}][Compound]", "".into()).unwrap();
        parse::entire_term(tokens[..].into()).unwrap()
    };

    let solns = rt
        .solve_query(query, UnifierSet::default(), &Default::default())
        .collect::<Vec<_>>();

    assert2::let_assert!([Ok(u)] = &solns[..]);
    let answer_var = Tm::Var(Var::from_repl("Compound")).into();
    assert2::check!(&u.reify_term(&answer_var).to_string() == "{1 2 3 4 5 6}");
}
