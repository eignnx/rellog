use std::{
    cell::RefCell,
    fmt::{self, Debug},
    iter,
};

use rpds::Vector;

use crate::{
    ast::dup::TmDuplicator,
    ast::{Module, RcTm, Sig, Tm},
    intrinsics::IntrinsicsMap,
    lex::tok::Tok,
    utils::cloning_iter::CloningIterator,
};

use super::{
    soln_stream::{self, SolnStream},
    UnifierSet,
};

#[derive(Debug, Clone)]
pub enum Err {
    AttemptToQueryNonCallable(RcTm),
    InstantiationError(RcTm),
    NoSuchRelation(Sig),
    ArgumentTypeError {
        rel: String,
        /// The attribute key that the incorrect value was passed to.
        /// Example: `[pred "adsf"][Succ]` -> key = "pred".
        key: String,
        expected_ty: String,
        recieved_tm: String,
    },
    TypeError {
        msg: String,
    },
}

impl fmt::Display for Err {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Err::AttemptToQueryNonCallable(tm) => {
                write!(f, "The term `{tm}` is not callable.")
            }
            Err::InstantiationError(tm) => {
                write!(f, "The term `{tm}` is not sufficiently instantiated.")
            }
            Err::NoSuchRelation(sig) => {
                write!(f, "No relation exists with signature `{sig}`.")
            }
            Err::ArgumentTypeError {
                rel,
                key,
                expected_ty,
                recieved_tm,
            } => {
                write!(
                    f,
                    "The `{key}` key of the relation `{rel}` expected a \
                     `{expected_ty}` argument, but received the term `{recieved_tm}`."
                )
            }
            Err::TypeError { msg } => {
                write!(f, "Type error: {msg}")
            }
        }
    }
}

pub struct Rt {
    db: Module,
    intrs: IntrinsicsMap,
}

impl Rt {
    pub fn new(db: Module) -> Self {
        Self {
            db,
            intrs: IntrinsicsMap::initialize(),
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
            Tm::Sym(..) | Tm::Num(..) | Tm::Txt(..) | Tm::Cons(..) | Tm::Nil => Box::new(
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
            None => {
                // If it can't be found in the loaded module, check the intrinsics.
                return match self.intrs.index_match(rel) {
                    Some(intr) => intr.apply(u, rel.clone()),
                    None => Box::new(iter::once(Err(Err::NoSuchRelation(rel.clone().into())))),
                };
            }
        };

        Box::new(clauses.flat_map(move |clause| {
            // Make a duplicate of the clause.
            let clause = td.borrow_mut().duplicate(clause);

            let head = Tm::Rel(clause.head).into();

            // Attempt to unify head and query.
            match u.unify(&query, &head) {
                // If unification succeeds...
                Some(u) => {
                    if let Some(body) = clause.body {
                        self.solve_query(body, u, td)
                    } else {
                        Box::new(iter::once(Ok(u)))
                    }
                }
                // Unification failed, move on to test next clause.
                None => Box::new(iter::empty()),
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
            .flat_map(move |q| self.solve_query(q, u.clone(), td))
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
        let init: Box<dyn SolnStream> = soln_stream::once(Ok(u));

        members.cloning_iter().fold(init, |solns, q| {
            Box::new(solns.flat_map(move |u_res| match u_res {
                Ok(u) => self.solve_query(q.clone(), u, td),
                Err(e) => soln_stream::once(Err(e)),
            }))
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
