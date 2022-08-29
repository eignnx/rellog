use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    fmt,
    hash::Hash,
};

use rpds::HashTrieMap;

mod tests;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Root<Term> {
    ForNonVar(Term, usize),
    OfVar(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Node<Var, Term> {
    Root(Root<Term>),
    Child(Var),
}

pub enum TermKind<Var> {
    Var(Var),
    NonVar,
}

pub trait ClassifyTerm<Var> {
    fn classify_term(&self) -> TermKind<&Var>;

    fn is_var(&self) -> bool {
        matches!(self.classify_term(), TermKind::Var(_))
    }

    fn is_non_var(&self) -> bool {
        matches!(self.classify_term(), TermKind::NonVar)
    }
}

pub trait Children: Sized {
    /// All *direct* children (and *only* the *direct* children) of `Self` which are of
    /// type `Self` should be yielded.
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a>;

    /// Given a function, you need to create a new `Self` where your *direct* children
    /// have been replaced with the ones provided by the function.
    fn map_children(&self, f: impl FnMut(Self) -> Self) -> Self;
}

pub trait SameVariant {
    /// If `self` and `other` are not of the same "kind" (i.e. different enum variants),
    /// then `false` should be returned.
    ///
    /// Note that this is a different distinction than that of `classify_term`. Two values
    /// could be `!same_variant` but have the same `TermKind`.
    fn same_variant(&self, other: &Self) -> bool;
}

#[derive(Clone)]
pub struct UnifierSet<Var, Term>
where
    Var: Eq + Hash,
{
    /// This persistent map is wrapped in a `RefCell` so that path compression can be
    /// performed behind the scenes. Path compression only speeds up access, it doesn't
    /// change externally observable behavior.
    map: RefCell<HashTrieMap<Var, Node<Var, Term>>>,
}

impl<Var, Term> fmt::Debug for UnifierSet<Var, Term>
where
    Var: Eq + Hash + fmt::Debug,
    Term: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut ds = &mut f.debug_struct("UnifierSet");
        for (var, node) in self.map.borrow().iter() {
            ds = ds.field(&format!("{var:?}"), node);
        }
        ds.finish()
    }
}

impl<Var, Term> fmt::Display for UnifierSet<Var, Term>
where
    Var: Clone + Eq + Hash + Into<Term> + fmt::Debug + fmt::Display,
    Term:
        Clone + Eq + Hash + ClassifyTerm<Var> + Children + SameVariant + fmt::Debug + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sets = HashMap::new();
        for var in self.map.borrow().keys() {
            let root = self.find_root(var);
            let set = sets.entry(root).or_insert_with(HashSet::new);
            set.insert(var.clone());
        }

        for (root, vars) in sets {
            match root {
                Root::OfVar(_) => {
                    let mut vs = vars.iter();
                    if let Some(first) = vs.next() {
                        write!(f, "    - {first}")?;
                    }
                    for var in vs {
                        write!(f, " = {var}")?;
                    }

                    if vars.len() > 2 {
                        writeln!(f)?;
                    }
                }
                Root::ForNonVar(term, _) => {
                    write!(f, "    - ")?;

                    for var in &vars {
                        write!(f, "{var} = ")?;
                    }

                    let term = self.reify_term(&term);
                    writeln!(f, "{term}")?;
                }
            }
        }

        Ok(())
    }
}

impl<Var, Term> From<HashTrieMap<Var, Node<Var, Term>>> for UnifierSet<Var, Term>
where
    Var: Eq + Hash,
{
    fn from(map: HashTrieMap<Var, Node<Var, Term>>) -> Self {
        Self {
            map: RefCell::new(map),
        }
    }
}

impl<Var, Term> UnifierSet<Var, Term>
where
    Var: Clone + Eq + Hash + Into<Term>,
    Term: Clone + Eq + Hash + ClassifyTerm<Var> + Children + SameVariant,
{
    pub fn new() -> Self {
        HashTrieMap::new().into()
    }

    fn insert(&self, var: Var, node: Node<Var, Term>) -> Self {
        self.map.borrow().insert(var, node).into()
    }

    fn hidden_update(&self, var: Var, node: Node<Var, Term>) {
        self.map.borrow_mut().insert_mut(var, node);
    }

    pub fn unify(&self, x: &Term, y: &Term) -> Option<Self> {
        use TermKind::*;

        match (x.classify_term(), y.classify_term()) {
            // If they're both compound terms, let's defer to a helper function.
            (NonVar, NonVar) => self.unify_non_vars(x, y),

            // This is an easy case. Just join the var's root to the term's root!
            (Var(x), NonVar) => match self.find_root(x) {
                // We now know `x` and `y` are both non-var terms. Let's reuse the `unify`
                // function! (We could also call `unify_non_vars`, but whatever.)
                Root::ForNonVar(x, _size) => self.unify(&x, y),

                // We're sure `x` is an unsolved variable, and `y` is a non-var.
                Root::OfVar(x_root_size) => {
                    // Every var who pointed to `x`'s root now points to the non-var `y`.
                    let new_root = Root::ForNonVar(y.clone(), x_root_size);
                    let new_node = Node::Root(new_root);
                    Some(self.insert(x.clone(), new_node))
                }
            },

            // Hmm this case looks familiar... Swap 'em and try again!
            (NonVar, Var(y)) => self.unify(&y.clone().into(), x),

            // At first glance, `x` and `y` both look like variables.
            (Var(x), Var(y)) => {
                // But what do their roots have to say on the matter?
                let (x, x_root_size) = self.find_root_and_size(x);
                let (y, y_root_size) = self.find_root_and_size(y);

                // Lets classify their roots.
                match (x.classify_term(), y.classify_term()) {
                    // We already know how to unify when either `x` or `y` is a nonvar:
                    // Use the `unify` function! Don't worry, we HAVE made progress!
                    (Var(_), NonVar) | (NonVar, Var(_)) | (NonVar, NonVar) => self.unify(&x, &y),

                    // Ok, `x` and `y` are FOR REAL unknowns.
                    (Var(x), Var(y)) => {
                        // Weighting heuristic.
                        let (small, large) = if x_root_size < y_root_size {
                            (x, y)
                        } else {
                            (y, x)
                        };

                        let new_size = x_root_size + y_root_size;

                        // Make the little guy (and all its siblings) point to the bigger
                        // guy. This is good because if the little guy has a TINY family
                        // and the big guy has a HUGE family, we wanna inconvenience
                        // the fewest number of people.
                        //
                        // (Making `small` point to `large` adds one more step in the
                        // lookup chain. Every time we do `find_root(small)`, it will take
                        // a little longer now.)
                        let new_self = self
                            .insert(small.clone(), Node::Child(large.clone()))
                            .insert(large.clone(), Node::Root(Root::OfVar(new_size)));

                        Some(new_self)
                    }
                }
            }
        }
    }

    fn unify_non_vars(&self, x: &Term, y: &Term) -> Option<Self> {
        debug_assert!(x.is_non_var() && y.is_non_var());

        // No way to unify two terms which are this different.
        if !x.same_variant(y) {
            return None;
        }

        let mut u = self.clone();

        for (x_child, y_child) in x.children().zip(y.children()) {
            u = u.unify(x_child, y_child)?;
        }

        Some(u)
    }

    fn find(&self, var: &Var) -> Term {
        let (root, _) = self.find_root_and_size(var);
        root
    }

    fn find_root_and_size(&self, var: &Var) -> (Term, usize) {
        match self.find_root(var) {
            Root::ForNonVar(term, size) => (term, size),
            Root::OfVar(size) => (var.clone().into(), size),
        }
    }

    fn get_associated(&self, var: &Var) -> Option<Node<Var, Term>> {
        self.map.borrow().get(var).cloned()
    }

    fn find_root(&self, var: &Var) -> Root<Term> {
        // NOTE: This is a load-bearing "extract to function" situation. Inlining the call
        // to `self.get_associated` causes a `RefCell` `BorrowMutError`.
        match self.get_associated(var) {
            None => {
                // Var has not been registered in the map yet, so put it in.
                let root = Root::OfVar(1);
                let node = Node::Root(root.clone());
                self.hidden_update(var.clone(), node);
                root
            }
            Some(Node::Root(root)) => root.clone(),
            Some(Node::Child(parent_var)) => {
                match self.find_root(&parent_var) {
                    Root::OfVar(size) => {
                        // Path compression heuristic.
                        let new_root = Root::OfVar(size + 1);
                        let new_node = Node::Root(new_root.clone());
                        self.hidden_update(var.clone(), new_node);
                        new_root
                    }

                    Root::ForNonVar(term, size) => {
                        debug_assert!(term.is_non_var());

                        // Path compression heuristic.
                        let new_root = Root::ForNonVar(term, size + 1);
                        let new_node = Node::Root(new_root.clone());
                        self.hidden_update(var.clone(), new_node);
                        new_root
                    }
                }
            }
        }
    }

    pub fn reify_term(&self, term: &Term) -> Term {
        term.map_children(|child| match child.classify_term() {
            TermKind::NonVar => self.reify_term(&child),
            TermKind::Var(var) => self.find(var),
        })
    }
}
