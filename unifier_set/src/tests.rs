#![cfg(test)]

use std::{fmt, iter, vec};

use crate::*;

use Term::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Term {
    Var(&'static str),
    Pred(&'static str, Vec<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Var(var) => write!(f, "{var}"),
            Pred(head, children) => {
                let children = children
                    .iter()
                    .map(|c| format!("{c}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{head}({children})")
            }
        }
    }
}

impl ClassifyTerm<&'static str> for Term {
    fn classify_term(&self) -> TermKind<&&'static str> {
        match self {
            Var(v) => TermKind::Var(v),
            Pred(_, _) => TermKind::NonVar,
        }
    }
}

impl SameVariant for Term {
    fn same_variant(&self, other: &Self) -> bool {
        match (self, other) {
            (Var(_), Var(_)) => true,
            (Pred(a, _), Pred(b, _)) if a == b => true,
            _ => false,
        }
    }
}

impl Children for Term {
    fn children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a> {
        match self {
            Var(_) => Box::new(iter::empty()),
            Pred(_, cs) => Box::new(cs.iter()),
        }
    }

    fn map_children(&self, f: impl FnMut(Self) -> Self) -> Self {
        match self {
            Var(_) => self.clone(),
            Pred(h, cs) => Pred(h.clone(), cs.iter().cloned().map(f).collect()),
        }
    }
}

impl From<&'static str> for Term {
    fn from(v: &'static str) -> Self {
        Var(v)
    }
}

#[test]
fn test_basic_socrates() {
    let u = UnifierSet::new();

    let p1 = Pred("mortal", vec![Pred("socrates", vec![])]);
    let p2 = Pred("mortal", vec![Var("Who")]);

    let u = u.unify(&p1, &p2).unwrap();

    assert_eq!(format!("{u}"), "    - Who = socrates()\n".to_string());
}

#[test]
fn test_two_vars_unify() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("Y")).unwrap();
    assert_eq!(format!("{u}"), "    - X = Y".to_string());
}

#[test]
fn test_var_unifies_with_atomic_pred() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Pred("the_answer", vec![])).unwrap();
    assert_eq!(format!("{u}"), "    - X = the_answer()\n".to_string());
}
