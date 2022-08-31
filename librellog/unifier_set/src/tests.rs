#![cfg(test)]

use std::{fmt, iter};

use crate::*;

use Term::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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

    fn superficially_unifiable(&self, other: &Self) -> bool {
        match (self, other) {
            (Var(_), Var(_)) => true,
            (Pred(a, _), Pred(b, _)) if a == b => true,
            _ => false,
        }
    }
}

impl DirectChildren for Term {
    fn direct_children<'a>(&'a self) -> Box<dyn Iterator<Item = &'a Self> + 'a> {
        match self {
            Var(_) => Box::new(iter::empty()),
            Pred(_, cs) => Box::new(cs.iter()),
        }
    }

    fn map_direct_children<'a>(&'a self, f: impl FnMut(&'a Self) -> Self + 'a) -> Self {
        match self {
            Var(_) => self.clone(),
            Pred(h, cs) => Pred(h.clone(), cs.iter().map(f).collect()),
        }
    }
}

impl From<&'static str> for Term {
    fn from(v: &'static str) -> Self {
        Var(v)
    }
}

#[test]
fn basic_socrates_example() {
    let u = UnifierSet::new();

    let p1 = Pred("mortal", vec![Pred("socrates", vec![])]);
    let p2 = Pred("mortal", vec![Var("Who")]);

    let u = u.unify(&p1, &p2).unwrap();

    assert_eq!(format!("{u}"), "    - Who = socrates()\n".to_string());
}

#[test]
fn expanded_socrates_example() {
    let u = UnifierSet::new();

    let p1 = Pred("mortal", vec![Pred("socrates", vec![])]);
    let p2 = Pred("mortal", vec![Var("Who")]);

    let u = u.unify(&p1, &p2).unwrap();
    let u = u.unify(&Var("Person"), &Var("Who")).unwrap();
    let u = u.unify(&Var("Other"), &Pred("john", vec![])).unwrap();
    let u = u.unify(&Var("Singleton"), &Var("Singleton")).unwrap();

    assert_eq!(
        format!("{u}"),
        [
            "    - Other = john()\n",
            "    - Person = Who = socrates()\n"
        ]
        .concat()
    );
}

#[test]
fn two_vars_unify() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("Y")).unwrap();
    assert_eq!(format!("{u}"), "    - X = Y\n".to_string());
}

#[test]
fn two_vars_unify_twice() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("Y")).unwrap();
    let u = u.unify(&Var("Y"), &Var("X")).unwrap();
    assert_eq!(format!("{u}"), "    - X = Y\n".to_string());
}

#[test]
fn var_unifies_with_atomic_pred() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Pred("the_answer", vec![])).unwrap();
    assert_eq!(format!("{u}"), "    - X = the_answer()\n".to_string());
}

#[test]
fn x_equals_x() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("X")).unwrap();
    assert_eq!(format!("{u}"), "".to_string());
}

#[test]
fn occurs_check_behavior() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Pred("shell", vec![Var("X")])).unwrap();
    assert_eq!(format!("{u}"), "    - X = shell(X)\n".to_string());
}
