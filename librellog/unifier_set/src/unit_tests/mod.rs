use super::*;
use std::{fmt, iter};

mod graph_viz;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Term {
    Var(&'static str),
    Pred(&'static str, Vec<Term>),
}

use Term::*;

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

impl DirectChildren<&'static str> for Term {
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

    assert_eq!(
        format!("{u:?}"),
        r#"{{Pred("socrates", []), "Who"}}"#.to_string()
    );
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
        format!("{u:?}"),
        r#"{{Var("Singleton")}{Pred("john", []), "Other"}{Pred("socrates", []), "Person", "Who"}}"#
    );
}

#[test]
fn two_vars_unify() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("Y")).unwrap();
    assert_eq!(format!("{u:?}"), r#"{{Var("Y"), "X"}}"#.to_string());
}

#[test]
fn two_vars_unify_twice() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("Y")).unwrap();
    let u = u.unify(&Var("Y"), &Var("X")).unwrap();
    assert_eq!(format!("{u:?}"), r#"{{Var("Y"), "X"}}"#.to_string());
}

#[test]
fn var_unifies_with_atomic_pred() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Pred("the_answer", vec![])).unwrap();
    assert_eq!(
        format!("{u:?}"),
        r#"{{Pred("the_answer", []), "X"}}"#.to_string()
    );
}

#[test]
fn x_equals_x() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Var("X")).unwrap();
    assert_eq!(format!("{u:?}"), r#"{{Var("X")}}"#.to_string());
}

#[test]
fn recursive_term_behavior() {
    let u = UnifierSet::new();
    let res = u.unify(&Var("X"), &Pred("shell", vec![Var("X")]));
    assert!(res.is_some());
}

#[test]
#[ignore] // TODO: Print recursive terms nicely.
fn recursive_term_display_behavior() {
    let u = UnifierSet::new();
    let u = u.unify(&Var("X"), &Pred("shell", vec![Var("X")])).unwrap();
    assert_eq!(format!("{u:?}"), r#"{{"X", shell("X")}}"#.to_string());
}

#[test]
fn reify_simple_term() {
    let u = UnifierSet::new();

    let v = Var("V");
    let w = Var("W");
    let x = Var("X");
    let y = Var("Y");
    let z = Var("Z");
    let object = Pred("my_sofa", vec![]);

    let u = u.unify(&v, &w).unwrap();
    u.print_to_dot_file("after_unify_1.gv").unwrap();
    let u = u.unify(&w, &x).unwrap();
    u.print_to_dot_file("after_unify_2.gv").unwrap();
    let u = u.unify(&x, &y).unwrap();
    u.print_to_dot_file("after_unify_3.gv").unwrap();
    let u = u.unify(&y, &z).unwrap();
    u.print_to_dot_file("after_unify_4.gv").unwrap();
    let u = u.unify(&x, &object).unwrap();
    u.print_to_dot_file("after_unify_5.gv").unwrap();

    // This one had BETTER work.
    assert_eq!(&u.reify_term(&x), &object);

    assert_eq!(&u.reify_term(&v), &object);
    assert_eq!(&u.reify_term(&w), &object);
    assert_eq!(&u.reify_term(&y), &object);
    assert_eq!(&u.reify_term(&z), &object);
}

#[test]
fn reify_term() {
    let u = UnifierSet::new();

    let v = Var("V");
    let w = Var("W");
    let x = Var("X");
    let y = Var("Y");
    let z = Var("Z");

    let object = Pred("one_var", vec![z.clone()]);
    let p1 = Pred(
        "thing",
        vec![v.clone(), Pred("equiv", vec![x.clone(), x.clone()])],
    );
    let p2 = Pred(
        "thing",
        vec![y.clone(), Pred("equiv", vec![y.clone(), object.clone()])],
    );

    let u = u.unify(&p1, &w).unwrap(); // thing(V, equiv(X, X)) = W.
    let u = u.unify(&w, &p2).unwrap(); // W = thing(Y, equiv(Y, one_var(Z))).

    u.print_to_dot_file("reify_term.after.gv").unwrap();

    assert_eq!(&u.reify_term(&z).to_string(), "Z"); // Z is still a var.
    assert_eq!(&u.reify_term(&x).to_string(), "one_var(Z)"); // X -> one_var(Z)
    assert_eq!(&u.reify_term(&y).to_string(), "one_var(Z)"); // Y -> one_var(Z)
    assert_eq!(&u.reify_term(&v).to_string(), "one_var(Z)"); // V -> one_var(Z)
    assert_eq!(
        &u.reify_term(&w).to_string(),
        "thing(one_var(Z), equiv(one_var(Z), one_var(Z)))"
    );
}

#[test]
fn variables_of_var() {
    let vs = Var("X").variables().copied().collect::<Vec<&str>>();
    assert_eq!(&vs, &["X"]);
}

#[test]
fn variables_of_compound_term() {
    let x = Var("X");
    let y = Var("Y");

    let term = Pred(
        "thing",
        vec![x.clone(), Pred("equiv", vec![y.clone(), y.clone()])],
    );

    let vs = term.variables().copied().collect::<Vec<&str>>();

    assert_eq!(&vs, &["X", "Y", "Y"]);
}
