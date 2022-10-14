use std::rc::Rc;

use crate::{char_list::FollowTail, CharList};

#[derive(Clone)]
enum Tail {
    Next(Rc<CharList<Tail>>),
    Nil,
}

impl FollowTail for Tail {
    fn follow_tail(&self) -> Option<&CharList<Self>> {
        match self {
            Tail::Nil => None,
            Tail::Next(next) => Some(next),
        }
    }

    fn make_tail(suffix: CharList<Self>) -> Self {
        Tail::Next(Rc::new(suffix))
    }

    fn nil() -> Self {
        Tail::Nil
    }
}

#[test]
fn round_trip() {
    let s = "alsdjfka js dglkj ahsdgh asdfljkh alod fh akljsd fnlau usdfl kadgs";
    let cl: CharList<Tail> = s.into();
    assert_eq!(s, cl.to_string());
}
