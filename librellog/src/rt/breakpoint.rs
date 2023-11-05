use core::fmt;
use std::str::FromStr;

use super::Rt;

pub trait Breakpoint {
    fn breakpoint(&mut self, rt: &Rt, title: Event);
}

/// See [Byrd Box Model](https://www.swi-prolog.org/pldoc/man?section=byrd-box-model)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Event {
    Call,
    Exit,
    Redo,
    Fail,
    Exception,
    Unify,
}

impl fmt::Display for Event {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Event::Call => "call",
                Event::Exit => "exit",
                Event::Redo => "redo",
                Event::Fail => "fail",
                Event::Exception => "exception",
                Event::Unify => "unify",
            }
        )
    }
}

impl FromStr for Event {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "call" => Ok(Event::Call),
            "exit" => Ok(Event::Exit),
            "redo" => Ok(Event::Redo),
            "fail" => Ok(Event::Fail),
            "exception" => Ok(Event::Exception),
            "unify" => Ok(Event::Unify),
            _ => Err(()),
        }
    }
}
