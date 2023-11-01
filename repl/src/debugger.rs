use std::{collections::BTreeSet, io::Write, ops::ControlFlow};

use librellog::{
    ast::{RcTm, Sig, Tm},
    lex::tok::Tok,
    rt::{Rt, UnifierSet},
};

pub struct Debugger {
    pub filters: BTreeSet<QueryFilter>,
}

impl Default for Debugger {
    fn default() -> Self {
        use QueryFilter::*;
        Self {
            filters: [Ignore(Box::new(AndBlocks)), Ignore(Box::new(OrBlocks))]
                .into_iter()
                .collect(),
        }
    }
}

impl librellog::rt::breakpoint::Breakpoint for Debugger {
    fn breakpoint(&mut self, rt: &Rt, title: &str) {
        if !rt.debug_mode.get() {
            return;
        }

        let query = rt.current_query();

        if self.ignores(&query, title) {
            return;
        }

        loop {
            println!("[[breakpoint]]");
            println!("[[depth:{:0>2}]]", rt.recursion_depth.get());
            println!("[[event '{title}'][query]]");
            println!("{query}");

            print!("[[dbg]]-- ");
            std::io::stdout().flush().unwrap();
            let mut cmd_buf = String::new();
            let _ = std::io::stdin().read_line(&mut cmd_buf).unwrap();
            println!();

            match self.interpret_cmd(rt, &cmd_buf) {
                ControlFlow::Break(()) => {
                    println!();
                    break;
                }
                ControlFlow::Continue(()) => continue,
            }
        }
    }
}

impl Debugger {
    pub fn ignores(&self, query: &RcTm, title: &str) -> bool {
        self.filters.iter().any(|qf| !qf.allows(query, title))
    }

    fn interpret_cmd(&mut self, rt: &Rt, cmd: &str) -> ControlFlow<(), ()> {
        let cmd = cmd.trim();
        match cmd {
            ":help" | "help" | "?" | ":h" | "-h" | "--help" => {
                self.print_help();
                ControlFlow::Continue(())
            }
            ":nodebug" | ":nodbg" | ":dbg" | ":d" => {
                rt.debug_mode.set(false);
                ControlFlow::Continue(())
            }
            "" | ":step" | ":s" => ControlFlow::Break(()),
            _ => {
                println!("# Unknown debugger command: `{cmd}`");
                println!("# Type `help` to show debugger help.");
                println!();
                ControlFlow::Continue(())
            }
        }
    }

    fn print_help(&self) {
        println!("[[debugger_help]]");
        println!("{{");
        println!("#  :help | :h | ?           Show debugger help.");
        println!("#  :nodebug | :d | :nodbg   Quit debugger.");
        println!("#  :step | <ENTER>          Advance to the goal to be solved.");
        println!("#  :+ FILTER                Add a debug filter");
        println!("#");
        println!("#  FILTERS");
        println!("#  todo!()");
        println!("}}");
        println!();
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum QueryFilter {
    Ignore(Box<QueryFilter>),
    AndBlocks,
    OrBlocks,
    RelWithSig(Sig),
    Title(String),
}

impl QueryFilter {
    pub fn allows(&self, query: &RcTm, title: &str) -> bool {
        match self {
            Self::Ignore(qf) => !qf.allows(query, title),
            Self::AndBlocks => matches!(query.as_ref(), Tm::Block(Tok::Dash, _)),
            Self::OrBlocks => matches!(query.as_ref(), Tm::Block(Tok::Pipe, _)),
            Self::RelWithSig(sig) => {
                let sig_tm = RcTm::from(sig.clone());
                let u = UnifierSet::new();
                u.unify(&sig_tm, query).is_some()
            }
            Self::Title(t) => title == t,
        }
    }
}
