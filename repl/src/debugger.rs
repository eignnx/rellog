use core::fmt;
use std::{collections::BTreeSet, io::Write, ops::ControlFlow, str::FromStr};

use librellog::{
    ast::{RcTm, Tm},
    interner::IStr,
    rt::{breakpoint::Event, Rt, UnifierSet},
};
use nu_ansi_term::Color;

#[derive(Debug)]
pub struct Debugger {
    pub watch_criteria: BTreeSet<WatchCriterion>,
}

impl Default for Debugger {
    fn default() -> Self {
        Self {
            watch_criteria: [WatchCriterion::Any].into_iter().collect(),
        }
    }
}

impl librellog::rt::breakpoint::Breakpoint for Debugger {
    fn breakpoint(&mut self, rt: &Rt, event: Event) {
        if !rt.debug_mode.get() {
            return;
        }

        let query = rt.current_query();

        if !self.matches_watch_criteria(&query, event) {
            return;
        }

        loop {
            let depth = rt.recursion_depth.get();
            println!("[[breakpoint][event {event}][depth {depth}][query");
            println!("  {}", Color::Yellow.paint(format!("{query}")));
            println!("]]");

            print!("[[dbg]]-- ");
            std::io::stdout().flush().unwrap();
            let mut src_buf = String::new();
            // let _ = std::io::stdin().read_line(&mut src_buf).unwrap();
            let _ = std::io::stdin().read_line(&mut src_buf).unwrap();
            println!();

            let mut tok_buf = Vec::new();
            let ts = match librellog::lex::tokenize_into(
                &mut tok_buf,
                &src_buf[..],
                "<debugger cmd input>".into(),
            ) {
                Ok(ts) => ts,
                Err(_) if src_buf.trim().is_empty() => {
                    // Step.
                    println!("# Stepping...");
                    break;
                }
                Err(e) => {
                    println!("Command error: {e}");
                    continue;
                }
            };
            let cmd = match librellog::parse::entire_term(ts) {
                Ok(tm) => tm,
                Err(_) if src_buf.trim().is_empty() => {
                    // Step.
                    println!("# Stepping...");
                    break;
                }
                Err(e) => {
                    println!("Command error: {e}");
                    continue;
                }
            };

            match self.interpret_cmd(rt, cmd) {
                ControlFlow::Break(()) => {
                    println!();
                    break;
                }
                ControlFlow::Continue(()) => continue,
            }
        }
    }
}

macro_rules! rel_match {
    ($expr:expr, { $( $([$key:ident = $value:pat])+ $(| $([$key2:ident = $value2:ident])+)* => $block:expr, )* else => $default:expr }) => {
        loop {
            $(
                $(
                    let $key = $expr.get(&IStr::from(stringify!($key)));
                )+
                if let ( $(Some($value),)+ ) = ( $($key, )+ ) {
                    break $block;
                }

                $(
                    $(
                        let $key2 = $expr.get(&IStr::from(stringify!($key2)));
                    )+
                    if let ( $(Some($value2),)+ ) = ( $($key2, )+ ) {
                        break $block;
                    }
                )*
            )*

            break $default;
        }
    };
}

impl Debugger {
    pub fn matches_watch_criteria(&self, query: &RcTm, event: Event) -> bool {
        self.watch_criteria
            .iter()
            .all(|qf| qf.matches(query, event))
    }

    fn interpret_cmd(&mut self, rt: &Rt, cmd: RcTm) -> ControlFlow<(), ()> {
        match cmd.as_ref() {
            Tm::Sym(s) => match s.to_str().as_ref() {
                "help" | "h" | "?" => {
                    self.print_help();
                    ControlFlow::Continue(())
                }
                "nodebug" | "nodbg" | "dbg" | "d" => {
                    rt.debug_mode.set(false);
                    ControlFlow::Break(())
                }
                "filters" | "f" | "watch_critera" | "wc" => {
                    println!("# Current Watch Critera:");
                    for (i, wc) in self.watch_criteria.iter().enumerate() {
                        println!("    - {wc} #{}", i + 1);
                    }
                    println!();
                    ControlFlow::Continue(())
                }
                "step" | "s" => {
                    println!("[[step]]");
                    ControlFlow::Break(())
                }
                _ => {
                    println!("# Unknown debugger command: `{cmd}`");
                    println!("# Type `help` to show debugger help.");
                    println!();
                    ControlFlow::Continue(())
                }
            },
            Tm::Var(_) => {
                self.watch_criteria.insert(WatchCriterion::Any);
                ControlFlow::Break(())
            }
            Tm::Rel(rel) => rel_match!(
                rel,
                {
                    [event = e_sym] | [e = e_sym] => {
                        if let Some(e) = e_sym.try_as_sym().and_then(|e| Event::from_str(e.to_str().as_ref()).ok()) {
                            self.watch_criteria.insert(WatchCriterion::Event(e, Box::new(WatchCriterion::Any)));
                            ControlFlow::Break(())
                        } else {
                            println!("# Unknown debugger event: `{e_sym}`");
                            println!("# Valid events are:");
                            println!("#   - call");
                            println!("#   - exit");
                            println!("#   - redo");
                            println!("#   - fail");
                            println!("#   - exception");
                            println!("#   - unify");
                            println!();
                            ControlFlow::Continue(())
                        }
                    },
                    [watch = query_tm] | [w = query_tm] => {
                        let wc = WatchCriterion::QueryTm(query_tm.clone());
                        self.watch_criteria.insert(wc);
                        ControlFlow::Break(())
                    },
                    [event = e_sym][watch = query_tm] | [e = e_sym][w = query_tm] => {
                        if let Some(e) = e_sym.try_as_sym().and_then(|e| Event::from_str(e.to_str().as_ref()).ok()) {
                            let q = WatchCriterion::QueryTm(query_tm.clone());
                            self.watch_criteria.insert(WatchCriterion::Event(e, Box::new(q)));
                            ControlFlow::Break(())
                        } else {
                            println!("# Unknown debugger event: `{e_sym}`");
                            println!("# Valid events are:");
                            println!("#   - call");
                            println!("#   - exit");
                            println!("#   - redo");
                            println!("#   - fail");
                            println!("#   - exception");
                            println!("#   - unify");
                            println!();
                            ControlFlow::Continue(())
                        }
                    },
                    else => {
                        println!("# Unknown debugger command: `{cmd}`");
                        println!("# Type `help` to show debugger help.");
                        println!();
                        ControlFlow::Continue(())
                    }
                }
            ),
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
        println!("#  :help | :h | ?             Show debugger help.");
        println!("#  :nodebug | :d | :nodbg     Quit debugger.");
        println!("#  :step | <ENTER>            Advance to the goal to be solved.");
        println!("#  (:watch | :w) WATCH_CRIT   Add a watch criterion.");
        println!("#");
        println!("#  Watch Criteria");
        println!("#  VAR | _             Matches any event.");
        println!("#  [e EVENT]           Matches a specific event.");
        println!("#  [w GOAL]            Matches a specific rellog goal (may contain vars).");
        println!("#  [e EVENT][w GOAL]   Matches a specific term at a specific event.");
        println!("#");
        println!("#  Events");
        println!("#  call        When a query is called.");
        println!("#  exit        When a query exits successfully.");
        println!("#  redo        When a query is called.");
        println!("#  fail        When a query is called.");
        println!("#  exception   When a query raises an exception.");
        println!("#  unify       When a query head is unified.");
        println!("}}");
        println!();
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum WatchCriterion {
    Any,
    QueryTm(RcTm),
    Event(Event, Box<WatchCriterion>),
}

impl WatchCriterion {
    pub fn matches(&self, query: &RcTm, event: Event) -> bool {
        match self {
            Self::Any => true,
            Self::QueryTm(tm) => UnifierSet::new().unify(tm, query).is_some(),
            Self::Event(e, wc) => *e == event && wc.matches(query, event),
        }
    }
}

impl fmt::Display for WatchCriterion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WatchCriterion::Any => write!(f, "Any"),
            WatchCriterion::QueryTm(tm) => write!(f, "[w {tm}]"),
            WatchCriterion::Event(e, tm) => write!(f, "[e {e}][w {tm}"),
        }
    }
}
