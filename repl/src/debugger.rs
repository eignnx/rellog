use core::fmt;
use std::{collections::BTreeSet, ops::ControlFlow, str::FromStr, sync::Arc};

use librellog::{
    ast::{tm_displayer::TmDisplayer, RcTm, Tm},
    interner::IStr,
    rt::{breakpoint::Event, Rt, UnifierSet},
};
use nu_ansi_term::Color;
use reedline::Signal;

use crate::line_editor::LineEditor;

pub struct Debugger {
    line_editor: Arc<LineEditor>,
    pub watch_criteria: BTreeSet<WatchCriterion>,
}

impl Debugger {
    pub fn new(line_editor: Arc<LineEditor>) -> Self {
        Self {
            line_editor,
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
            let query_disp = TmDisplayer::default().indenting(query.as_ref());
            println!(
                "[[breakpoint][event {event}][depth {depth}][query",
                event = Color::Magenta.paint(event.to_string()),
                depth = Color::Cyan.paint(depth.to_string()),
            );
            println!("    {}", Color::Yellow.paint(query_disp.to_string()));
            println!("]]");

            let src_buf = match self.line_editor.read_line().unwrap() {
                Signal::Success(line) => line,
                Signal::CtrlC => {
                    println!("Exiting debugger...");
                    rt.debug_mode.set(false);
                    self.line_editor.toggle_debug_mode();
                    return;
                }
                Signal::CtrlD => {
                    println!("Exiting...");
                    std::process::exit(0);
                }
            };
            println!();

            if src_buf.trim().is_empty() {
                // Step.
                println!("# Stepping...");
                break;
            }

            let mut tok_buf = Vec::new();
            let filename = "<debugger cmd input>".into();

            let ts = match librellog::lex::tokenize_into(&mut tok_buf, &src_buf[..], filename) {
                Ok(ts) => ts,
                Err(e) => {
                    println!("Command error: {e}");
                    continue;
                }
            };

            let cmd = match librellog::parse::entire_term(ts) {
                Ok(tm) => tm,
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
                    self.line_editor.toggle_debug_mode();
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
            Tm::Rel(rel) => librellog::rel_match!(
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
                            println!("#   - exit      (TODO)");
                            println!("#   - redo      (TODO)");
                            println!("#   - fail      (TODO)");
                            println!("#   - exception (TODO)");
                            println!("#   - unify     (TODO");
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
                            println!("#   - exit      (TODO)");
                            println!("#   - redo      (TODO)");
                            println!("#   - fail      (TODO)");
                            println!("#   - exception (TODO)");
                            println!("#   - unify     (TODO");
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
        println!("# Debugger Help");
        println!("#  help | h                Show debugger help.");
        println!("#  nodebug | d | nodbg     Quit debugger.");
        println!("#  step | <ENTER>          Advance to the goal to be solved.");
        println!("#  (watch | w) WATCH_CRIT  Add a watch criterion.");
        println!("#");
        println!("# Watch Criteria");
        println!("#  VAR | _             Matches any event.");
        println!("#  [e EVENT]           Matches a specific event.");
        println!("#  [w GOAL]            Matches a specific rellog goal (may contain vars).");
        println!("#  [e EVENT][w GOAL]   Matches a specific term at a specific event.");
        println!("#");
        println!("# Events");
        println!("#  call        When a query is called.");
        println!("#  exit        When a query exits successfully.");
        println!("#  redo        When a query is called.");
        println!("#  fail        When a query is called.");
        println!("#  exception   When a query raises an exception.");
        println!("#  unify       When a query head is unified.");
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
