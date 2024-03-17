use std::{
    cell::RefCell,
    collections::BTreeSet,
    ops::ControlFlow,
    path::{Path, PathBuf},
    sync::Arc,
};

use librellog::{
    ast::{dup::TmDuplicator, RcTm},
    lex::{
        self,
        tok::{At, Tok},
    },
    parse,
    rt::UnifierSet,
    session::Session,
    utils::display_unifier_set::DisplayUnifierSet,
};
use nu_ansi_term::Color;
use reedline::Signal;

use crate::{
    app_err::{AppErr, AppRes},
    debugger::Debugger,
    line_editor::LineEditor,
    line_editor_config::ReplMode,
};

pub struct Repl {
    session: Session,
    line_editor: Arc<LineEditor>,
    files_loaded: BTreeSet<String>,
    files_failed_to_load: BTreeSet<String>,
}

impl Repl {
    pub fn new_loading_files(fnames: Vec<String>) -> Self {
        let line_editor = Arc::new(LineEditor::new());
        let mut session = Session::new(".").unwrap_or_else(|e| {
            println!(
                "{}",
                Color::Red.paint(format!("# Error creating session: {e}"))
            );
            println!("{}", Color::Red.paint("# Exiting..."));
            std::process::exit(1);
        });

        session.rt = session.rt.with_debugger(Debugger::new(line_editor.clone()));
        let mut repl = Self {
            session,
            line_editor,
            files_loaded: BTreeSet::new(),
            files_failed_to_load: BTreeSet::new(),
        };

        for fname in fnames {
            let _ = repl.load_file(fname);
        }

        repl
    }

    pub fn load_file<'ts>(&mut self, fname: impl AsRef<Path>) -> AppRes<'ts, ()> {
        let fname = fname.as_ref();
        let fname_path = PathBuf::from(fname);
        let (nrels, ndirs) = match self.session.load_file(fname_path) {
            Ok(m) => m,
            Err(e) => {
                println!(
                    "{}",
                    Color::Red.paint(format!(
                        "# Error loading file `{}`:\n\t{}",
                        &fname.display(),
                        &e
                    ))
                );
                self.files_failed_to_load
                    .insert(fname.display().to_string());
                return Err(AppErr::RtError(e));
            }
        };
        println!(
            "{}",
            Color::Yellow.paint(format!(
                "# Loaded `{}` ({nrels} relations, {ndirs} directives).",
                fname.display()
            ))
        );
        self.files_loaded.insert(fname.display().to_string());
        Ok(())
    }

    pub fn run(&mut self) {
        let mut tok_buf = Vec::new();

        'outer: loop {
            let debug = self.session.rt.debug_mode.get();
            self.line_editor.set_repl_mode(ReplMode::TopLevel { debug });

            let query_buf = match self.line_editor.read_line() {
                Ok(Signal::Success(s)) => s,
                Ok(Signal::CtrlC | Signal::CtrlD) => break 'outer,
                Err(e) => {
                    println!("Unable to read line from terminal: {e}");
                    println!("Exiting...");
                    break 'outer;
                }
            };

            let query_parts: Vec<&str> = query_buf.trim().split_ascii_whitespace().collect();

            match query_parts[..] {
                ["help" | ":help" | ":h" | "?"] => {
                    print_help();
                    continue 'outer;
                }
                [":quit" | ":q" | ":exit" | ":wq"] => {
                    println!("Exiting Rellog REPL...");
                    break 'outer;
                }
                [":debug" | ":d"] => {
                    let new_mode = !self.session.rt.debug_mode.get();
                    self.session.rt.debug_mode.set(new_mode);
                    if new_mode {
                        println!("{}", Color::Yellow.paint("# Entering debug mode."));
                    } else {
                        println!("{}", Color::Yellow.paint("# Exiting debug mode."));
                    }
                    continue 'outer;
                }
                [":reload" | ":r"] => {
                    self.reload(&mut tok_buf);
                    continue 'outer;
                }
                [":load" | ":l" | ":unload" | ":u"] => {
                    println!(
                        "# Which file would you like to load? Please specify `{query_buf} FILENAME`"
                    );
                    continue 'outer;
                }
                [":load" | ":l", fname] => {
                    if let Some(fname) = self.parse_filename(fname) {
                        let _ = self.load_file(fname);
                    }
                    continue 'outer;
                }
                [":unload" | ":u", fname] => {
                    let a = self.files_loaded.remove(fname);
                    let b = self.files_failed_to_load.remove(fname);
                    if a || b {
                        println!(
                            "{}",
                            Color::Yellow
                                .paint(format!("# Removing file `{fname}` from reload list."))
                        );
                    } else {
                        println!(
                            "{}",
                            Color::Yellow.paint(format!(
                                "# The file `{fname}` already does not appear in the reload list:"
                            ))
                        );
                        for fname in self.files_loaded.difference(&self.files_failed_to_load) {
                            println!(
                                "{}",
                                Color::Yellow.paint(format!("#     - `{fname}` (loaded)"))
                            );
                        }
                        for fname in self.files_failed_to_load.difference(&self.files_loaded) {
                            println!(
                                "{}",
                                Color::Yellow.paint(format!("#     - `{fname}` (failed to load)"))
                            );
                        }
                        continue 'outer;
                    }
                    println!("{}", Color::Yellow.paint("# Reloading..."));
                    self.reload(&mut tok_buf);
                    continue 'outer;
                }
                _ => {}
            }

            let query = match parse_term(&query_buf[..]) {
                Some(value) => value,
                None => continue 'outer,
            };

            match self.solve_and_display_solns(query) {
                ControlFlow::Continue(_) => continue 'outer,
                ControlFlow::Break(_) => break 'outer,
            }
        }
    }

    fn solve_and_display_solns(&mut self, query: RcTm) -> ControlFlow<(), ()> {
        let mut no_solns_yet_found = true;
        let u = UnifierSet::new();
        let td = RefCell::new(TmDuplicator::default());
        let mut solns = self.session.solve_query(query, u, &td);
        let mut or_bar_printed = false;

        let debug = self.session.rt.debug_mode.get();
        self.line_editor
            .set_repl_mode(ReplMode::PrintingSolns { debug });

        while let Some(soln) = solns.next() {
            let soln = match soln {
                Ok(soln) => {
                    let disp = DisplayUnifierSet {
                        u: soln.clone(),
                        display_or_bar: or_bar_printed,
                    };
                    print!("{disp}");
                    no_solns_yet_found = false;
                    soln
                }
                Err(e) => {
                    println!("{}", Color::Red.paint(format!("Exception: {e}")));
                    for (i, q) in self
                        .session
                        .rt
                        .query_stack
                        .borrow()
                        .iter()
                        .enumerate()
                        .rev()
                    {
                        println!("{}", Color::Red.paint(format!("# ^[depth:{i:0>2}]: `{q}`")));
                    }
                    return ControlFlow::Continue(());
                }
            };

            // If we *know* there are no more solutions...
            match solns.size_hint() {
                (_, Some(0)) if soln.is_empty() => {
                    let msg = Color::Yellow.paint("# The query holds unconditionally.");
                    println!(" {msg}");
                    return ControlFlow::Continue(());
                }
                (_, Some(0)) => {
                    let msg = Color::Yellow.paint("# Exactly 1 solution found.");
                    println!(" {msg}");
                    return ControlFlow::Continue(());
                }
                (lo, Some(hi)) if lo == hi => {
                    let msg = Color::Yellow.paint(format!("# {lo} solution(s) remain."));
                    println!(" {msg}");
                }
                (_, Some(hi)) => {
                    let msg = Color::Yellow.paint(format!("# At most {hi} solution(s) remain."));
                    println!(" {msg}");
                }
                _ => {}
            }

            match self.line_editor.read_line().unwrap() {
                Signal::Success(_) => {}
                Signal::CtrlC => {
                    println!("...");
                    return ControlFlow::Continue(());
                }
                Signal::CtrlD => return ControlFlow::Break(()),
            }

            print!("\n|"); // Print an "or"; more solns incoming.
            or_bar_printed = true;
        }

        if !or_bar_printed {
            print!(" ");
        }

        if no_solns_yet_found {
            println!(
                "   - [false] {}",
                Color::Yellow.paint("# The query has no solutions.")
            );
        } else {
            println!(
                "   - [false] {}",
                Color::Yellow.paint("# No additional solutions found.")
            );
        }

        if self.session.rt.debug_mode.get() {
            for (i, q) in self
                .session
                .rt
                .query_stack
                .borrow()
                .iter()
                .enumerate()
                .rev()
            {
                println!(
                    "{}",
                    Color::Yellow.paint(format!(
                        "# ^[depth:{i:0>2}]: `{}`",
                        Color::LightYellow.paint(q.to_string())
                    ))
                );
            }
        }

        ControlFlow::Continue(())
    }

    fn reload(&mut self, _tok_buf: &mut [At<Tok>]) {
        self.session.rt.db.clear();

        let to_load: Vec<String> = self
            .files_loaded
            .union(&self.files_failed_to_load)
            .cloned()
            .collect();

        for fname in to_load {
            let _ = self.load_file(fname);
        }
    }

    /// Allows both a native file path (using OS path separator) and a module
    /// path (using `::` as a separator).
    ///
    /// If the path begins with `std::`, it is assumed to be a module path
    /// referencing the standard library.
    ///
    /// Also handles the case where the `.rellog` file extension is omitted.
    fn parse_filename(&self, fname: &str) -> Option<PathBuf> {
        if let Some((_std, fname)) = fname.trim_start().split_once("std::") {
            let mut path = PathBuf::from(librellog::STD_LIB_ROOT);
            for part in fname.split("::") {
                path.push(part);
            }
            if path.extension().is_none() {
                path.set_extension("rellog");
            } else {
                println!(
                    "{}",
                    Color::Yellow.paint(
                        "# Note: you don't need to include the `.rellog` \
                        extension when importing via a module path."
                    )
                );
            }
            Some(path)
        } else {
            Some(PathBuf::from(fname))
        }
    }
}

fn print_help() {
    println!("Enter a RELLOG TERM or one of these REPL COMMANDS:");
    println!("  :help | :h | ?    Displays this help text.");
    println!("  :load | :l        Load the source given source file.");
    println!("  :reload | :r      Reloads the source file(s).");
    println!("  :unload | :u      Unloads the given source file.");
    println!("  :debug | :d       Enters/exits debugging mode.");
    println!("  [Builtins]        Show a list of builtin relations.");
    println!("  [help Sig]        Show help text for a relation given by `Sig`.");
    println!("  :quit | :q |      Exit the repl.");
    println!("… :exit | :e |");
    println!("… :wq");
}

fn parse_term(src: &str) -> Option<RcTm> {
    let mut buf = Vec::new();
    let tokens = match lex::tokenize_into(&mut buf, src, "<user input>".into()) {
        Ok(ts) => ts,
        Err(le) => {
            println!("{}", Color::Red.paint(format!("Tokenization error: {le}")));
            return None;
        }
    };
    let query = match parse::entire_term(tokens) {
        Ok(q) => q,
        Err(e) => {
            println!("Parse error: {e}");
            return None;
        }
    };
    Some(query.source_vars_to_repl_vars())
}

// fn load_module_from_file(tok_buf: &mut Vec<At<Tok>>, fname: String) -> AppRes<ast::Module> {
//     let f = std::fs::File::open(&fname).map_err(|e| AppErr::FileOpen(fname.clone(), e))?;
//     let mut r = io::BufReader::new(f);
//     let mut src = String::new();
//     r.read_to_string(&mut src)
//         .map_err(|e| AppErr::FileRead(fname.clone(), e))?;
//     load_module_from_string(tok_buf, src, fname.into())
// }

// fn load_module_from_string(
//     tok_buf: &mut Vec<At<Tok>>,
//     src: impl AsRef<str>,
//     filename: PathBuf,
// ) -> AppRes<ast::Module> {
//     let tokens = lex::tokenize_into(tok_buf, src.as_ref(), filename.clone())?;

//     let m = match parse::entire_module(tokens) {
//         Ok(m) => m,
//         Err(verbose_err) => {
//             println!("{verbose_err}");
//             return Err(AppErr::Parse {
//                 fname: Some(filename),
//                 err: verbose_err,
//             });
//         }
//     };

//     Ok(m)
// }
