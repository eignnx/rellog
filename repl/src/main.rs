#![deny(unused_must_use)]

use std::path::PathBuf;

use librellog::STD_LIB_ROOT;

mod app_err;
mod debugger;
mod line_editor;
mod line_editor_config;
mod repl;

fn main() {
    human_panic::setup_panic!();
    librellog::init_interner();

    println!("Rellog REPL");

    let mut repl = repl::Repl::new_loading_files(vec![]);

    let args: Vec<_> = std::env::args().skip(1).collect();

    let mut load_std_lib = true;

    for arg in &args {
        if arg == "--no-std" {
            load_std_lib = false;
        } else {
            let fname = arg;
            if let Err(err) = repl.load_file(fname.clone()) {
                println!("Error loading file {}: {}", fname, err);
            }
        }
    }

    if load_std_lib {
        let std_lib = PathBuf::from(STD_LIB_ROOT).join("std.rellog");
        if let Err(err) = repl.load_file(&std_lib) {
            println!("Error loading standard library: {}", err);
        }
    }

    if args.is_empty() || args == ["--no-std"] {
        println!("Running in interactive mode.");
        println!("Type `:help` for a list of commands.");
        println!("Use `:load` to load a source file.");
    }

    repl.run()
}
