#![deny(unused_must_use)]

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
    let mut tok_buf = Vec::new();

    for arg in &args {
        if arg == "--no-std" {
            load_std_lib = false;
        } else {
            let fname = arg;
            tok_buf.clear();
            let _ = repl.load_file(&mut tok_buf, fname.clone());
        }
    }

    if load_std_lib {
        tok_buf.clear();
        let _ = repl.load_file(&mut tok_buf, "./librellog/src/std/std.rellog".into());
    }

    if args.is_empty() || args == ["--no-std"] {
        println!("Running in interactive mode.");
        println!("Type `:help` for a list of commands.");
        println!("Use `:load` to load a source file.");
    }

    repl.run()
}
