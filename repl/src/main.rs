#![deny(unused_must_use)]

mod app_err;
mod line_editor_config;
mod repl;

fn main() {
    human_panic::setup_panic!();
    librellog::init_interner();

    println!("Rellog REPL");

    let mut repl = repl::Repl::loading_std_lib();

    if let Some(fname) = std::env::args().nth(1) {
        let mut tok_buf = Vec::new();
        match repl.load_file(&mut tok_buf, fname.clone()) {
            Ok(()) => {}
            Err(e) => {
                println!("Could not load file {fname}.");
                println!("Error: {e}");
            }
        }
        println!("Loaded `{fname}`.");
    } else {
        println!("Running in interactive mode.");
        println!("Type `:help` for a list of commands.");
        println!("Use `:load` to load a source file.");
    }

    repl.run()
}
