#![deny(unused_must_use)]

mod app_err;
mod line_editor_config;
mod repl;

fn main() -> ! {
    librellog::init_interner();

    println!("Rellog REPL");

    if let Some(fname) = std::env::args().nth(1) {
        let mut tok_buf = Vec::new();
        let mut r = repl::Repl::loading_std_lib();
        match r.load_file(&mut tok_buf, fname.clone()) {
            Ok(()) => {}
            Err(e) => {
                println!("Could not load file {fname}.");
                println!("Error: {e}");
            }
        }
        println!("Loaded `{fname}`.");
        r.run();
    } else {
        println!("Running in interactive mode.");
        println!("Type `:help` for a list of commands.");
        println!("Use `:load` to load a source file.");
        repl::Repl::loading_std_lib().run()
    }
}
