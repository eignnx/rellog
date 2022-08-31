#![deny(unused_must_use)]

mod app_err;
mod repl;

fn main() {
    librellog::init_interner();

    let fname = std::env::args().nth(1).unwrap_or_else(|| {
        let exe_name = std::env::args().next().unwrap();
        eprintln!("USAGE:");
        eprintln!("\t{exe_name} FILENAME");
        eprintln!();
        eprintln!("Please provide a filename. (Exiting...)");
        std::process::exit(1);
    });

    repl::Repl::loading_file(&fname).run();
}
