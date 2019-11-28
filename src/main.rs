extern crate argparse;
#[macro_use]
extern crate lalrpop_util;

mod ast;

use std::fs::File;
use std::io::prelude::*;

use argparse::{ArgumentParser, Print, Store};

lalrpop_mod!(
    // Disable clippy on generated parser.
    #[allow(clippy::all)]
    rust_grammar
);

#[cfg(test)]
mod tests;

fn main() {
    // Parse args.
    let mut script_filename = "".to_string();
    {
        let mut ap = ArgumentParser::new();
        ap.set_description("Mini-Rust interpreter");
        ap.add_option(
            &["--version"],
            Print(env!("CARGO_PKG_VERSION").to_string()),
            "Show version",
        );
        ap.refer(&mut script_filename)
            .add_argument("script_filename", Store, "Rust file to interpret");
        ap.parse_args_or_exit();
    }

    // Read the file.
    let mut file = File::open(&script_filename)
        .unwrap_or_else(|_| panic!("Source file not found: {}", script_filename));
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .unwrap_or_else(|_| panic!("Unable to read file: {}", script_filename));
    // Parse.
    let result = rust_grammar::ModuleParser::new().parse(&contents);
    match result {
        Ok(ast) => println!("{:#?}", ast),
        Err(e) => eprintln!("Parse error: {}", e),
    }
}
