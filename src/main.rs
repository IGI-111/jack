extern crate inkwell;
extern crate nom;

mod ast;
mod eval;
mod parser;

use eval::generate;
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let arg = env::args()
        .skip(1)
        .next()
        .expect("You must specify an input file");
    let path = Path::new(&arg);
    let mut file = File::open(path.clone()).expect(&format!("Can't open file: {}", path.display()));
    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect(&format!("Can't read file: {}", path.display()));

    let (_, expr) = match parser::expression(&text) {
        Ok(res) => res,
        Err(e) => panic!(format!("{:?}", e)),
    };

    generate(&expr);
}
