extern crate nom;

mod ast;
mod eval;
mod parser;

use eval::run;
use parser::parser;
use std::env;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    for arg in env::args().skip(1) {
        let mut file = File::open(arg.clone()).expect(&format!("Can't open file: {}", arg));
        let mut text = String::new();
        file.read_to_string(&mut text)
            .expect(&format!("Can't read file: {}", arg));
        let (_, module) = match parser(&text) {
            Ok(res) => res,
            Err(e) => {
                eprintln!("{:?}", e);
                panic!();
            }
        };
        println!("{:?}", run(&module));
    }
}
