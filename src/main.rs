extern crate inkwell;
extern crate nom;

mod ast;
mod gen;
mod parser;
mod types;

use gen::generate;
use inkwell::OptimizationLevel;
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

    let (rem, ast) = match parser::expression(&text) {
        Ok(res) => res,
        Err(e) => panic!(format!("{:?}", e)),
    };
    if rem != "\n" {
        panic!(format!(
            "Cant completely parse program. Remaining: {:?}",
            rem
        ))
    }

    let typed_ast = types::TypedNode::infer_types(ast);

    let module = generate(&typed_ast);
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();
    unsafe {
        let res = ee.run_function_as_main(&module.get_function("main").unwrap(), &[]);
        println!("{}", res);
    }
}
