extern crate nom;

mod ast;
mod eval;
mod parser;

use ast::Module;
use eval::run;
use std::collections::{HashMap, VecDeque};
use std::env;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    let arg = env::args()
        .skip(1)
        .next()
        .expect("You must specify an input file");
    let arg_path = Path::new(&arg);

    let main_module_name = arg_path.file_stem().unwrap().to_str().unwrap().to_string();

    let cwd = arg_path.parent().unwrap();
    let mut modules: HashMap<String, Module> = HashMap::new();

    let mut load_queue = VecDeque::new();
    load_queue.push_back(main_module_name.clone());

    while !load_queue.is_empty() {
        let module_name = load_queue.pop_front().unwrap();
        if !modules.contains_key(&module_name) {
            let module = load_module(cwd, &module_name);

            for import in module.imports.iter() {
                load_queue.push_back(import.module.clone());
            }

            modules.insert(module_name, module);
        }
    }

    println!("{:?}", run(&main_module_name, &modules));
}

pub fn load_module(cwd: &Path, module_name: &str) -> Module {
    let path = Path::new(&cwd).join(format!("{}.jack", module_name));
    let mut file = File::open(path.clone()).expect(&format!("Can't open file: {}", path.display()));
    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect(&format!("Can't read file: {}", path.display()));

    let (_, module) = match parser::module(&text, module_name.to_string()) {
        Ok(res) => res,
        Err(e) => panic!(format!("{:?}", e)),
    };
    module
}
