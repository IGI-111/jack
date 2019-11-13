mod gen;
mod ir;
mod parser;

use gen::gen;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::collections::HashSet;
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

    let (rem, functions) = match parser::program(&text) {
        Ok(res) => res,
        Err(e) => panic!(format!("{:?}", e)),
    };
    if rem != "" {
        panic!(format!(
            "Cant completely parse program. Remaining: {:?}",
            rem
        ))
    }

    let mut uniq = HashSet::new();
    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }

    let ctx =
        ir::sem::SemContext::from_funs(functions.iter().map(|f| (f.name.clone(), f.ty.clone())));
    let typed_functions = functions
        .into_iter()
        .map(|func| ir::sem::SemFunction::analyze(func, &ctx))
        .collect::<Vec<_>>();

    let module = gen(&typed_functions);
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    let function = module.get_function("main").unwrap();
    let res = unsafe { ee.run_function_as_main(&function, &[]) };
    println!("{:?}", res);
}
