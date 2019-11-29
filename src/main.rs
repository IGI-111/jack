mod error;
mod gen;
mod ir;
mod parser;

use crate::error::{CompilerError, Result};
use crate::ir::raw::RawFunction;
use gen::gen;
use inkwell::OptimizationLevel;
use nom::error::convert_error;
use std::collections::HashSet;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;

fn main() {
    if let Err(err) = try_main() {
        eprintln!("{}", err);
        std::process::exit(1);
    }
}

fn try_main() -> Result<()> {
    let arg = std::env::args()
        .skip(1)
        .next()
        .expect("You must specify an input file");
    let path = Path::new(&arg);
    let mut file = File::open(path.clone()).expect(&format!("Can't open file: {}", path.display()));
    let mut text = String::new();
    file.read_to_string(&mut text)
        .expect(&format!("Can't read file: {}", path.display()));

    let functions = parse(&text)?;

    let mut uniq = HashSet::new();
    if !functions.iter().all(|x| uniq.insert(x.name.clone())) {
        panic!("Duplicate function name");
    }

    let ctx =
        ir::sem::SemContext::from_funs(functions.iter().map(|f| (f.name.clone(), f.ty.clone())));
    let mut typed_functions = Vec::new();
    for func in functions.into_iter() {
        typed_functions.push(ir::sem::SemFunction::analyze(func, &ctx)?);
    }

    let module = gen(&typed_functions);
    let ee = module
        .create_jit_execution_engine(OptimizationLevel::Default)
        .unwrap();

    let function = module.get_function("main").expect("No main function");
    let res = unsafe { ee.run_function_as_main(&function, &[]) };
    println!("{:?}", res);
    Ok(())
}

fn parse(text: &str) -> Result<Vec<RawFunction>> {
    let (_, text) = match parser::comments_ommited(&text) {
        Ok(res) => res,
        Err(nom::Err::Incomplete(n)) => {
            return Err(CompilerError::Syntax(format!(
                "Undertermined syntax. More input needed to be sure: {:?}",
                n
            ))
            .into());
        }
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            return Err(CompilerError::Syntax(convert_error(&text, e)).into());
        }
    };

    let (_, functions) = match parser::program(&text) {
        Ok(res) => res,
        Err(nom::Err::Incomplete(n)) => {
            return Err(CompilerError::Syntax(format!(
                "Undertermined syntax. More input needed to be sure: {:?}",
                n
            ))
            .into());
        }
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            return Err(CompilerError::Syntax(convert_error(&text, e)).into());
        }
    };
    Ok(functions)
}
