use crate::error::{CompilerError, Result};
use crate::ir::raw::RawFunction;
use gen::Jit;
use nom::error::convert_error;
use std::collections::HashSet;
use std::mem;

pub mod error;
mod gen;
mod ir;
mod parser;

pub fn compile_and_run(program: &str) -> Result<i32> {
    let functions = parse(program)?;

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

    let mut jit = Jit::new();
    let code_ptr = jit.compile(&typed_functions)?;
    unsafe {
        let code_fn = mem::transmute::<_, fn() -> i32>(code_ptr);
        Ok(code_fn())
    }
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
