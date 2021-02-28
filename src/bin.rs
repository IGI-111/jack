use jack_lang::error::{CompilerError, Result};
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
        .nth(1)
        .ok_or_else(|| Box::new(CompilerError::NoInput))?;
    let path = Path::new(&arg);
    let mut file = File::open(path)
        .map_err(|_| Box::new(CompilerError::FileError(path.display().to_string())))?;
    let mut text = String::new();
    file.read_to_string(&mut text)
        .map_err(|_| Box::new(CompilerError::FileError(path.display().to_string())))?;

    let res = jack_lang::compile_and_run(&text)?;
    println!("{:?}", res);
    Ok(())
}
