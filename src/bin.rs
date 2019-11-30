use jack_lang::error::Result;
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

    let res = jack_lang::compile_and_run(&text)?;
    println!("{:?}", res);
    Ok(())
}
