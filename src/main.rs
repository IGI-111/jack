extern crate nom;

mod ast;
mod parser;

use parser::parser;

fn main() {
    let txt = r##"
import a
import b
import c

export if if ( -10 + 1 >= 3) != false { true } else { false } {
    78 + 1
    } else {
        if true { 3 }
    }
"##;
    println!("{:#?}", parser(txt));
}
