use std::env;
use std::fs;

use powdr::parser;

fn main() {
    let input = fs::read_to_string(env::args().nth(1).unwrap()).unwrap();
    match parser::parse(&input) {
        Ok(result) => println!("{result:?}"),
        Err(err) => println!("Parse error: {err}"),
    }
}
