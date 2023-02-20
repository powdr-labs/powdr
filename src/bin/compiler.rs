use std::{env, fs, path::Path};

fn main() {
    if env::args().nth(1).unwrap() == "--asm" {
        let file_name = env::args().nth(2).unwrap();
        let contents = fs::read_to_string(Path::new(&file_name)).unwrap();
        match powdr::parser::parse_asm(Some(&file_name), &contents) {
            Ok(ast) => println!("{ast:?}"),
            Err(err) => err.output_to_stderr(),
        }
    } else {
        powdr::compiler::compile(Path::new(&env::args().nth(1).unwrap()));
    }
}
