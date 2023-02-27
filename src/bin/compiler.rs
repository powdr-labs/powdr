use std::{env, fs, path::Path};

use powdr::compiler::no_callback;

fn main() {
    if env::args().nth(1).unwrap() == "--asm" {
        let file_name = env::args().nth(2).unwrap();
        let contents = fs::read_to_string(Path::new(&file_name)).unwrap();
        match powdr::asm_compiler::compile(Some(&file_name), &contents) {
            Ok(pil) => println!("{pil}"),
            Err(err) => err.output_to_stderr(),
        }
    } else if env::args().nth(1).unwrap() == "--reformat" {
        let file_name = env::args().nth(2).unwrap();
        let contents = fs::read_to_string(Path::new(&file_name)).unwrap();
        match powdr::parser::parse(Some(&file_name), &contents) {
            Ok(ast) => println!("{ast}"),
            Err(err) => err.output_to_stderr(),
        }
    } else {
        powdr::compiler::compile_pil(Path::new(&env::args().nth(1).unwrap()), no_callback());
    }
}
