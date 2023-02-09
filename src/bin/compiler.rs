use std::{env, path::Path};

fn main() {
    powdr::compiler::compile(Path::new(&env::args().nth(1).unwrap()));
}
