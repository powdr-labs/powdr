use std::env;

fn main() {
    powdr::compiler::compile(&env::args().nth(1).unwrap());
}
