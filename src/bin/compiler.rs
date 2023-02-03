use std::{env, path::Path};

fn main() {
    let analyzed = powdr::analyzer::analyze(Path::new(&env::args().nth(1).unwrap()));
    let json_out = powdr::json_exporter::export(&analyzed);
    println!("{}", json_out.pretty(4));
}
