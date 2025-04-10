mod code_gen;
mod loader;

use std::path::Path;

fn main() -> wasmparser::Result<()> {
    env_logger::init();

    // TODO: do proper command line argument parsing
    let args: Vec<String> = std::env::args().collect();
    let wasm_file = std::fs::read(&args[1]).unwrap();

    let program = loader::load_wasm(&wasm_file)?;

    code_gen::generate_code(Path::new(&args[2]), &program);

    Ok(())
}
