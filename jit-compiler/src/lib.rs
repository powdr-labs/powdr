mod codegen;
mod compiler;

use std::{collections::HashMap, fs};

use compiler::{call_cargo, create_full_code, load_library};
use powdr_ast::analyzed::Analyzed;
use powdr_number::FieldElement;

pub type SymbolMap = HashMap<String, fn(u64) -> u64>;

/// Compiles the given symbols (and their dependencies) and returns them as a map
/// from symbol name to function pointer.
/// Only functions of type (int -> int) are supported for now.
pub fn compile<T: FieldElement>(
    analyzed: &Analyzed<T>,
    symbols: &[&str],
) -> Result<SymbolMap, String> {
    log::info!("JIT-compiling {} symbols...", symbols.len());
    let code = create_full_code(analyzed, symbols)?;

    let (dir, lib_path) = call_cargo(&code);
    let metadata = fs::metadata(&lib_path).unwrap();

    log::info!(
        "Loading library with size {} MB...",
        metadata.len() as f64 / 1000000.0
    );

    let result = load_library(&lib_path, symbols);
    log::info!("Done.");

    drop(dir);
    result
}
