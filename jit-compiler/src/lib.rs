mod codegen;
mod compiler;

use std::collections::HashMap;

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
    let code = create_full_code(analyzed, symbols)?;

    let (dir, lib_path) = call_cargo(&code);

    let result = load_library(&lib_path, symbols);

    drop(dir);
    result
}
