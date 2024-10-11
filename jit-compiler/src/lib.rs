mod codegen;
mod compiler;

use std::{
    collections::{HashMap, HashSet},
    fs,
    sync::Arc,
};

use codegen::CodeGenerator;
use compiler::{call_cargo, generate_glue_code, load_library};

use itertools::Itertools;
use powdr_ast::analyzed::Analyzed;
use powdr_number::FieldElement;

/// Wrapper around a dynamically loaded function.
/// Prevents the dynamically loaded library to be unloaded while the function is still in use.
#[derive(Clone)]
pub struct LoadedFunction {
    #[allow(dead_code)]
    library: Arc<libloading::Library>,
    function: extern "C" fn(u64) -> u64,
}

impl LoadedFunction {
    pub fn call(&self, arg: u64) -> u64 {
        (self.function)(arg)
    }
}

/// Compiles the given symbols (and their dependencies) and returns them as a map
/// from symbol name to function.
/// Only functions of type (int -> int) are supported for now.
pub fn compile<T: FieldElement>(
    analyzed: &Analyzed<T>,
    requested_symbols: &[&str],
) -> Result<HashMap<String, LoadedFunction>, String> {
    log::info!("JIT-compiling {} symbols...", requested_symbols.len());

    let mut codegen = CodeGenerator::new(analyzed);
    let successful_symbols = requested_symbols
        .iter()
        .filter_map(|&sym| match codegen.request_symbol(sym, &[]) {
            Err(e) => {
                log::debug!("Unable to generate code for symbol {sym}: {e}");
                None
            }
            Ok(access) => Some((sym, access)),
        })
        .collect::<Vec<_>>();
    let successful_symbol_names: Vec<_> = successful_symbols.iter().map(|(s, _)| *s).collect();

    if successful_symbols.len() < requested_symbols.len() {
        let successful_hash = successful_symbol_names.iter().collect::<HashSet<_>>();
        log::info!(
            "Unable to JIT-compile the following symbols. Will use evaluator instead.\n{}",
            requested_symbols
                .iter()
                .filter(|&sym| !successful_hash.contains(sym))
                .format(", ")
        );
    }

    if successful_symbols.is_empty() {
        return Ok(Default::default());
    };

    let glue_code = generate_glue_code(&successful_symbols, analyzed)?;

    let lib_file = call_cargo(&format!("{glue_code}\n{}\n", codegen.generated_code()))?;
    let metadata = fs::metadata(&lib_file.path).unwrap();

    log::info!(
        "Loading library of size {} MB...",
        metadata.len() as f64 / (1024.0 * 1024.0)
    );

    let result = load_library(&lib_file.path, &successful_symbol_names);
    log::info!("Done.");
    result
}
