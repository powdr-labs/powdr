#![deny(clippy::print_stdout)]

mod module_loader;
mod path_canonicalizer;
mod powdr_std;

use std::path::PathBuf;

pub use module_loader::load_module_files;
use path_canonicalizer::canonicalize_paths;
use powdr_ast::parsed::asm::{non_unique, unique};
use powdr_parser::parse_asm;
use powdr_parser_util::{Error, SourceRef};
use powdr_std::add_std;

pub fn load_dependencies_and_resolve(
    path: Option<PathBuf>,
    module: non_unique::ASMProgram,
) -> Result<unique::ASMProgram, Error> {
    load_module_files(path, module)
        .and_then(add_std)
        .map_err(|e| SourceRef::default().with_error(e))
        .and_then(canonicalize_paths)
}

/// A test utility to process a source file until after import resolution
pub fn load_dependencies_and_resolve_str(source: &str) -> unique::ASMProgram {
    load_dependencies_and_resolve(None, parse_asm(None, source).unwrap()).unwrap()
}
