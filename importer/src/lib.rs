mod module_loader;
mod path_canonicalizer;
mod powdr_std;

use std::path::PathBuf;

use ast::parsed::asm::ASMProgram;
pub use module_loader::load_module_files;
use number::FieldElement;
use parser::parse_asm;
use path_canonicalizer::canonicalize_paths;
use powdr_std::add_std;

pub fn resolve<T: FieldElement>(
    path: Option<PathBuf>,
    module: ASMProgram<T>,
) -> Result<ASMProgram<T>, String> {
    load_module_files(path, module)
        .and_then(add_std)
        .and_then(canonicalize_paths)
}

/// A test utility to process a source file until after import resolution
pub fn resolve_str<T: FieldElement>(source: &str) -> ASMProgram<T> {
    resolve(None, parse_asm(None, source).unwrap()).unwrap()
}
