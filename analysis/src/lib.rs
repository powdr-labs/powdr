mod batcher;
mod macro_expansion;
mod type_check;

/// expose the macro expander for use in the pil_analyzer
pub use macro_expansion::MacroExpander;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

pub fn analyze<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let expanded = macro_expansion::expand(file);
    let checked = type_check::check(expanded)?;
    let batched = batcher::batch(checked);
    Ok(batched)
}
