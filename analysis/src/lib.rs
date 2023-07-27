mod batcher;
mod inference;
mod macro_expansion;
mod romgen;
mod type_check;

/// expose the macro expander for use in the pil_analyzer
pub use macro_expansion::MacroExpander;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
use number::FieldElement;

pub fn analyze<T: FieldElement>(file: ASMFile<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let expanded = macro_expansion::expand(file);
    let checked = type_check::check(expanded)?;
    let inferred = inference::infer(checked)?;
    let rommed = romgen::generate_rom(inferred);
    let batched = batcher::batch(rommed);
    Ok(batched)
}

#[cfg(test)]
mod test_util {
    use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMFile};
    use number::FieldElement;
    use parser::parse_asm;

    use crate::{inference, macro_expansion, type_check};

    /// A test utility to process a source file until after macro expansion
    pub fn expand_str<T: FieldElement>(source: &str) -> ASMFile<T> {
        let file = parse_asm(None, source).unwrap();
        macro_expansion::expand(file)
    }

    /// A test utility to process a source file until after type checking
    pub fn check_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        type_check::check(expand_str(source))
    }

    /// A test utility to process a source file until after inference
    pub fn infer_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        inference::infer(check_str(source).unwrap())
    }
}
