//! Analysis for VM machines, reducing them to constrained machines
//! Machines which do not have a pc should be left unchanged by this

use powdr_ast::asm_analysis::AnalysisASMFile;
use powdr_number::FieldElement;

pub mod batcher;
pub mod inference;

pub(crate) fn analyze<T: FieldElement>(
    file: AnalysisASMFile<T>,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    // infer assignment registers
    log::debug!("Run inference analysis step");
    let file = inference::infer(file)?;
    // batch statements in each function
    log::debug!("Run batch analysis step");
    let file = batcher::batch(file);

    Ok(file)
}

#[cfg(test)]
mod test_utils {
    use super::*;

    /// A test utility to process a source file until after inference
    pub fn infer_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        let machines =
            crate::machine_check::check(powdr_importer::load_dependencies_and_resolve_str(source))
                .unwrap();
        inference::infer(machines)
    }

    /// A test utility to process a source file until after batching
    pub fn batch_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        batcher::batch(infer_str(source).unwrap())
    }
}
