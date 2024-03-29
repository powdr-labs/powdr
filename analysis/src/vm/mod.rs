//! Analysis for VM machines, reducing them to constrained machines
//! Machines which do not have a pc should be left unchanged by this

use powdr_ast::asm_analysis::AnalysisASMFile;

pub mod batcher;
pub mod inference;

pub(crate) fn analyze(file: AnalysisASMFile) -> Result<AnalysisASMFile, Vec<String>> {
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
    pub fn infer_str(source: &str) -> Result<AnalysisASMFile, Vec<String>> {
        let machines =
            crate::machine_check::check(powdr_importer::load_dependencies_and_resolve_str(source))
                .unwrap();
        inference::infer(machines)
    }

    /// A test utility to process a source file until after batching
    pub fn batch_str(source: &str) -> AnalysisASMFile {
        batcher::batch(infer_str(source).unwrap())
    }
}
