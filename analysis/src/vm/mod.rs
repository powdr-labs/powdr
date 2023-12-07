//! Analysis for VM machines, reducing them to constrained machines
//! Machines which do not have a pc should be left unchanged by this

use ast::{asm_analysis::AnalysisASMFile, DiffMonitor};
use number::FieldElement;

pub mod batcher;
pub mod inference;

pub(crate) fn analyze<T: FieldElement>(
    file: AnalysisASMFile<T>,
    monitor: &mut DiffMonitor,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    // infer assignment registers
    log::debug!("Run inference analysis step");
    let file = inference::infer(file)?;
    monitor.push(&file);
    // batch statements in each function
    log::debug!("Run batch analysis step");
    let file = batcher::batch(file);
    monitor.push(&file);

    Ok(file)
}

#[cfg(test)]
mod test_utils {
    use crate::test_util::typecheck_str;

    use super::*;

    /// A test utility to process a source file until after inference
    pub fn infer_str<T: FieldElement>(source: &str) -> Result<AnalysisASMFile<T>, Vec<String>> {
        inference::infer(typecheck_str(source).unwrap())
    }

    /// A test utility to process a source file until after batching
    pub fn batch_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        batcher::batch(infer_str(source).unwrap())
    }

    /// A test utility to process a source file until after asm to pil reduction
    #[allow(dead_code)]
    pub fn asm_to_pil_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        asm_to_pil::compile(batch_str(source))
    }
}
