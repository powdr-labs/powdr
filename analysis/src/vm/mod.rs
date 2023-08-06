//! Analysis for VM machines, reducing them to PIL
//! Machines which do not have a pc should be left unchanged by this

use ast::asm_analysis::AnalysisASMFile;
use number::FieldElement;

use crate::DiffMonitor;

pub mod batcher;
pub mod function_desugar;
pub mod inference;
pub mod romgen;

pub fn analyze<T: FieldElement>(
    file: AnalysisASMFile<T>,
    monitor: &mut DiffMonitor,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    // infer assignment registers
    log::debug!("Run inference analysis step");
    let file = inference::infer(file)?;
    monitor.push(&file);
    // desugar functions
    log::debug!("Run function desugar analysis step");
    let file = function_desugar::desugar(file);
    monitor.push(&file);
    // batch statements in each function
    log::debug!("Run batch analysis step");
    let file = batcher::batch(file);
    monitor.push(&file);
    // generate the rom using a dispatcher
    log::debug!("Run generate_rom analysis step");
    let file = romgen::generate_rom(file);
    monitor.push(&file);
    // turn asm into pil
    log::debug!("Run asm_to_pil analysis step");
    let file = asm_to_pil::compile(file);
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

    /// A test utility to process a source file until after function desugar
    pub fn function_desugar_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        function_desugar::desugar(infer_str(source).unwrap())
    }

    /// A test utility to process a source file until after batching
    pub fn batch_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        batcher::batch(function_desugar_str(source))
    }

    /// A test utility to process a source file until after batching
    pub fn generate_rom_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        romgen::generate_rom(batch_str(source))
    }

    /// A test utility to process a source file until after asm to pil reduction
    #[allow(dead_code)]
    pub fn asm_to_pil_str<T: FieldElement>(source: &str) -> AnalysisASMFile<T> {
        asm_to_pil::compile(generate_rom_str(source))
    }
}
