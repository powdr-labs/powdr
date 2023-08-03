//! Analysis for VM machines, reducing them to PIL
//! Machines which do not have a pc should be left unchanged by this

use ast::asm_analysis::AnalysisASMFile;
use number::FieldElement;

use crate::DiffMonitor;

/// Batch compatible ROM statements
pub mod batcher;
/// Desugar asm functions using registers and witness columns
pub mod function_desugar;
/// Infer assignment registers in asm statements
pub mod inference;
/// Generate one ROM per machine from all declared functions
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
