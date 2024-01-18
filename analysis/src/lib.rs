#![deny(clippy::print_stdout)]

mod block_enforcer;
pub mod machine_check;
mod vm;

use ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMProgram, DiffMonitor};
use number::FieldElement;

pub fn convert_asm_to_pil<T: FieldElement>(
    file: ASMProgram<T>,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let mut monitor = DiffMonitor::default();
    let file = analyze(file, &mut monitor)?;
    Ok(convert_vms_to_constrained(file, &mut monitor))
}

pub fn analyze<T: FieldElement>(
    file: ASMProgram<T>,
    monitor: &mut DiffMonitor,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    log::debug!("Run machine check analysis step");
    let file = machine_check::check(file)?;
    monitor.push(&file);

    // run analysis on virtual machines, batching instructions
    log::debug!("Start asm analysis");
    let file = vm::analyze(file, monitor)?;
    log::debug!("End asm analysis");

    Ok(file)
}

/// Converts all VMs to constrained machines, replacing
/// assembly instructions by lookups to programs.
pub fn convert_vms_to_constrained<T: FieldElement>(
    file: AnalysisASMFile<T>,
    monitor: &mut DiffMonitor,
) -> AnalysisASMFile<T> {
    // remove all asm (except external instructions)
    log::debug!("Run asm_to_pil");
    let file = asm_to_pil::compile(file);
    monitor.push(&file);

    // enforce blocks using `operation_id` and `latch`
    log::debug!("Run enforce_block analysis step");
    let file = block_enforcer::enforce(file);
    monitor.push(&file);

    file
}

pub mod utils {
    use ast::parsed::PilStatement;
    use number::FieldElement;

    pub fn parse_pil_statement<T: FieldElement>(input: &str) -> PilStatement<T> {
        let ctx = parser::ParserContext::new(None, input);
        parser::powdr::PilStatementParser::new()
            .parse(&ctx, input)
            .unwrap()
    }
}
