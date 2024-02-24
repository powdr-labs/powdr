#![deny(clippy::print_stdout)]

mod block_enforcer;
pub mod machine_check;
mod vm;

use powdr_ast::{asm_analysis::AnalysisASMFile, parsed::asm::ASMProgram};
use powdr_number::FieldElement;

pub fn convert_asm_to_pil<T: FieldElement>(
    file: ASMProgram<T>,
) -> Result<AnalysisASMFile<T>, Vec<String>> {
    let file = analyze(file)?;
    Ok(convert_vms_to_constrained(file))
}

pub fn analyze<T: FieldElement>(file: ASMProgram<T>) -> Result<AnalysisASMFile<T>, Vec<String>> {
    log::debug!("Run machine check analysis step");
    let file = machine_check::check(file)?;

    // run analysis on virtual machines, batching instructions
    log::debug!("Start asm analysis");
    let file = vm::analyze(file)?;
    log::debug!("End asm analysis");

    Ok(file)
}

/// Converts all VMs to constrained machines, replacing
/// assembly instructions by lookups to programs.
pub fn convert_vms_to_constrained<T: FieldElement>(file: AnalysisASMFile<T>) -> AnalysisASMFile<T> {
    // remove all asm (except external instructions)
    log::debug!("Run asm_to_pil");
    let file = powdr_asm_to_pil::compile(file);

    // enforce blocks using `operation_id` and `latch`
    log::debug!("Run enforce_block analysis step");
    block_enforcer::enforce(file)
}

pub mod utils {
    use powdr_ast::parsed::PilStatement;
    use powdr_number::FieldElement;
    use powdr_parser::powdr;

    lazy_static::lazy_static! {
        static ref PIL_STATEMENT_PARSER: powdr::PilStatementParser = powdr::PilStatementParser::new();
    }

    pub fn parse_pil_statement<T: FieldElement>(input: &str) -> PilStatement<T> {
        let ctx = powdr_parser::ParserContext::new(None, input);
        PIL_STATEMENT_PARSER.parse(&ctx, input).unwrap()
    }
}
