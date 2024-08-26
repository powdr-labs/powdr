#![deny(clippy::print_stdout)]

pub mod machine_check;
mod vm;

use powdr_ast::{asm_analysis::AnalysisASMFile, parsed::asm::unique::ASMProgram};
use powdr_number::FieldElement;

pub fn convert_asm_to_pil<T: FieldElement>(
    file: ASMProgram,
) -> Result<AnalysisASMFile, Vec<String>> {
    let file = analyze(file)?;
    Ok(powdr_asm_to_pil::compile::<T>(file))
}

pub fn analyze(file: ASMProgram) -> Result<AnalysisASMFile, Vec<String>> {
    log::debug!("Run machine check analysis step");
    let file = machine_check::check(file)?;

    // run analysis on virtual machines, batching instructions
    log::debug!("Start asm analysis");
    let file = vm::analyze(file)?;
    log::debug!("End asm analysis");

    Ok(file)
}

pub mod utils {
    use powdr_ast::parsed::PilStatement;
    use powdr_parser::powdr;

    lazy_static::lazy_static! {
        static ref PIL_STATEMENT_PARSER: powdr::PilStatementParser = powdr::PilStatementParser::new();
    }

    pub fn parse_pil_statement(input: &str) -> PilStatement {
        let ctx = powdr_parser::ParserContext::new(None, input);
        PIL_STATEMENT_PARSER.parse(&ctx, input).unwrap()
    }
}
