use lalrpop_util::*;

use crate::code_gen::{FunctionKind, Register};
use powdr_parser_util::handle_parse_error;

use super::Statement;

lalrpop_mod!(
    #[allow(clippy::all)]
    #[allow(clippy::uninlined_format_args)]
    riscv_asm,
    "/asm/riscv_asm.rs"
);

pub struct RiscParser {
    parser: riscv_asm::StatementsParser,
}

impl Default for RiscParser {
    fn default() -> Self {
        Self {
            parser: riscv_asm::StatementsParser::new(),
        }
    }
}

impl powdr_asm_utils::parser::Parser<Register, FunctionKind> for RiscParser {
    fn parse(&self, input: &str) -> Result<Vec<Statement>, String> {
        self.parser.parse(input).map_err(|err| {
            handle_parse_error(err, None, input).output_to_stderr();
            panic!("RISCV assembly parse error");
        })
    }
}
