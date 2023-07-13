use lalrpop_util::*;

use crate::{
    compiler::{FunctionKind, Register},
    Statement,
};
use parser_util::handle_parse_error;

lalrpop_mod!(
    #[allow(clippy::all)]
    riscv_asm,
    "/riscv_asm.rs"
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

impl asm_utils::parser::Parser<Register, FunctionKind> for RiscParser {
    fn parse(&self, input: &str) -> Result<Vec<Statement>, String> {
        self.parser.parse(input).map_err(|err| {
            handle_parse_error(err, None, input).output_to_stderr();
            panic!("RISCV assembly parse error");
        })
    }
}
