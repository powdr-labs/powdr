/// Values which are common to many steps from asm to PIL
use number::FieldElement;

use crate::utils::parse_instruction;

/// The name for the `return` keyword in the PIL constraints
pub const RETURN_NAME: &str = "return";
/// The name for the `reset` instruction in the PIL constraints
pub const RESET_NAME: &str = "_reset";

pub fn instruction_flag(name: &str) -> String {
    format!("instr_{name}")
}

/// The names of the output assignment registers for `count` outputs. All `return` statements assign to these.
fn output_registers(count: usize) -> Vec<String> {
    (0..count).map(output_at).collect()
}

/// The name of the read-only registers at index `i`
pub fn input_at(i: usize) -> String {
    format!("_input_{}", i)
}

/// The name of the output assignment registers at index `i`
pub fn output_at(i: usize) -> String {
    format!("_output_{}", i)
}

/// The return instruction for `output_count` outputs and `pc_name` the name of the pc
pub fn return_instruction<T: FieldElement>(
    output_count: usize,
    pc_name: &str,
) -> ast::asm_analysis::Instruction<T> {
    parse_instruction(&format!(
        "{} {{ {pc_name}' = 0 }}",
        output_registers(output_count).join(", ")
    ))
}
