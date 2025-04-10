use std::path::Path;

use crate::loader::Program;

const WASM_ASM_TEMPLATE: &str = include_str!("wasm.asm.template");

pub fn generate_code(output_file: &Path, program: &Program) {
    todo!()
}
