use std::path::Path;

use wasmparser::FuncToValidate;

use crate::loader::Program;

const WASM_ASM_TEMPLATE: &str = include_str!("wasm.asm.template");

pub fn generate_code(output_file: &Path, program: &Program) {
    for func in program.functions.iter() {
        for directive in func.iter() {
            println!("{directive:?}");
        }
    }

    todo!()
}
