use compiler::{compile_asm_string, verify, BackendType};
use number::GoldilocksField;

/// Like compiler::verify::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string(file_name: &str, contents: &str, inputs: Vec<GoldilocksField>) {
    let temp_dir = mktemp::Temp::new_dir().unwrap();
    compile_asm_string(
        file_name,
        contents,
        &inputs,
        Some(&mut |analyzed| {
            riscv_executor::execute_ast(analyzed, &inputs);
        }),
        &temp_dir,
        true,
        Some(BackendType::PilStarkCli),
        vec![],
    )
    .unwrap();
    verify(&temp_dir);
}
