use compiler::{
    compile_asm_string, verify, write_commits_to_fs, write_constants_to_fs,
    write_constraints_to_fs, BackendType,
};
use number::GoldilocksField;

/// Like compiler::verify::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string(file_name: &str, contents: &str, inputs: Vec<GoldilocksField>) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();
    let (_, result) = compile_asm_string(
        file_name,
        contents,
        inputs.clone(),
        Some(&mut |analyzed| {
            riscv_executor::execute_ast(analyzed, &inputs.clone(), usize::MAX, None);
        }),
        &temp_dir,
        true,
        Some(BackendType::PilStarkCli),
        vec![],
    )
    .unwrap();

    let result = result.unwrap();
    write_constants_to_fs(&result.constants, &temp_dir);
    write_commits_to_fs(&result.witness.unwrap(), &temp_dir);
    write_constraints_to_fs(&result.constraints_serialization.unwrap(), &temp_dir);

    verify(&temp_dir);
}
