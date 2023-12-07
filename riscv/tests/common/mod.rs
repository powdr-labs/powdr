use compiler::{
    compile_asm_string, verify, write_commits_to_fs, write_constants_to_fs,
    write_constraints_to_fs, BackendType,
};
use number::GoldilocksField;
use riscv::bootloader::default_input;
use std::collections::HashMap;

/// Like compiler::verify::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string(file_name: &str, contents: &str, inputs: Vec<GoldilocksField>) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let mut inputs_hash: HashMap<GoldilocksField, Vec<GoldilocksField>> = HashMap::default();
    inputs_hash.insert(0u32.into(), inputs.clone());

    let (_, result) = compile_asm_string(
        file_name,
        contents,
        inputs.clone(),
        Some(&mut |analyzed| {
            riscv_executor::execute_ast(
                analyzed,
                &inputs_hash.clone(),
                &default_input(),
                usize::MAX,
            );
        }),
        &temp_dir,
        true,
        Some(BackendType::PilStarkCli),
        vec![],
        None,
    )
    .unwrap();

    let result = result.unwrap();
    write_constants_to_fs(&result.constants, &temp_dir);
    write_commits_to_fs(&result.witness.unwrap(), &temp_dir);
    write_constraints_to_fs(&result.constraints_serialization.unwrap(), &temp_dir);

    verify(&temp_dir);
}
