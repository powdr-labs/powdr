use compiler::{pipeline::Pipeline, test_util::verify_pipeline};
use number::GoldilocksField;
use riscv::bootloader::default_input;
use std::{collections::HashMap, path::PathBuf};

/// Like compiler::test_util::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string(file_name: &str, contents: &str, inputs: Vec<GoldilocksField>) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let mut inputs_hash: HashMap<GoldilocksField, Vec<GoldilocksField>> = HashMap::default();
    inputs_hash.insert(0u32.into(), inputs.clone());

    let mut pipeline = Pipeline::default()
        .with_output(temp_dir.to_path_buf(), false)
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)));
    let analyzed = pipeline.analyzed_asm_ref().unwrap();
    riscv_executor::execute_ast(
        analyzed,
        &inputs_hash.clone(),
        &default_input(),
        usize::MAX,
        riscv_executor::ExecMode::Fast,
    );
    verify_pipeline(pipeline, inputs, vec![]);
}
