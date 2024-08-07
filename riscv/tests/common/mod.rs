use powdr_backend::BackendType;
use powdr_number::GoldilocksField;
use powdr_pipeline::{test_util::verify_pipeline, Pipeline};
use std::path::PathBuf;

/// Like compiler::test_util::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string<S: serde::Serialize + Send + Sync + 'static>(
    file_name: &str,
    contents: &str,
    inputs: Vec<GoldilocksField>,
    data: Option<Vec<(u32, S)>>,
    backend: BackendType,
) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let mut pipeline = Pipeline::default()
        .with_prover_inputs(inputs.clone())
        .with_output(temp_dir.to_path_buf(), false)
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)));

    if let Some(data) = data {
        pipeline = pipeline.add_data_vec(&data);
    }

    let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
    powdr_riscv_executor::execute_ast(
        &analyzed,
        Default::default(),
        pipeline.data_callback().unwrap(),
        // Assume the RISC-V program was compiled without a bootloader, otherwise this will fail.
        &[],
        usize::MAX,
        powdr_riscv_executor::ExecMode::Fast,
        Default::default(),
    );
    verify_pipeline(pipeline, backend).unwrap();
}
