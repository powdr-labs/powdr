use powdr_backend::BackendType;
use powdr_number::GoldilocksField;
use powdr_pipeline::{test_util::verify_pipeline, Pipeline};
use std::path::PathBuf;

/// Like compiler::test_util::verify_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string<S: serde::Serialize + Send + Sync + 'static>(
    file_name: &str,
    contents: &str,
    inputs: &[GoldilocksField],
    data: Option<&[(u32, S)]>,
    backend: BackendType,
) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let mut pipeline = Pipeline::default()
        .with_prover_inputs(inputs.to_vec())
        .with_output(temp_dir.to_path_buf(), false)
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)));

    if let Some(data) = data {
        pipeline = pipeline.add_data_vec(data);
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

pub fn verify_riscv_asm_file(case: &Path, runtime: &Runtime, use_pie: bool) {
    let tmp_dir = Temp::new_dir().unwrap();
    let executable = tmp_dir.join("executable");

    // Assemble the file using clang, because it is the most likely thing to be already installed.
    // i.e: clang --target=riscv32-unknown-elf -march=rv32imac -mabi=ilp32 -nostdlib -static -o {executable} {case} [-Wl,-pie | -Wl,--emit-relocs]
    Command::new("clang")
        .arg("--target=riscv32-unknown-elf")
        .arg("-march=rv32imac")
        .arg("-mabi=ilp32")
        .arg("-nostdlib")
        .arg("-static")
        .arg(if use_pie {
            "-Wl,-pie"
        } else {
            "-Wl,--emit-relocs"
        })
        .arg("-o")
        .arg(executable)
        .arg(case)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let powdr_asm = powdr_riscv::elf::elf_translate::<GoldilocksField>(&executable, runtime, false);
    verify_riscv_asm_string(
        &format!("{case}.asm"),
        &powdr_asm,
        &[],
        None,
        BackendType::EStarkDump,
    );
}
