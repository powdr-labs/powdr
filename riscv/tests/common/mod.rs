use mktemp::Temp;
use powdr_backend::BackendType;
use powdr_number::GoldilocksField;
use powdr_pipeline::{test_util::verify_pipeline, Pipeline};
use powdr_riscv::Runtime;
use std::{
    path::{Path, PathBuf},
    process::Command,
};

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

fn find_assembler() -> &'static str {
    let options = ["riscv64-elf-as", "riscv64-unknown-elf-as"];
    for option in options.iter() {
        if Command::new(option).arg("--version").output().is_ok() {
            return option;
        }
    }
    panic!("No RISC-V assembler found");
}

pub fn verify_riscv_asm_file(asm_file: &Path, runtime: &Runtime, use_pie: bool) {
    let tmp_dir = Temp::new_dir().unwrap();
    let executable = tmp_dir.join("executable");
    let obj_file = tmp_dir.join("obj.o");

    // We have a bit of a conundrum here. GNU assembler is better, as it
    // supports 64-bit literals that some tests in the RISC-V testsuite use. But
    // LLVM linker is better as it always supports the -pie flag (which one of
    // the tests uses), and has a suitable default linker script, which unlike
    // GNU's, does not place the ELF header inside the text section.

    // So, our hacky solution is to assemble with GNU, and link with LLVM.

    // Assemble with GNU
    let assembler = find_assembler();
    log::info!("Using assembler: {}", assembler);
    Command::new(assembler)
        .arg("-march=rv32imac")
        .arg("-mabi=ilp32")
        .arg("-o")
        .arg(&obj_file)
        .arg(asm_file)
        .status()
        .unwrap();

    // Link with LLVM
    Command::new("ld.lld")
        .arg(if use_pie { "-pie" } else { "--emit-relocs" })
        .arg("-o")
        .arg(&executable)
        .arg(obj_file)
        .status()
        .unwrap();

    let case_name = asm_file.file_stem().unwrap().to_str().unwrap();

    let powdr_asm = powdr_riscv::elf::translate::<GoldilocksField>(&executable, runtime, false);
    verify_riscv_asm_string::<()>(
        &format!("{case_name}.asm"),
        &powdr_asm,
        &[],
        None,
        BackendType::EStarkDumpComposite,
    );
}
