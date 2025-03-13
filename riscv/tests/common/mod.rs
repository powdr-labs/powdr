use mktemp::Temp;
use powdr_backend::BackendType;
use powdr_number::{BabyBearField, FieldElement, GoldilocksField, KnownField, KoalaBearField};
use powdr_pipeline::{
    test_util::{
        run_pilcom_with_backend_variant, test_mock_backend, test_plonky3_pipeline, BackendVariant,
    },
    Pipeline,
};
use powdr_riscv::CompilerOptions;
use std::{
    path::{Path, PathBuf},
    process::Command,
};

/// Like compiler::test_util::run_pilcom_asm_string, but also runs RISCV executor.
pub fn verify_riscv_asm_string<T: FieldElement, S: serde::Serialize + Send + Sync + 'static>(
    file_name: &str,
    contents: &str,
    inputs: &[T],
    data: Option<&[(u32, S)]>,
    executor_witgen: bool,
) {
    let temp_dir = mktemp::Temp::new_dir().unwrap();

    let mut pipeline = Pipeline::default()
        .with_prover_inputs(inputs.to_vec())
        .with_output(temp_dir.to_path_buf(), true)
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)))
        .with_backend(BackendType::Mock, None);

    if let Some(data) = data {
        pipeline = pipeline.add_data_vec(data);
    }

    // Test with the fast RISCV executor.
    // TODO remove the guard once the executor is implemented for BB
    if T::known_field().unwrap() == KnownField::GoldilocksField {
        let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
        powdr_riscv_executor::execute(
            &analyzed,
            Default::default(),
            pipeline.data_callback().unwrap(),
            &[],
            None,
        );
    }

    // Compute the witness once for all tests that follow.
    // we will have to include a backend type here or compute_witness will panic
    pipeline.compute_witness().unwrap();

    test_mock_backend(pipeline.clone());

    // verify with PILCOM
    if T::known_field().unwrap() == KnownField::GoldilocksField {
        let pipeline_gl: Pipeline<GoldilocksField> =
            unsafe { std::mem::transmute(pipeline.clone()) };
        run_pilcom_with_backend_variant(pipeline_gl, BackendVariant::Composite).unwrap();
    }

    test_plonky3_pipeline::<T>(pipeline.clone());

    // verify executor generated witness
    if executor_witgen {
        let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
        let pil = pipeline.compute_backend_tuned_pil().unwrap().clone();
        let fixed = pipeline.compute_fixed_cols().unwrap().clone();
        let execution = powdr_riscv_executor::execute_with_witness(
            &analyzed,
            &pil,
            fixed,
            Default::default(),
            pipeline.data_callback().unwrap(),
            &[],
            None,
            None,
        );
        pipeline.rollback_from_witness();
        let executor_trace: Vec<_> = execution.trace.into_iter().collect();
        let pipeline = pipeline.add_external_witness_values(executor_trace);
        test_mock_backend(pipeline);
    }
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

pub fn compile_riscv_asm_file(asm_file: &Path, options: CompilerOptions, use_pie: bool) -> String {
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

    powdr_riscv::elf::translate(&executable, options)
}

pub fn verify_riscv_asm_file(
    asm_file: &Path,
    options: CompilerOptions,
    use_pie: bool,
    executor_witgen: bool,
) {
    let powdr_asm = compile_riscv_asm_file(asm_file, options, use_pie);
    let case_name = asm_file.file_stem().unwrap().to_str().unwrap();

    match options.field {
        KnownField::BabyBearField => {
            verify_riscv_asm_string::<BabyBearField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
                false,
            );
        }
        KnownField::KoalaBearField => {
            verify_riscv_asm_string::<KoalaBearField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
                false,
            );
        }
        KnownField::Mersenne31Field => todo!(),
        KnownField::GoldilocksField => {
            verify_riscv_asm_string::<GoldilocksField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
                executor_witgen,
            );
        }
        KnownField::Bn254Field => todo!(),
    }
}
