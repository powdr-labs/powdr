use mktemp::Temp;
use powdr_number::{BabyBearField, FieldElement, GoldilocksField, KnownField, KoalaBearField};
use powdr_pipeline::{
    test_util::{run_pilcom_with_backend_variant, test_plonky3_pipeline, BackendVariant},
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
) {
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let mut pipeline = Pipeline::default()
        .with_prover_inputs(inputs.to_vec())
        .with_output(temp_dir.to_path_buf(), true)
        .from_asm_string(contents.to_string(), Some(PathBuf::from(file_name)));

    if let Some(data) = data {
        pipeline = pipeline.add_data_vec(data);
    }

    // TODO remove the guard once the executor is implemented for BB
    if T::known_field().unwrap() == KnownField::GoldilocksField {
        let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
        let pil = pipeline.compute_optimized_pil().unwrap();
        powdr_riscv_executor::execute_ast(
            &analyzed,
            &pil,
            None,
            Default::default(),
            pipeline.data_callback().unwrap(),
            // Assume the RISC-V program was compiled without a bootloader, otherwise this will fail.
            &[],
            usize::MAX,
            powdr_riscv_executor::ExecMode::Fast,
            Default::default(),
        );
        let pipeline_gl: Pipeline<GoldilocksField> =
            unsafe { std::mem::transmute(pipeline.clone()) };
        run_pilcom_with_backend_variant(pipeline_gl, BackendVariant::Composite).unwrap();
    }

    test_plonky3_pipeline::<T>(pipeline);
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

pub fn verify_riscv_asm_file(asm_file: &Path, options: CompilerOptions, use_pie: bool) {
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

    let powdr_asm = powdr_riscv::elf::translate(&executable, options);

    match options.field {
        KnownField::BabyBearField => {
            verify_riscv_asm_string::<BabyBearField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
            );
        }
        KnownField::KoalaBearField => {
            verify_riscv_asm_string::<KoalaBearField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
            );
        }
        KnownField::Mersenne31Field => todo!(),
        KnownField::GoldilocksField => {
            verify_riscv_asm_string::<GoldilocksField, ()>(
                &format!("{case_name}.asm"),
                &powdr_asm,
                &[],
                None,
            );
        }
        KnownField::Bn254Field => todo!(),
    }
}
