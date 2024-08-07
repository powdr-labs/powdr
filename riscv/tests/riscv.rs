mod common;

use common::{verify_riscv_asm_file, verify_riscv_asm_string};
use mktemp::Temp;
use powdr_number::GoldilocksField;
use powdr_pipeline::{
    test_util::{run_pilcom_with_backend_variant, BackendVariant},
    Pipeline,
};
use std::path::{Path, PathBuf};
use test_log::test;

use powdr_riscv::{
    continuations::{rust_continuations, rust_continuations_dry_run},
    Runtime,
};

/// Compiles and runs a rust program with continuations, runs the full
/// witness generation & verifies it using Pilcom.
pub fn test_continuations(case: &str) {
    let runtime = Runtime::base().with_poseidon_for_continuations();
    let temp_dir = Temp::new_dir().unwrap();

    let compiled = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );

    // Test continuations from ELF file.
    let powdr_asm = powdr_riscv::elf::translate::<GoldilocksField>(
        compiled.executable.as_ref().unwrap(),
        &runtime,
        true,
    );
    run_continuations_test(case, powdr_asm);

    // Test continuations from assembly files.
    let powdr_asm =
        powdr_riscv::asm::compile::<GoldilocksField>(compiled.load_asm_files(), &runtime, true);
    run_continuations_test(case, powdr_asm);
}

fn run_continuations_test(case: &str, powdr_asm: String) {
    // Manually create tmp dir, so that it is the same in all chunks.
    let tmp_dir = mktemp::Temp::new_dir().unwrap();

    let mut pipeline = Pipeline::<GoldilocksField>::default()
        .from_asm_string(powdr_asm.clone(), Some(PathBuf::from(&case)))
        .with_prover_inputs(Default::default())
        .with_output(tmp_dir.to_path_buf(), false);
    let pipeline_callback = |pipeline: Pipeline<GoldilocksField>| -> Result<(), ()> {
        run_pilcom_with_backend_variant(pipeline, BackendVariant::Composite).unwrap();

        Ok(())
    };
    let bootloader_inputs = rust_continuations_dry_run(&mut pipeline, Default::default());
    rust_continuations(pipeline, pipeline_callback, bootloader_inputs).unwrap();
}

#[test]
#[ignore = "Too slow"]
fn trivial() {
    let case = "trivial";
    verify_riscv_crate(case, Default::default(), &Runtime::base())
}

#[test]
#[ignore = "Too slow"]
fn halt() {
    let case = "halt";
    verify_riscv_crate(case, Default::default(), &Runtime::base())
}

#[test]
#[ignore = "Too slow"]
fn zero_with_values() {
    let case = "zero_with_values";
    verify_riscv_crate(case, Default::default(), &Runtime::base())
}

#[test]
#[ignore = "Too slow"]
fn runtime_poseidon_gl() {
    let case = "poseidon_gl_via_coprocessor";
    verify_riscv_crate(
        case,
        Default::default(),
        &Runtime::base().with_poseidon_no_continuations(),
    );
}

#[test]
#[ignore = "Too slow"]
fn sum() {
    let case = "sum";
    verify_riscv_crate(
        case,
        [16, 4, 1, 2, 8, 5].iter().map(|&x| x.into()).collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn byte_access() {
    let case = "byte_access";
    verify_riscv_crate(
        case,
        [0, 104, 707].iter().map(|&x| x.into()).collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn double_word() {
    let case = "double_word";
    let a0 = 0x01000000u32;
    let a1 = 0x010000ffu32;
    let b0 = 0xf100b00fu32;
    let b1 = 0x0100f0f0u32;
    let c = ((a0 as u64) | ((a1 as u64) << 32)).wrapping_mul((b0 as u64) | ((b1 as u64) << 32));
    verify_riscv_crate(
        case,
        [
            a0,
            a1,
            b0,
            b1,
            (c & 0xffffffff) as u32,
            ((c >> 32) & 0xffffffff) as u32,
        ]
        .iter()
        .map(|&x| x.into())
        .collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn memfuncs() {
    let case = "memfuncs";
    verify_riscv_crate(case, Default::default(), &Runtime::base());
}

#[test]
#[ignore = "Too slow"]
fn keccak() {
    let case = "keccak";
    verify_riscv_crate(case, Default::default(), &Runtime::base());
}

#[cfg(feature = "estark-polygon")]
#[test]
#[ignore = "Too slow"]
fn vec_median_estark_polygon() {
    let case = "vec_median";
    verify_riscv_crate(
        case,
        [5, 11, 15, 75, 6, 5, 1, 4, 7, 3, 2, 9, 2]
            .into_iter()
            .map(|x| x.into())
            .collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn vec_median() {
    let case = "vec_median";
    verify_riscv_crate(
        case,
        [5, 11, 15, 75, 6, 5, 1, 4, 7, 3, 2, 9, 2]
            .into_iter()
            .map(|x| x.into())
            .collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn password() {
    let case = "password_checker";
    verify_riscv_crate(case, Default::default(), &Runtime::base());
}

#[test]
#[ignore = "Too slow"]
fn std_hello_world() {
    let case = "std_hello_world";
    // We only test via ELF because std is not supported via assembly.
    verify_riscv_crate_impl::<()>(case, vec![], &Runtime::base(), true, false, None);
}

#[test]
#[ignore = "Too slow"]
fn function_pointer() {
    let case = "function_pointer";
    verify_riscv_crate(
        case,
        [2734, 735, 1999].into_iter().map(|x| x.into()).collect(),
        &Runtime::base(),
    );
}

#[test]
#[ignore = "Too slow"]
fn runtime_ec_double() {
    let case = "ec_double";
    verify_riscv_crate(case, vec![], &Runtime::base().with_arith());
}

#[test]
#[ignore = "Too slow"]
fn runtime_ec_add() {
    let case = "ec_add";
    verify_riscv_crate(case, vec![], &Runtime::base().with_arith());
}

#[test]
#[ignore = "Too slow"]
fn runtime_affine_256() {
    let case = "affine_256";
    verify_riscv_crate(case, vec![], &Runtime::base().with_arith());
}

#[test]
#[ignore = "Too slow"]
fn runtime_modmul_256() {
    let case = "modmul_256";
    verify_riscv_crate(case, vec![], &Runtime::base().with_arith());
}

/*
mstore(0, 666)
return(0, 32)
*/
#[cfg(feature = "complex-tests")]
static BYTECODE: &str = "61029a60005260206000f3";

#[cfg(feature = "complex-tests")]
#[ignore = "Too slow"]
#[test]
fn evm() {
    let case = "evm";
    let bytes = hex::decode(BYTECODE).unwrap();

    verify_riscv_crate_with_data(case, vec![], &Runtime::base(), vec![(666, bytes)]);
}

#[ignore = "Too slow"]
#[test]
fn sum_serde() {
    let case = "sum_serde";

    let data: Vec<u32> = vec![1, 2, 8, 5];
    let answer = data.iter().sum::<u32>();

    verify_riscv_crate_with_data(
        case,
        vec![answer.into()],
        &Runtime::base(),
        vec![(42, data)],
    );
}

#[test]
fn read_slice() {
    let case = "read_slice";
    let runtime = Runtime::base();
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm = powdr_riscv::compile_rust_crate_to_riscv_asm(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );
    let powdr_asm = powdr_riscv::asm::compile::<GoldilocksField>(riscv_asm, &runtime, false);

    let data: Vec<u32> = vec![];
    let answer = data.iter().sum::<u32>();

    use std::collections::BTreeMap;
    let d: BTreeMap<u32, Vec<GoldilocksField>> = vec![(
        42,
        vec![
            0u32.into(),
            1u32.into(),
            2u32.into(),
            3u32.into(),
            4u32.into(),
            5u32.into(),
            6u32.into(),
            7u32.into(),
        ],
    )]
    .into_iter()
    .collect();

    let mut pipeline = Pipeline::<GoldilocksField>::default()
        .from_asm_string(powdr_asm, Some(PathBuf::from(case)))
        .with_prover_inputs(vec![answer.into()])
        .with_prover_dict_inputs(d);

    pipeline.compute_witness().unwrap();
}

#[ignore = "Too slow"]
#[test]
fn two_sums_serde() {
    let case = "two_sums_serde";

    let data1: Vec<u32> = vec![1, 2, 8, 5];
    let data2 = data1.clone();

    verify_riscv_crate_with_data(
        case,
        vec![],
        &Runtime::base(),
        vec![(42, data1), (43, data2)],
    );
}

const DISPATCH_TABLE_S: &str = "tests/riscv_data/dispatch_table/dispatch_table.s";

/// Tests that the dispatch table is correctly relocated when PIE is enabled.
#[ignore = "Too slow"]
#[test]
fn dispatch_table_pie_relocation() {
    verify_riscv_asm_file(Path::new(DISPATCH_TABLE_S), &Runtime::base(), true);
}

/// Tests that the dispatch table is correctly relocated when PIE is disabled.
#[ignore = "Too slow"]
#[test]
fn dispatch_table_static_relocation() {
    verify_riscv_asm_file(Path::new(DISPATCH_TABLE_S), &Runtime::base(), false);
}

#[test]
#[ignore = "Too slow"]
#[should_panic(expected = "reached a fail instruction")]
fn print() {
    let case = "print";
    verify_riscv_crate(case, vec![0.into()], &Runtime::base());
}

#[test]
fn many_chunks_dry() {
    // Compiles and runs the many_chunks example with continuations, just computing
    // and validating the bootloader inputs.
    // Doesn't do a full witness generation, verification, or proving.
    let case = "many_chunks";
    let runtime = Runtime::base().with_poseidon_for_continuations();
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm = powdr_riscv::compile_rust_crate_to_riscv_asm(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );
    let powdr_asm = powdr_riscv::asm::compile::<GoldilocksField>(riscv_asm, &runtime, true);

    let mut pipeline = Pipeline::default()
        .from_asm_string(powdr_asm, Some(PathBuf::from(case)))
        .with_prover_inputs(Default::default());
    rust_continuations_dry_run::<GoldilocksField>(&mut pipeline, Default::default());
}

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct Point {
    x: i32,
    y: i32,
}

#[test]
fn output_syscall() {
    let case = "output";
    let runtime = Runtime::base();
    let temp_dir = Temp::new_dir().unwrap();
    let riscv_asm = powdr_riscv::compile_rust_crate_to_riscv_asm(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );
    let powdr_asm = powdr_riscv::asm::compile::<GoldilocksField>(riscv_asm, &runtime, false);

    let inputs = vec![1u32, 2, 3]
        .into_iter()
        .map(GoldilocksField::from)
        .collect();
    let mut pipeline = Pipeline::default()
        .from_asm_string(powdr_asm, Some(PathBuf::from(case)))
        .with_prover_inputs(inputs);

    pipeline.compute_witness().unwrap();

    let ctx = &pipeline.host_context();
    // Need to put the lock in a separate scope, so that it is dropped before the next read.
    {
        let fs = &ctx.file_data.lock().unwrap();
        assert_eq!(fs[&42], vec![1]);
        assert_eq!(fs[&43], vec![1, 2, 3]);
    }

    let p: Point = ctx.read(44).unwrap();
    assert_eq!(p.x, 1);
    assert_eq!(p.y, 2);
}

#[test]
#[ignore = "Too slow"]
fn many_chunks() {
    test_continuations("many_chunks")
}

#[test]
#[ignore = "Too slow"]
fn many_chunks_memory() {
    test_continuations("many_chunks_memory")
}

fn verify_riscv_crate(case: &str, inputs: Vec<GoldilocksField>, runtime: &Runtime) {
    verify_riscv_crate_impl::<()>(case, inputs, runtime, true, true, None)
}

fn verify_riscv_crate_with_data<S: serde::Serialize + Send + Sync + 'static>(
    case: &str,
    inputs: Vec<GoldilocksField>,
    runtime: &Runtime,
    data: Vec<(u32, S)>,
) {
    verify_riscv_crate_impl(case, inputs, runtime, true, true, Some(data))
}

fn verify_riscv_crate_impl<S: serde::Serialize + Send + Sync + 'static>(
    case: &str,
    inputs: Vec<GoldilocksField>,
    runtime: &Runtime,
    via_elf: bool,
    via_asm: bool,
    data: Option<Vec<(u32, S)>>,
) {
    let temp_dir = Temp::new_dir().unwrap();
    let compiled = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
    );

    if via_elf {
        log::info!("Verifying {case} converted from ELF file");
        let from_elf = powdr_riscv::elf::translate::<GoldilocksField>(
            compiled.executable.as_ref().unwrap(),
            runtime,
            false,
        );
        verify_riscv_asm_string(
            &format!("{case}_from_elf.asm"),
            &from_elf,
            &inputs,
            data.as_deref(),
        );
    }

    if via_asm {
        log::info!("Verifying {case} converted from assembly files");
        let from_asm =
            powdr_riscv::asm::compile::<GoldilocksField>(compiled.load_asm_files(), runtime, false);
        verify_riscv_asm_string(
            &format!("{case}_from_asm.asm"),
            &from_asm,
            &inputs,
            data.as_deref(),
        );
    }
}
