mod common;

use common::{compile_riscv_asm_file, verify_riscv_asm_file, verify_riscv_asm_string};
use mktemp::Temp;
use powdr_number::{BabyBearField, FieldElement, GoldilocksField, KnownField};
use powdr_pipeline::{
    test_util::{run_pilcom_with_backend_variant, BackendVariant},
    Pipeline,
};
use powdr_riscv_executor::ProfilerOptions;
use std::path::{Path, PathBuf};
use test_log::test;

use powdr_riscv::{
    continuations::{rust_continuations, rust_continuations_dry_run},
    CompilerOptions, RuntimeLibs,
};

/// Compiles and runs a rust program with continuations, runs the full
/// witness generation & verifies it using Pilcom.
pub fn test_continuations(case: &str, prover_data: Vec<Vec<u8>>) {
    let temp_dir = Temp::new_dir().unwrap();

    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    // Test continuations from ELF file.
    let powdr_asm =
        powdr_riscv::elf::translate(&executable, CompilerOptions::new_gl().with_continuations());
    run_continuations_test(case, powdr_asm, prover_data);
}

fn run_continuations_test(case: &str, powdr_asm: String, prover_data: Vec<Vec<u8>>) {
    // Manually create tmp dir, so that it is the same in all chunks.
    let tmp_dir = mktemp::Temp::new_dir().unwrap();

    let mut pipeline = Pipeline::<GoldilocksField>::default()
        .from_asm_string(powdr_asm.clone(), Some(PathBuf::from(&case)))
        .with_prover_inputs(Default::default())
        .with_output(tmp_dir.to_path_buf(), false);

    for v in prover_data {
        pipeline = pipeline.add_to_initial_memory(v);
    }

    let pipeline_callback = |pipeline: &mut Pipeline<GoldilocksField>| -> Result<(), ()> {
        run_pilcom_with_backend_variant(pipeline.clone(), BackendVariant::Composite).unwrap();

        Ok(())
    };
    let bootloader_inputs = rust_continuations_dry_run(&mut pipeline, Default::default());
    rust_continuations(&mut pipeline, pipeline_callback, bootloader_inputs).unwrap();
}

/*

The RISCV GL machine cannot be used for BN anymore.
Uncomment if we ever write proper support for RISCV BN.

#[test]
#[ignore = "Too slow"]
// TODO: this a temporary test so we at least go through the bn254 code path.
// Once we fully support it, the whole test suite here should probably be modified to take a generic field, and this can be removed.
fn bn254_sanity_check() {
    let case = "trivial";

    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    log::info!("Verifying {case} converted from ELF file");
    let options = CompilerOptions::new(KnownField::Bn254Field, RuntimeLibs::new(), false);
    let from_elf = powdr_riscv::elf::translate(&executable, options);

    let temp_dir = mktemp::Temp::new_dir().unwrap().release();

    let file_name = format!("{case}_from_elf.asm");
    let mut pipeline = Pipeline::default()
        .with_output(temp_dir.to_path_buf(), false)
        .from_asm_string(from_elf, Some(PathBuf::from(file_name)));

    let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
    powdr_riscv_executor::execute_fast(
        &analyzed,
        Default::default(),
        pipeline.data_callback().unwrap(),
        // Assume the RISC-V program was compiled without a bootloader, otherwise this will fail.
        &[],
        Default::default(),
    );
    run_pilcom_with_backend_variant(pipeline, BackendVariant::Composite).unwrap();
}
*/

#[test]
#[ignore = "Too slow"]
fn trivial() {
    let case = "trivial";
    verify_riscv_crate(case, Default::default(), true)
}

#[test]
#[ignore = "Too slow"]
fn halt() {
    let case = "halt";
    verify_riscv_crate(case, Default::default(), true)
}

#[test]
#[ignore = "Too slow"]
fn zero_with_values() {
    let case = "zero_with_values";
    verify_riscv_crate(case, Default::default(), true)
}

#[test]
#[ignore = "Too slow"]
fn runtime_poseidon_gl() {
    let case = "poseidon_gl_via_coprocessor";
    let options = CompilerOptions::new_gl();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, true);
}

#[test]
#[ignore = "Too slow"]
fn runtime_poseidon2_gl() {
    let case = "poseidon2_gl_via_coprocessor";
    let options = CompilerOptions::new_gl().with_poseidon2();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

#[test]
#[ignore = "Too slow"]
fn inverse_gl() {
    let case = "goldilocks_inverse";
    let options = CompilerOptions::new_gl();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, true);
}

#[test]
#[ignore = "Too slow"]
fn sum() {
    let case = "sum";
    verify_riscv_crate(case, &[16u64, 4, 1, 2, 8, 5], true);
}

#[test]
#[ignore = "Too slow"]
fn byte_access() {
    let case = "byte_access";
    verify_riscv_crate(case, &[0u64, 104, 707], true);
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

    let v = [
        a0,
        a1,
        b0,
        b1,
        (c & 0xffffffff) as u32,
        ((c >> 32) & 0xffffffff) as u32,
    ];
    verify_riscv_crate_bb_with_data(case, Default::default(), vec![(1, v)]);
    verify_riscv_crate_gl_with_data(case, Default::default(), vec![(1, v)], true);
}

#[test]
#[ignore = "Too slow"]
fn memfuncs() {
    let case = "memfuncs";
    verify_riscv_crate(case, Default::default(), true);
}

#[test]
#[ignore = "Too slow"]
fn keccak() {
    let case = "keccak";
    verify_riscv_crate(case, Default::default(), false);
}

#[test]
#[ignore = "Too slow"]
fn keccak_powdr() {
    let case = "keccak_powdr";
    let options = CompilerOptions::new_gl().with_keccak();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

#[cfg(feature = "estark-polygon")]
#[test]
#[ignore = "Too slow"]
fn vec_median_estark_polygon() {
    let case = "vec_median";
    verify_riscv_crate(case, &[5u64, 11, 15, 75, 6, 5, 1, 4, 7, 3, 2, 9, 2], true);
}

#[test]
#[ignore = "Too slow"]
fn vec_median() {
    let case = "vec_median";
    verify_riscv_crate(case, &[5, 11, 15, 75, 6, 5, 1, 4, 7, 3, 2, 9, 2], true);
}

#[test]
#[ignore = "Too slow"]
fn password() {
    let case = "password_checker";
    verify_riscv_crate(case, Default::default(), true);
}

#[test]
#[ignore = "Too slow"]
fn std_hello_world() {
    let case = "std_hello_world";
    verify_riscv_crate(case, Default::default(), true);
}

#[test]
fn plonky3_verify() {
    let case = "plonky3_verify";
    // Just compile for now
    // TODO: make it execute like the other tests, and mark it as ignore = "Too slow".
    let temp_dir = Temp::new_dir().unwrap();
    powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );
}

#[test]
#[ignore = "Too slow"]
fn function_pointer() {
    let case = "function_pointer";
    verify_riscv_crate(case, &[2734, 735, 1999], true);
}

#[test]
#[ignore = "Too slow"]
fn runtime_ec_double() {
    let case = "ec_double";
    let options = CompilerOptions::new_gl().with_arith();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

#[test]
#[ignore = "Too slow"]
fn runtime_ec_add() {
    let case = "ec_add";
    let options = CompilerOptions::new_gl().with_arith();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

#[test]
#[ignore = "Too slow"]
fn runtime_affine_256() {
    let case = "affine_256";
    let options = CompilerOptions::new_gl().with_arith();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

#[test]
#[ignore = "Too slow"]
fn runtime_modmul_256() {
    let case = "modmul_256";
    let options = CompilerOptions::new_gl().with_arith();
    verify_riscv_crate_gl_with_options(case, Default::default(), options, false);
}

/*
mstore(0, 666)
return(0, 32)
*/
static BYTECODE: &str = "61029a60005260206000f3";

#[ignore = "Too slow"]
#[test]
fn evm() {
    let case = "evm";
    let bytes = hex::decode(BYTECODE).unwrap();

    verify_riscv_crate_bb_with_data(case, vec![], vec![(666, bytes.clone())]);
    verify_riscv_crate_gl_with_data(case, vec![], vec![(666, bytes)], true);
}

#[ignore = "Too slow"]
#[test]
fn sum_serde() {
    let case = "sum_serde";

    let data: Vec<u32> = vec![1, 2, 8, 5];
    let answer = data.iter().sum::<u32>();

    verify_riscv_crate_bb_with_data(case, vec![answer.into()], vec![(42, data.clone())]);
    verify_riscv_crate_gl_with_data(case, vec![answer.into()], vec![(42, data)], true);
}

#[ignore = "Too slow"]
#[test]
fn sum_serde_in_mem() {
    let case = "sum_serde_in_mem";

    let data: Vec<u32> = vec![1, 2, 8, 5];
    let answer = data.iter().sum::<u32>();

    test_continuations(
        case,
        vec![
            serde_cbor::to_vec(&answer).unwrap(),
            serde_cbor::to_vec(&data).unwrap(),
            serde_cbor::to_vec(&answer).unwrap(),
            serde_cbor::to_vec(&data).unwrap(),
        ],
    );
}

#[test]
#[ignore = "Too slow"]
fn read_slice() {
    read_slice_with_options::<BabyBearField>(CompilerOptions::new_bb());
    read_slice_with_options::<GoldilocksField>(CompilerOptions::new_gl());
}

/// Tests that the syscalls are inlined when the following pattern is used:
///     addi t0, x0, opcode
///     ecall
#[test]
fn syscalls_inlined_when_possible() {
    // The following program should have two inlined syscalls,
    // and two calls to the dispatcher that could not be inlined.
    let asm = r#"
    .section .text
    .globl _start
    _start:
        # inlined commit_public
        addi t0, x0, 12
        ecall

        # non-inlined halt
        ori t0, x0, 9
        ecall

        # inlined input
        addi t0, x0, 1 # input opcode
        ecall

        # non-inlined output
        ori t0, x0, 2 # output opcode
        ecall
    "#;
    let tmp_dir = Temp::new_dir().unwrap();
    let asm_file = tmp_dir.join("test.s");
    std::fs::write(&asm_file, asm).unwrap();

    let compiled = compile_riscv_asm_file(&asm_file, CompilerOptions::new_gl(), true);

    // The resulting compiled program should contain the following strings in
    // between the first automatic "return;" and the "__data_init:" definition,
    // in the provided order:
    let expected_strings = [
        // initial marker
        "return;",
        // from the inlined commit_public
        "commit_public 10, 11;",
        // from the non-inlined halt call
        "or 0, 0, 9, 5;",
        "jump __ecall_handler, 1;",
        // from the inlined input call
        "std::prelude::Query::Input",
        // from the non-inlined output call
        "or 0, 0, 2, 5;",
        "jump __ecall_handler, 1;",
        // final marker
        "__data_init:",
    ];

    let mut remaining = compiled.as_str();
    for expected in &expected_strings {
        let pos = remaining
            .find(expected)
            .unwrap_or_else(|| panic!("Expected string not found in generated code: {expected}"));
        remaining = &remaining[pos + expected.len()..];
    }
}

fn read_slice_with_options<T: FieldElement>(options: CompilerOptions) {
    let case = "read_slice";
    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );
    let powdr_asm = powdr_riscv::elf::translate(&executable, options);

    let data: Vec<u32> = vec![];
    let answer = data.iter().sum::<u32>();

    use std::collections::BTreeMap;
    let d: BTreeMap<u32, Vec<T>> = vec![(
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

    let mut pipeline = Pipeline::<T>::default()
        .from_asm_string(powdr_asm, Some(PathBuf::from(case)))
        .with_backend(powdr_backend::BackendType::Mock, None)
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

    verify_riscv_crate_bb_with_data(case, vec![], vec![(42, data1.clone()), (43, data2.clone())]);
    verify_riscv_crate_gl_with_data(case, vec![], vec![(42, data1), (43, data2)], true);
}

const DISPATCH_TABLE_S: &str = "tests/riscv_data/dispatch_table/dispatch_table.s";

/// Tests that the dispatch table is correctly relocated when PIE is enabled.
#[ignore = "Too slow"]
#[test]
fn dispatch_table_pie_relocation() {
    verify_riscv_asm_file(
        Path::new(DISPATCH_TABLE_S),
        CompilerOptions::new_bb(),
        true,
        false,
    );
    verify_riscv_asm_file(
        Path::new(DISPATCH_TABLE_S),
        CompilerOptions::new_gl(),
        true,
        true,
    );
}

/// Tests that the dispatch table is correctly relocated when PIE is disabled.
#[ignore = "Too slow"]
#[test]
fn dispatch_table_static_relocation() {
    verify_riscv_asm_file(
        Path::new(DISPATCH_TABLE_S),
        CompilerOptions::new_bb(),
        false,
        false,
    );
    verify_riscv_asm_file(
        Path::new(DISPATCH_TABLE_S),
        CompilerOptions::new_gl(),
        false,
        true,
    );
}

#[test]
#[ignore = "Too slow"]
#[should_panic(expected = "reached a fail instruction")]
fn print() {
    let case = "print";
    verify_riscv_crate(case, &[0], true);
}

#[test]
#[ignore = "Too slow"]
// Test compiling a program with features.
// If no features are enabled, the expected input is 0.
// The test program has two features, "add_two" and "add_three".
// Enabling these features adds 2 and 3 to the expected input, respectively.
fn features() {
    features_with_options::<BabyBearField>(CompilerOptions::new_bb());
    features_with_options::<GoldilocksField>(CompilerOptions::new_gl());
}

fn features_with_options<T: FieldElement>(options: CompilerOptions) {
    let case = "features";

    let temp_dir = Temp::new_dir().unwrap();

    // no features
    let expected = 0;
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    log::info!("Verifying {case} converted from ELF file");
    let from_elf = powdr_riscv::elf::translate(&executable, options);
    verify_riscv_asm_string::<T, usize>(
        &format!("{case}_from_elf.asm"),
        &from_elf,
        &[expected.into()],
        None,
        false,
    );

    // "add_two"
    let expected = 2;
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        Some(vec!["add_two".to_string()]),
    );

    log::info!("Verifying {case} converted from ELF file");
    let from_elf = powdr_riscv::elf::translate(&executable, options);
    verify_riscv_asm_string::<T, usize>(
        &format!("{case}_from_elf.asm"),
        &from_elf,
        &[expected.into()],
        None,
        false,
    );

    // "add_two" and "add_three"
    let expected = 5;
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        Some(vec!["add_two".to_string(), "add_three".to_string()]),
    );

    log::info!("Verifying {case} converted from ELF file");
    let from_elf = powdr_riscv::elf::translate(&executable, options);
    verify_riscv_asm_string::<T, usize>(
        &format!("{case}_from_elf.asm"),
        &from_elf,
        &[expected.into()],
        None,
        false,
    );
}

#[test]
#[ignore = "Too slow"]
fn many_chunks_dry() {
    // Compiles and runs the many_chunks example with continuations, just computing
    // and validating the bootloader inputs.
    // Doesn't do a full witness generation, verification, or proving.
    let case = "many_chunks";
    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );
    let powdr_asm =
        powdr_riscv::elf::translate(&executable, CompilerOptions::new_gl().with_continuations());

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
#[ignore = "Too slow"]
fn output_syscall() {
    output_syscall_with_options::<BabyBearField>(CompilerOptions::new_bb());
    output_syscall_with_options::<GoldilocksField>(CompilerOptions::new_gl());
}

fn output_syscall_with_options<T: FieldElement>(options: CompilerOptions) {
    let case = "output";
    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );
    let powdr_asm = powdr_riscv::elf::translate(&executable, options);

    let inputs = vec![1u32, 2, 3].into_iter().map(T::from).collect();
    let mut pipeline = Pipeline::<T>::default()
        .with_backend(powdr_backend::BackendType::Mock, None)
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
    test_continuations("many_chunks", Vec::new())
}

#[test]
#[ignore = "Too slow"]
fn many_chunks_memory() {
    test_continuations("many_chunks_memory", Vec::new())
}

fn verify_riscv_crate(case: &str, inputs: &[u64], executor_witgen: bool) {
    verify_riscv_crate_gl(
        case,
        inputs.iter().map(|&x| x.into()).collect(),
        executor_witgen,
    );
    verify_riscv_crate_bb(case, inputs.iter().map(|&x| x.into()).collect());
}

fn verify_riscv_crate_bb(case: &str, inputs: Vec<BabyBearField>) {
    let options = CompilerOptions::new_bb();
    verify_riscv_crate_impl::<BabyBearField, ()>(case, options, inputs, None, false)
}

fn verify_riscv_crate_gl(case: &str, inputs: Vec<GoldilocksField>, executor_witgen: bool) {
    let options = CompilerOptions::new_gl();
    verify_riscv_crate_impl::<GoldilocksField, ()>(case, options, inputs, None, executor_witgen)
}

fn verify_riscv_crate_gl_with_options(
    case: &str,
    inputs: Vec<GoldilocksField>,
    options: CompilerOptions,
    executor_witgen: bool,
) {
    verify_riscv_crate_impl::<GoldilocksField, ()>(case, options, inputs, None, executor_witgen)
}

fn verify_riscv_crate_bb_with_data<S: serde::Serialize + Send + Sync + 'static>(
    case: &str,
    inputs: Vec<BabyBearField>,
    data: Vec<(u32, S)>,
) {
    let options = CompilerOptions::new_bb();
    verify_riscv_crate_impl(case, options, inputs, Some(data), false)
}

fn verify_riscv_crate_gl_with_data<S: serde::Serialize + Send + Sync + 'static>(
    case: &str,
    inputs: Vec<GoldilocksField>,
    data: Vec<(u32, S)>,
    executor_witgen: bool,
) {
    let options = CompilerOptions::new_gl();
    verify_riscv_crate_impl(case, options, inputs, Some(data), executor_witgen)
}

fn verify_riscv_crate_impl<T: FieldElement, S: serde::Serialize + Send + Sync + 'static>(
    case: &str,
    options: CompilerOptions,
    inputs: Vec<T>,
    data: Option<Vec<(u32, S)>>,
    executor_witgen: bool,
) {
    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    log::info!("Verifying {case}");
    let from_elf = powdr_riscv::elf::translate(&executable, options);
    verify_riscv_asm_string(
        &format!("{case}_from_elf.asm"),
        &from_elf,
        &inputs,
        data.as_deref(),
        executor_witgen,
    );
}

#[test]
fn profiler_sanity_check() {
    let case = "keccak";

    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    let options = CompilerOptions::new(KnownField::GoldilocksField, RuntimeLibs::new(), false);
    let asm = powdr_riscv::elf::translate(&executable, options);

    let temp_dir = mktemp::Temp::new_dir().unwrap().release();
    let file_name = format!("{case}.asm");
    let mut pipeline = Pipeline::<GoldilocksField>::default()
        .with_output(temp_dir.to_path_buf(), false)
        .from_asm_string(asm, Some(PathBuf::from(file_name)));
    let analyzed = pipeline.compute_analyzed_asm().unwrap().clone();
    let profiler_opt = ProfilerOptions {
        file_stem: Some("{case}".to_string()),
        output_directory: temp_dir.to_path_buf().to_str().unwrap().to_string(),
        flamegraph: true,
        callgrind: true,
    };
    powdr_riscv_executor::execute(
        &analyzed,
        Default::default(),
        pipeline.data_callback().unwrap(),
        &[],
        Some(profiler_opt),
    );

    // check files were created in temp dir, and that they are not empty
    let mut svg_path = temp_dir.to_path_buf();
    svg_path.push("{case}.svg");
    let flamegraph = std::fs::read_to_string(svg_path);
    assert!(!flamegraph.unwrap().is_empty());

    let mut callgrind_path = temp_dir.to_path_buf();
    callgrind_path.push("{case}.callgrind");
    let callgrind = std::fs::read_to_string(callgrind_path);
    assert!(!callgrind.unwrap().is_empty());
}

#[cfg(feature = "plonky3")]
#[test]
#[ignore = "Too slow"]
/// check that exported witness CSV can be loaded back in
fn exported_csv_as_external_witness() {
    use powdr_number::{read_polys_csv_file, CsvRenderMode};

    let case = "keccak";

    let temp_dir = Temp::new_dir().unwrap();
    let executable = powdr_riscv::compile_rust_crate_to_riscv(
        &format!("tests/riscv_data/{case}/Cargo.toml"),
        &temp_dir,
        None,
    );

    // compile
    let options = CompilerOptions::new(KnownField::GoldilocksField, RuntimeLibs::new(), false);
    let asm = powdr_riscv::elf::translate(&executable, options);

    // export witness
    let temp_dir = mktemp::Temp::new_dir().unwrap().release();
    let file_name = format!("{case}.asm");
    let mut pipeline = Pipeline::<GoldilocksField>::default()
        .with_output(temp_dir.to_path_buf(), false)
        .with_backend(powdr_backend::BackendType::Plonky3, None)
        .with_witness_csv_settings(true, false, CsvRenderMode::Hex)
        .from_asm_string(asm, Some(PathBuf::from(file_name)));
    pipeline.compute_witness().unwrap();
    pipeline.rollback_from_witness();

    // load witness back in and check that proving works
    let mut witness_path = temp_dir.to_path_buf();
    witness_path.push(format!("{case}_witness.csv"));
    let witness_csv = std::fs::File::open(witness_path).unwrap();
    let witness = read_polys_csv_file(witness_csv);
    let mut pipeline = pipeline.add_external_witness_values(witness);

    // check we can generate a proof
    pipeline.compute_proof().cloned().unwrap();
}
