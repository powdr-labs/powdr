use expect_test::expect;
use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};
use powdr_openvm::{BabyBearOpenVmApcAdapter, DEFAULT_DEGREE_BOUND};

use test_log::test;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        27194
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        13167
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        27689
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());

    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
    )
    .unwrap();

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        1756
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        1512
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        182
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_sha256() {
    let file = std::fs::File::open("tests/sha256_apc_pre_opt.cbor.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());

    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
    )
    .unwrap();

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        12394
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        9781
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        3746
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

// Extract the sign of byte range constraints
// #[cfg(test)]
// mod tests {
//     use powdr_openvm::symbolic_instruction_builder::*;
//     use powdr_openvm::tests::apc_builder_single_instructions::assert_machine_output;

//     #[test]
//     fn single_bge() {
//     let program = [
//         // pc = pc + 2 if x8 >= x5 (signed)
//         bge(8, 5, 2),
//     ];
//     assert_machine_output(program.to_vec(), "single_bge");
// }

// }
