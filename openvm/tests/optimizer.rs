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
        1808
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        1512
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        234
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_sha256() {
    let file = std::fs::File::open("tests/sha256_apc_pre_opt.cbor.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

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
        12506
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        9834
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        3858
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}
