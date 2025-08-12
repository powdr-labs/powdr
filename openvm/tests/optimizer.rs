use expect_test::expect;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_autoprecompiles::{optimizer::optimize, DegreeBound};
use powdr_number::BabyBearField;
use powdr_openvm::BabyBearOpenVmApcAdapter;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};

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
        DegreeBound {
            identities: 5,
            bus_interactions: 5,
        },
        &default_openvm_bus_map(),
    )
    .unwrap();

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        5487
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        5192
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        233
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}
