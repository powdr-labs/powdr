use powdr_autoprecompiles::SymbolicMachine;
use powdr_autoprecompiles::{optimizer::optimize, DegreeBound};
use powdr_number::BabyBearField;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};

use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.main_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [23838, 13167, 22998]
    );
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let machine = optimize(
        machine,
        OpenVmBusInteractionHandler::new(default_openvm_bus_map()),
        0x10ff,
        DegreeBound {
            identities: 5,
            bus_interactions: 5,
        },
        &default_openvm_bus_map(),
    )
    .unwrap();

    println!(
        "Columns: {}, bus interactions: {}, constraints: {}",
        machine.main_columns().count(),
        machine.bus_interactions.len(),
        machine.constraints.len()
    );

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.main_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [2010, 1783, 165]
    );
}
