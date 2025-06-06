use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::powdr::UniqueColumns;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::{BusMap, OpenVmBusInteractionHandler};

use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.unique_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [3540, 3207, 506]
    );
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let machine = optimize(
        machine,
        OpenVmBusInteractionHandler::new(BusMap::openvm_base()),
        None, // opcode
        5,
    )
    .unwrap();

    println!(
        "Columns: {}, bus interactions: {}, constraints: {}",
        machine.unique_columns().count(),
        machine.bus_interactions.len(),
        machine.constraints.len()
    );

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    assert_eq!(
        [
            machine.unique_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [1996, 1775, 160]
    );
}

#[test]
fn test_conflicting_constraints_in_bus_interaction() {
    // Tests a file where a previous version of the optimizer reported conflicting
    // constraints in a bus interaction. Now that this is fixed, this test is
    // not very specific any more, but could still increase the test surface.
    let file =
        std::fs::File::open("tests/conflicting_constraints_in_bus_interaction.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let machine = optimize(
        machine,
        OpenVmBusInteractionHandler::new(BusMap::openvm_base()),
        None, // opcode
        5,
    )
    .unwrap();

    println!(
        "Columns: {}, bus interactions: {}, constraints: {}",
        machine.unique_columns().count(),
        machine.bus_interactions.len(),
        machine.constraints.len()
    );
    assert_eq!(
        [
            machine.unique_columns().count(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [34, 28, 20]
    );
}
