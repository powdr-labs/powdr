use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;
use powdr_openvm::bus_interaction_handler::OpenVmBusInteractionHandler;

use test_log::test;

use pretty_assertions::assert_eq;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(
        [
            machine.constraint_columns().len(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [562, 3207, 506]
    );
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let machine = optimize(machine, OpenVmBusInteractionHandler::default(), 5);

    println!(
        "Columns: {}, bus interactions: {}, constraints: {}",
        machine.constraint_columns().len(),
        machine.bus_interactions.len(),
        machine.constraints.len()
    );
    assert_eq!(
        [
            machine.constraint_columns().len(),
            machine.bus_interactions.len(),
            machine.constraints.len()
        ],
        [562, 3207, 506]
    );
}
