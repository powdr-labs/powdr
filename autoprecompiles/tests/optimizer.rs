use powdr_autoprecompiles::memory_optimizer::optimize_memory;
use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(machine.constraints.len(), 506);
    assert_eq!(machine.bus_interactions.len(), 6485);
}

#[test]
fn test_optimize_memory() {
    let file = std::fs::File::open("tests/keccak_apc_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();

    let bus_interactions_before = machine.bus_interactions.len();
    let machine = optimize_memory(machine);
    let removed_bus_interactions = bus_interactions_before - machine.bus_interactions.len();
    assert_eq!(removed_bus_interactions, 447);
}
