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

    let machine = optimize_memory(machine);

    println!(
        "columns: {}, constraints: {}, bus interactions: {}",
        machine.constraint_columns().len(),
        machine.constraints.len(),
        machine.bus_interactions.len()
    );
    assert_eq!(machine.constraint_columns().len(), 563);
    assert_eq!(machine.constraints.len(), 506);
    assert_eq!(machine.bus_interactions.len(), 6485);
}
