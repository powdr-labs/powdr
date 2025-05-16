use powdr_autoprecompiles::SymbolicMachine;
use powdr_number::BabyBearField;

#[test]
fn load_machine_cbor() {
    let file = std::fs::File::open("tests/machine_post_opt.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert_eq!(machine.constraints.len(), 6770);
    assert_eq!(machine.bus_interactions.len(), 3573);
}
