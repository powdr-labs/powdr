use expect_test::expect;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::ColumnAllocator;
use powdr_number::BabyBearField;
use powdr_openvm::bus_map::{
    OpenVmBusType, DEFAULT_BITWISE_LOOKUP, DEFAULT_EXECUTION_BRIDGE, DEFAULT_MEMORY,
    DEFAULT_PC_LOOKUP, DEFAULT_VARIABLE_RANGE_CHECKER,
};
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};
use powdr_openvm::{BabyBearOpenVmApcAdapter, BusType, DEFAULT_DEGREE_BOUND};

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

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
    )
    .unwrap()
    .0;

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        3121
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        2572
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        530
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_ecrecover() {
    let file = std::fs::File::open("tests/ecrecover_apc_pre_opt.cbor.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
    )
    .unwrap()
    .0;

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        2872
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        1635
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        2878
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_sha256() {
    let file = std::fs::File::open("tests/sha256_apc_pre_opt.cbor.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());
    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);

    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
    )
    .unwrap()
    .0;

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        12391
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        9753
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        3746
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

fn default_reth_openvm_bus_map() -> BusMap<OpenVmBusType> {
    /*
    bus map 0 EXECUTION_BRIDGE
    bus map 1 MEMORY
    bus map 2 PC_LOOKUP
    bus map 3 VARIABLE_RANGE_CHECKER
    bus map 6 BITWISE_LOOKUP
    bus map 8 TUPLE_RANGE_CHECKER
    */
    let bus_ids = [
        (DEFAULT_EXECUTION_BRIDGE, BusType::ExecutionBridge),
        (DEFAULT_MEMORY, BusType::Memory),
        (DEFAULT_PC_LOOKUP, BusType::PcLookup),
        (
            DEFAULT_VARIABLE_RANGE_CHECKER,
            BusType::Other(OpenVmBusType::VariableRangeChecker),
        ),
        (
            DEFAULT_BITWISE_LOOKUP,
            BusType::Other(OpenVmBusType::BitwiseLookup),
        ),
        (8, BusType::Other(OpenVmBusType::TupleRangeChecker)),
    ];
    BusMap::from_id_type_pairs(bus_ids)
}

#[test]
fn test_optimize_reth_op() {
    let bus_map = default_reth_openvm_bus_map();
    let bus_int_handler = OpenVmBusInteractionHandler::new(bus_map.clone(), [256, 8192]);

    let file = std::fs::File::open("tests/apc_reth_op_bug.cbor").unwrap();
    let reader = std::io::BufReader::new(file);
    let machine: SymbolicMachine<BabyBearField> = serde_cbor::from_reader(reader).unwrap();
    assert!(machine.derived_columns.is_empty());

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<BabyBearOpenVmApcAdapter>(
        machine,
        bus_int_handler,
        DEFAULT_DEGREE_BOUND,
        &bus_map,
        column_allocator,
    )
    .unwrap()
    .0;

    expect![[r#"
        446
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        356
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        313
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}
