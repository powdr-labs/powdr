use expect_test::expect;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::export::{ApcWithBusMap, SimpleInstruction};
use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::{Apc, ColumnAllocator};
use powdr_number::BabyBearField;
use powdr_openvm::bus_map::OpenVmBusType;
use powdr_openvm::memory_bus_interaction::OpenVmMemoryBusInteraction;
use powdr_openvm::DEFAULT_DEGREE_BOUND;
use powdr_openvm::{
    bus_interaction_handler::OpenVmBusInteractionHandler, bus_map::default_openvm_bus_map,
};

use serde::{Deserialize, Serialize};
use test_log::test;

type TestApc = Apc<BabyBearField, SimpleInstruction<BabyBearField>, (), ()>;

/// These custom bus types are those from Openvm.
#[derive(Serialize, Deserialize, PartialEq, Eq, Hash, Clone, Copy, Debug, derive_more::Display)]
enum TestBusType {
    VariableRangeChecker,
    TupleRangeChecker,
    BitwiseLookup,
}

#[test]
fn load_machine_json() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.json.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let apc: ApcWithBusMap<TestApc, BusMap<TestBusType>> = serde_json::from_reader(reader).unwrap();

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    assert!(machine.derived_columns.is_empty());

    expect![[r#"
        27521
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        13262
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        28627
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_optimize() {
    let file = std::fs::File::open("tests/keccak_apc_pre_opt.json.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let apc: ApcWithBusMap<TestApc, BusMap<TestBusType>> = serde_json::from_reader(reader).unwrap();

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    assert!(machine.derived_columns.is_empty());

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &apc.bus_map,
        column_allocator,
        &mut Default::default(),
    )
    .unwrap()
    .0;

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        2021
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        1734
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        186
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
    let machine = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
        &mut Default::default(),
    )
    .unwrap()
    .0;

    // This cbor file above has the `is_valid` column removed, this is why the number below
    // might be one less than in other tests.
    expect![[r#"
        2852
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        1619
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        2870
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

    let machine = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
        &mut Default::default(),
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

#[test]
fn test_optimize_reth_op() {
    let file = std::fs::File::open("tests/apc_reth_op_bug.json.gz").unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    let apc: ApcWithBusMap<TestApc, BusMap<OpenVmBusType>> =
        serde_json::from_reader(reader).unwrap();

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    assert!(machine.derived_columns.is_empty());

    let bus_map = &apc.bus_map;
    let bus_int_handler = OpenVmBusInteractionHandler::new(bus_map.clone(), [256, 8192]);

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        bus_int_handler,
        DEFAULT_DEGREE_BOUND,
        bus_map,
        column_allocator,
        &mut Default::default(),
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
