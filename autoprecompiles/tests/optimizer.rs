use expect_test::expect;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::export::{ApcWithBusMap, ExportOptions, SimpleInstruction};
use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::{Apc, ColumnAllocator, DegreeBound};
use powdr_number::BabyBearField;
use powdr_openvm_bus_interaction_handler::memory_bus_interaction::OpenVmMemoryBusInteraction;
use powdr_openvm_bus_interaction_handler::{
    bus_map::{default_openvm_bus_map, OpenVmBusType},
    OpenVmBusInteractionHandler,
};
use test_log::test;

const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: 3,
    bus_interactions: 2,
};

type TestApc = Apc<BabyBearField, SimpleInstruction<BabyBearField>, (), ()>;

fn import_apc_from_gzipped_json(file: &str) -> ApcWithBusMap<TestApc, BusMap<OpenVmBusType>> {
    let file = std::fs::File::open(file).unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    serde_json::from_reader(reader).unwrap()
}

#[test]
fn load_machine_json() {
    let apc = import_apc_from_gzipped_json("tests/keccak_apc_pre_opt.json.gz");
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
    let apc = import_apc_from_gzipped_json("tests/keccak_apc_pre_opt.json.gz");

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
    let apc = import_apc_from_gzipped_json("tests/ecrecover_apc_pre_opt.json.gz");

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
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
        3730
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        2314
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        3114
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_sha256() {
    let apc = import_apc_from_gzipped_json("tests/sha256_apc_pre_opt.json.gz");

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
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
        12034
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        9539
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        3770
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_optimize_reth_op() {
    let apc = import_apc_from_gzipped_json("tests/apc_reth_op_bug.json.gz");
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
        &mut ExportOptions::from_env_vars(Some("3".to_string()), Some("/tmp/e".to_string()), 0),
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
