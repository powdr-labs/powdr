use expect_test::expect;
use itertools::Itertools;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::export::{ApcWithBusMap, SimpleInstruction};
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
    let machine = optimize::<
        _,
        _,
        _,
        OpenVmMemoryBusInteraction<_, _>,
        powdr_autoprecompiles::wom_memory_optimizer::NoWomMemory<_, _>,
    >(
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
    let machine = optimize::<
        _,
        _,
        _,
        OpenVmMemoryBusInteraction<_, _>,
        powdr_autoprecompiles::wom_memory_optimizer::NoWomMemory<_, _>,
    >(
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

    let machine = optimize::<
        _,
        _,
        _,
        OpenVmMemoryBusInteraction<_, _>,
        powdr_autoprecompiles::wom_memory_optimizer::NoWomMemory<_, _>,
    >(
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
fn test_single_div_nondet() {
    let apc = import_apc_from_gzipped_json("tests/single_div_nondet.json.gz");

    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    assert!(machine.derived_columns.is_empty());
    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);

    let machine = optimize::<
        _,
        _,
        _,
        OpenVmMemoryBusInteraction<_, _>,
        powdr_autoprecompiles::wom_memory_optimizer::NoWomMemory<_, _>,
    >(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        &default_openvm_bus_map(),
        column_allocator,
        &mut Default::default(),
    )
    .unwrap()
    .0;

    let algebraic_constraints_with_zero = machine
        .constraints
        .iter()
        .map(|c| c.to_string())
        .filter(|s| s.contains("zero"))
        .join("\n");
    expect![[r#"
        (zero_divisor_0 + r_zero_0) * (zero_divisor_0 + r_zero_0 - 1)
        zero_divisor_0 * (zero_divisor_0 - 1)
        zero_divisor_0 * (q__0_0 - 255)
        zero_divisor_0 * (q__1_0 - 255)
        zero_divisor_0 * (q__2_0 - 255)
        zero_divisor_0 * (q__3_0 - 255)
        (1 - zero_divisor_0) * ((c__0_0 + c__1_0 + c__2_0 + c__3_0) * c_sum_inv_0 - 1)
        r_zero_0 * (r_zero_0 - 1)
        (1 - (zero_divisor_0 + r_zero_0)) * ((r__0_0 + r__1_0 + r__2_0 + r__3_0) * r_sum_inv_0 - 1)
        (q__0_0 + q__1_0 + q__2_0 + q__3_0) * ((1 - zero_divisor_0) * (q_sign_0 - sign_xor_0))
        (q_sign_0 - sign_xor_0) * ((1 - zero_divisor_0) * q_sign_0)
        (1 - (zero_divisor_0 + r_zero_0 + lt_marker__0_0 + lt_marker__1_0 + lt_marker__2_0)) * (zero_divisor_0 + r_zero_0 + lt_marker__0_0 + lt_marker__1_0 + lt_marker__2_0)
        (1 - (zero_divisor_0 + r_zero_0 + lt_marker__0_0 + lt_marker__1_0 + lt_marker__2_0)) * (lt_diff_0 - (r_prime__3_0 * (2 * c_sign_0 - 1) + c__3_0 * (1 - 2 * c_sign_0)))
        zero_divisor_0 * (c__0_0 + c__1_0 + c__2_0 + c__3_0)
        r_zero_0 * (r__0_0 + r__1_0 + r__2_0 + r__3_0)"#]]
    .assert_eq(&algebraic_constraints_with_zero);

    expect![[r#"
        47
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        24
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        44
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

#[test]
fn test_optimize_reth_op() {
    let apc = import_apc_from_gzipped_json("tests/apc_reth_op_bug.json.gz");
    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    assert!(machine.derived_columns.is_empty());

    let bus_map = &apc.bus_map;
    let bus_int_handler = OpenVmBusInteractionHandler::new(bus_map.clone());

    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let machine = optimize::<
        _,
        _,
        _,
        OpenVmMemoryBusInteraction<_, _>,
        powdr_autoprecompiles::wom_memory_optimizer::NoWomMemory<_, _>,
    >(
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
