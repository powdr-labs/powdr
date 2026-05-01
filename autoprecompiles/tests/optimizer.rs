use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use expect_test::expect;
use itertools::Itertools;
use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::export::{ApcWithBusMap, SimpleInstruction};
use powdr_autoprecompiles::optimizer::optimize;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::{Apc, ColumnAllocator, DegreeBound, Substitution};
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

#[allow(clippy::print_stdout)]
fn expect_file_contents(file_path: &Path, actual: &str, test_name: &str) {
    let should_update_expectation = std::env::var("UPDATE_EXPECT")
        .map(|v| v.as_str() == "1")
        .unwrap_or(false);

    let expected = file_path
        .exists()
        .then(|| fs::read_to_string(file_path).unwrap());

    match (expected, should_update_expectation) {
        (Some(expected), _) if expected == actual => {
            // Test succeeded.
        }
        (Some(expected), false) => {
            // The expectation file exists, is different from "actual" and we are
            // not allowed to update it.
            pretty_assertions::assert_eq!(
                expected.trim(),
                actual.trim(),
                "The output of `{test_name}` does not match the expected output. \
                 To overwrite the expected output with the currently generated one, \
                 re-run the test with the environment variable `UPDATE_EXPECT=1`.",
            );
        }
        _ => {
            // Expectation file does not exist or is different from "actual" and we are allowed to update it.
            fs::create_dir_all(file_path.parent().unwrap()).unwrap();
            fs::write(file_path, actual).unwrap();
            println!("Expected output for `{test_name}` was created. Re-run the test to confirm.");
        }
    }
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
fn test_single_div_nondet() {
    let apc = import_apc_from_gzipped_json("tests/single_div_nondet.json.gz");

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

#[test]
fn wasm_register_reuse() {
    let apc = import_apc_from_gzipped_json("tests/wasm_register_reuse.json.gz");

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

    expect_file_contents(
        &Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests")
            .join("wasm_register_reuse.txt"),
        &machine.to_string(),
        "wasm_register_reuse",
    );

    expect![[r#"
        32
    "#]]
    .assert_debug_eq(&machine.main_columns().count());
    expect![[r#"
        20
    "#]]
    .assert_debug_eq(&machine.bus_interactions.len());
    expect![[r#"
        14
    "#]]
    .assert_debug_eq(&machine.constraints.len());
}

/// Reusable JIT analysis function
#[allow(clippy::print_stdout)]
fn jit_analysis(apc_data: &ApcWithBusMap<TestApc, BusMap<OpenVmBusType>>, label: &str) {
    let bus_map = &apc_data.bus_map;

    let pre_opt_machine: SymbolicMachine<BabyBearField> = apc_data.apc.machine.clone();
    let pre_opt_subs: Vec<Vec<Substitution>> = apc_data.apc.subs.clone();
    let pre_opt_cols: Vec<_> = pre_opt_machine.main_columns().sorted().collect();

    println!("\n{}", "=".repeat(60));
    println!("  JIT Analysis: {label}");
    println!("{}", "=".repeat(60));
    println!("Pre-opt: {} cols, {} constraints, {} bus, {} instructions",
        pre_opt_cols.len(), pre_opt_machine.constraints.len(),
        pre_opt_machine.bus_interactions.len(), pre_opt_subs.len());

    let machine: SymbolicMachine<BabyBearField> = apc_data.apc.machine.clone();
    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let (opt_machine, _) = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        bus_map,
        column_allocator,
        &mut Default::default(),
    )
    .unwrap();

    let opt_col_ids: std::collections::BTreeSet<u64> =
        opt_machine.main_columns().map(|r| r.id).collect();
    let opt_cols: Vec<_> = opt_machine.main_columns().sorted().collect();

    println!("Post-opt: {} cols, {} constraints, {} bus, {} derived",
        opt_cols.len(), opt_machine.constraints.len(),
        opt_machine.bus_interactions.len(), opt_machine.derived_columns.len());

    let mut total_surviving_subs = 0;
    let unused_instructions = pre_opt_subs
        .iter()
        .filter(|subs| subs.iter().all(|s| !opt_col_ids.contains(&s.apc_poly_id)))
        .count();
    for subs in &pre_opt_subs {
        total_surviving_subs += subs.iter().filter(|s| opt_col_ids.contains(&s.apc_poly_id)).count();
    }
    println!("Column reduction: {} -> {} ({:.1}x)",
        pre_opt_cols.len(), opt_cols.len(),
        pre_opt_cols.len() as f64 / opt_cols.len().max(1) as f64);
    println!("Surviving subs: {total_surviving_subs}/{}", pre_opt_cols.len());
    println!("Zero-sub instructions: {unused_instructions}/{}", pre_opt_subs.len());
    println!("Derived columns: {}", opt_machine.derived_columns.len());

    // Width distribution
    let width_distribution: BTreeMap<usize, usize> = pre_opt_subs
        .iter()
        .map(|subs| subs.len())
        .fold(BTreeMap::new(), |mut acc, w| { *acc.entry(w).or_default() += 1; acc });
    println!("\nAIR types (by width): {:?}", width_distribution);

    // Build id-to-name map
    let id_to_name: BTreeMap<u64, String> = pre_opt_cols
        .iter()
        .map(|c| (c.id, c.name.to_string()))
        .collect();

    // Global pattern classification
    let mut global_patterns: BTreeMap<&str, usize> = BTreeMap::new();
    for subs in &pre_opt_subs {
        for sub in subs {
            if !opt_col_ids.contains(&sub.apc_poly_id) { continue; }
            let col_name = id_to_name.get(&sub.apc_poly_id).cloned().unwrap_or_default();
            let base_name = col_name.rsplitn(2, '_').last().unwrap_or(&col_name);
            let category = if base_name.contains("timestamp_lt_aux__lower_decomp") {
                "COMPUTED: timestamp_decomp"
            } else if base_name.contains("mem_ptr_limbs") {
                "COMPUTED: mem_ptr_limbs"
            } else if base_name.contains("write_data__") {
                "COMPUTED: write_data"
            } else if base_name.contains("flags__") || base_name.contains("is_valid")
                || base_name.contains("is_load") || base_name.contains("needs_write")
                || base_name.contains("cmp_result")
            {
                "COMPUTED: flags/opcode_dep"
            } else if base_name.contains("sign_xor") || base_name.contains("_inv")
                || base_name.contains("lt_marker") || base_name.contains("lt_diff")
                || base_name.contains("r_prime") || base_name.contains("zero_divisor")
                || base_name.contains("r_zero") || base_name.contains("q_sign")
                || base_name.contains("c_sign") || base_name.contains("c_sum_inv")
                || base_name.contains("r_sum_inv")
            {
                "COMPUTED: division/comparison aux"
            } else if base_name.contains("shift_amount") || base_name.contains("bit_shift")
                || base_name.contains("_shifted") || base_name.contains("limb_shift")
            {
                "COMPUTED: shift aux"
            } else {
                "DIRECT: copy/byte_decomp"
            };
            *global_patterns.entry(category).or_default() += 1;
        }
    }
    let total: usize = global_patterns.values().sum();
    println!("\nColumn computation categories:");
    let mut sorted: Vec<_> = global_patterns.iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(a.1));
    for (cat, count) in &sorted {
        println!("  {count:>5} ({:>5.1}%)  {cat}", **count as f64 / total as f64 * 100.0);
    }
    println!("  Total: {total}");

    // Derived column expressions
    if !opt_machine.derived_columns.is_empty() {
        println!("\nDerived columns:");
        for (i, d) in opt_machine.derived_columns.iter().enumerate().take(5) {
            println!("  {i}: {} = {}", d.variable, d.computation_method);
        }
        if opt_machine.derived_columns.len() > 5 {
            println!("  ... and {} more", opt_machine.derived_columns.len() - 5);
        }
    }

    // Constraint-only columns
    let substituted_col_ids: std::collections::BTreeSet<u64> = pre_opt_subs
        .iter()
        .flat_map(|subs| subs.iter())
        .filter(|s| opt_col_ids.contains(&s.apc_poly_id))
        .map(|s| s.apc_poly_id)
        .collect();
    let derived_col_ids: std::collections::BTreeSet<u64> = opt_machine
        .derived_columns.iter().map(|d| d.variable.id).collect();
    let constraint_only: usize = opt_col_ids.iter()
        .filter(|id| !substituted_col_ids.contains(id) && !derived_col_ids.contains(id))
        .count();
    if constraint_only > 0 {
        println!("\nWARNING: {constraint_only} constraint-only columns (not substituted, not derived)!");
        for id in opt_col_ids.iter()
            .filter(|id| !substituted_col_ids.contains(id) && !derived_col_ids.contains(id))
            .take(10)
        {
            let name = opt_cols.iter().find(|c| c.id == *id).map(|c| c.name.as_str()).unwrap_or("?");
            println!("    {} (id={})", name, id);
        }
    }
}

/// Detailed analysis of a keccak APC for JIT trace generation feasibility.
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_keccak() {
    let apc_data = import_apc_from_gzipped_json("tests/keccak_apc_pre_opt.json.gz");
    jit_analysis(&apc_data, "keccak");
}

/// Same analysis for ecrecover (different program type)
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_ecrecover() {
    let apc_data = import_apc_from_gzipped_json("tests/ecrecover_apc_pre_opt.json.gz");
    jit_analysis(&apc_data, "ecrecover");
}

/// Same analysis for sha256
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_sha256() {
    let apc_data = import_apc_from_gzipped_json("tests/sha256_apc_pre_opt.json.gz");
    jit_analysis(&apc_data, "sha256");
}

/// Same analysis for single div_nondet
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_div() {
    let apc_data = import_apc_from_gzipped_json("tests/single_div_nondet.json.gz");
    jit_analysis(&apc_data, "div_nondet");
}

/// Same analysis for reth_op
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_reth() {
    let apc_data = import_apc_from_gzipped_json("tests/apc_reth_op_bug.json.gz");
    jit_analysis(&apc_data, "reth_op");
}

/// Detailed analysis of a keccak APC for JIT trace generation feasibility.
/// Dumps:
/// - Pre/post-optimization column counts and names
/// - Substitution mapping (which original columns survive)
/// - Derived column expressions (computed columns)
/// - Per-instruction substitution breakdown
/// - Column classification (substituted vs derived vs constrained-only)
#[test]
#[allow(clippy::print_stdout)]
fn jit_tracegen_analysis_keccak_detail() {
    let apc_data = import_apc_from_gzipped_json("tests/keccak_apc_pre_opt.json.gz");
    let bus_map = &apc_data.bus_map;

    let pre_opt_machine: SymbolicMachine<BabyBearField> = apc_data.apc.machine.clone();
    let pre_opt_subs: Vec<Vec<Substitution>> = apc_data.apc.subs.clone();

    // Pre-optimization stats
    let pre_opt_cols: Vec<_> = pre_opt_machine.main_columns().sorted().collect();
    println!("=== PRE-OPTIMIZATION ===");
    println!("Total columns: {}", pre_opt_cols.len());
    println!("Total constraints: {}", pre_opt_machine.constraints.len());
    println!(
        "Total bus interactions: {}",
        pre_opt_machine.bus_interactions.len()
    );
    println!(
        "Total derived columns: {}",
        pre_opt_machine.derived_columns.len()
    );
    println!(
        "Instructions in block (subs count): {}",
        pre_opt_subs.len()
    );

    // Per-instruction substitution counts
    println!("\n=== PRE-OPT SUBSTITUTIONS (per instruction) ===");
    for (i, subs) in pre_opt_subs.iter().enumerate() {
        println!(
            "  Instruction {i}: {} substitutions (original col indices: [{}])",
            subs.len(),
            subs.iter()
                .map(|s| format!(
                    "orig_idx={} -> apc_id={}",
                    s.original_poly_index, s.apc_poly_id
                ))
                .take(5) // limit output
                .join(", ")
        );
        if subs.len() > 5 {
            println!("    ... and {} more", subs.len() - 5);
        }
    }

    // Now optimize
    let machine: SymbolicMachine<BabyBearField> = apc_data.apc.machine;
    let column_allocator = ColumnAllocator::from_max_poly_id_of_machine(&machine);
    let (opt_machine, _opt_col_alloc) = optimize::<_, _, _, OpenVmMemoryBusInteraction<_, _>>(
        machine,
        OpenVmBusInteractionHandler::default(),
        DEFAULT_DEGREE_BOUND,
        bus_map,
        column_allocator,
        &mut Default::default(),
    )
    .unwrap();

    // Build the Apc to get post-opt substitutions
    // Note: Apc::new filters subs to only keep those referenced in the optimized machine
    // We can't call Apc::new without the block, so instead we manually check which pre-opt
    // subs survive by checking if their apc_poly_id is in the optimized machine's columns.
    let opt_col_ids: std::collections::BTreeSet<u64> =
        opt_machine.main_columns().map(|r| r.id).collect();

    println!("\n=== POST-OPTIMIZATION ===");
    let opt_cols: Vec<_> = opt_machine.main_columns().sorted().collect();
    println!("Total columns: {}", opt_cols.len());
    println!("Total constraints: {}", opt_machine.constraints.len());
    println!(
        "Total bus interactions: {}",
        opt_machine.bus_interactions.len()
    );
    println!(
        "Total derived columns: {}",
        opt_machine.derived_columns.len()
    );

    // Count surviving substitutions per instruction
    println!("\n=== POST-OPT SURVIVING SUBSTITUTIONS (per instruction) ===");
    let mut total_surviving_subs = 0;
    let mut total_original_subs = 0;
    for (i, subs) in pre_opt_subs.iter().enumerate() {
        let surviving: Vec<_> = subs
            .iter()
            .filter(|s| opt_col_ids.contains(&s.apc_poly_id))
            .collect();
        total_surviving_subs += surviving.len();
        total_original_subs += subs.len();
        if !surviving.is_empty() {
            println!(
                "  Instruction {i}: {}/{} substitutions survive",
                surviving.len(),
                subs.len()
            );
        }
    }
    println!(
        "\n  TOTAL: {total_surviving_subs}/{total_original_subs} substitutions survive optimization"
    );

    // Classify columns
    let substituted_col_ids: std::collections::BTreeSet<u64> = pre_opt_subs
        .iter()
        .flat_map(|subs| subs.iter())
        .filter(|s| opt_col_ids.contains(&s.apc_poly_id))
        .map(|s| s.apc_poly_id)
        .collect();

    let derived_col_ids: std::collections::BTreeSet<u64> = opt_machine
        .derived_columns
        .iter()
        .map(|d| d.variable.id)
        .collect();

    let constraint_only_col_ids: std::collections::BTreeSet<u64> = opt_col_ids
        .iter()
        .filter(|id| !substituted_col_ids.contains(id) && !derived_col_ids.contains(id))
        .cloned()
        .collect();

    println!("\n=== COLUMN CLASSIFICATION ===");
    println!(
        "Substituted (copied from original traces): {}",
        substituted_col_ids.len()
    );
    println!(
        "Derived (computed from other columns): {}",
        derived_col_ids.len()
    );
    println!(
        "Constraint-only (not substituted, not derived): {}",
        constraint_only_col_ids.len()
    );

    // Show derived column expressions (first 20)
    println!("\n=== DERIVED COLUMN EXPRESSIONS (first 20) ===");
    for (i, d) in opt_machine.derived_columns.iter().take(20).enumerate() {
        println!("  {i}: {} = {}", d.variable, d.computation_method);
    }
    if opt_machine.derived_columns.len() > 20 {
        println!(
            "  ... and {} more",
            opt_machine.derived_columns.len() - 20
        );
    }

    // Show constraint-only columns (these are the problematic ones for JIT -
    // they aren't directly substituted and aren't derived, so their values come
    // from somewhere else, likely the constraint solver)
    println!("\n=== CONSTRAINT-ONLY COLUMNS (first 30) ===");
    let constraint_only_names: Vec<_> = opt_cols
        .iter()
        .filter(|c| constraint_only_col_ids.contains(&c.id))
        .take(30)
        .collect();
    for col in &constraint_only_names {
        println!("  {} (id={})", col.name, col.id);
    }
    if constraint_only_col_ids.len() > 30 {
        println!(
            "  ... and {} more",
            constraint_only_col_ids.len() - 30
        );
    }

    // Show the first 10 substituted column names for context
    println!("\n=== SAMPLE SUBSTITUTED COLUMN NAMES (first 30) ===");
    let sub_col_names: Vec<_> = opt_cols
        .iter()
        .filter(|c| substituted_col_ids.contains(&c.id))
        .take(30)
        .collect();
    for col in &sub_col_names {
        println!("  {} (id={})", col.name, col.id);
    }

    // Show first 10 constraints
    println!("\n=== ALGEBRAIC CONSTRAINTS (first 10) ===");
    for (i, c) in opt_machine.constraints.iter().take(10).enumerate() {
        println!("  {i}: {} = 0", c);
    }
    if opt_machine.constraints.len() > 10 {
        println!(
            "  ... and {} more constraints",
            opt_machine.constraints.len() - 10
        );
    }

    // Show first 10 bus interactions
    println!("\n=== BUS INTERACTIONS (first 10) ===");
    for (i, bi) in opt_machine.bus_interactions.iter().take(10).enumerate() {
        let bus_type = bus_map.bus_type(bi.id);
        println!(
            "  {i}: bus={} ({}), mult={}, args=[{}]",
            bi.id,
            bus_type,
            bi.mult,
            bi.args.iter().join(", ")
        );
    }
    if opt_machine.bus_interactions.len() > 10 {
        println!(
            "  ... and {} more bus interactions",
            opt_machine.bus_interactions.len() - 10
        );
    }

    // JIT feasibility summary
    println!("\n=== JIT FEASIBILITY SUMMARY ===");
    println!(
        "Column reduction: {} -> {} ({:.1}x)",
        pre_opt_cols.len(),
        opt_cols.len(),
        pre_opt_cols.len() as f64 / opt_cols.len() as f64
    );
    println!(
        "Substituted columns (direct copy, trivial for JIT): {}",
        substituted_col_ids.len()
    );
    println!(
        "Derived columns (algebraic formula, easy for JIT): {}",
        derived_col_ids.len()
    );
    println!(
        "Constraint-only columns (need solver/witness gen, hard for JIT): {}",
        constraint_only_col_ids.len()
    );
    let trivial_pct =
        (substituted_col_ids.len() + derived_col_ids.len()) as f64 / opt_cols.len() as f64 * 100.0;
    println!(
        "JIT-trivial columns: {:.1}% ({}/{})",
        trivial_pct,
        substituted_col_ids.len() + derived_col_ids.len(),
        opt_cols.len()
    );

    // Check how many original instructions contribute no surviving subs
    let unused_instructions = pre_opt_subs
        .iter()
        .filter(|subs| subs.iter().all(|s| !opt_col_ids.contains(&s.apc_poly_id)))
        .count();
    println!(
        "Original instructions with NO surviving substitutions: {}/{}",
        unused_instructions,
        pre_opt_subs.len()
    );
    println!(
        "  -> These instructions' traces are generated but entirely discarded!"
    );

    // Count distinct instruction widths (proxy for AIR types)
    let width_distribution: BTreeMap<usize, usize> = pre_opt_subs
        .iter()
        .map(|subs| subs.len())
        .fold(BTreeMap::new(), |mut acc, w| {
            *acc.entry(w).or_default() += 1;
            acc
        });
    println!("\n=== INSTRUCTION WIDTH DISTRIBUTION (proxy for AIR types) ===");
    for (width, count) in &width_distribution {
        println!("  Width {width} columns: {count} instructions");
    }
    println!("  Distinct widths: {}", width_distribution.len());

    // Build a map from apc_poly_id to column name (from pre-opt machine)
    let id_to_name: BTreeMap<u64, String> = pre_opt_cols
        .iter()
        .map(|c| (c.id, c.name.to_string()))
        .collect();

    // For each AIR width (proxy for AIR type), categorize surviving column names by pattern
    println!("\n=== SURVIVING COLUMN PATTERNS BY AIR TYPE ===");
    for (air_width, _) in &width_distribution {
        let mut pattern_counts: BTreeMap<String, usize> = BTreeMap::new();
        let mut sample_cols: BTreeMap<String, Vec<(usize, String)>> = BTreeMap::new();

        for (i, subs) in pre_opt_subs.iter().enumerate() {
            if subs.len() != *air_width {
                continue;
            }
            for sub in subs {
                if !opt_col_ids.contains(&sub.apc_poly_id) {
                    continue;
                }
                let col_name = id_to_name
                    .get(&sub.apc_poly_id)
                    .cloned()
                    .unwrap_or_else(|| format!("unknown_{}", sub.apc_poly_id));
                // Strip the instruction suffix (e.g., "_0", "_123") to get the base column name
                let base_name = col_name.rsplitn(2, '_').last().unwrap_or(&col_name);
                // Classify by pattern
                let pattern = if base_name.contains("timestamp_lt_aux__lower_decomp") {
                    "timestamp_decomp".to_string()
                } else if base_name.contains("prev_timestamp") {
                    "prev_timestamp (direct copy)".to_string()
                } else if base_name.contains("from_state__timestamp") {
                    "from_state_timestamp (direct copy)".to_string()
                } else if base_name.contains("from_state__pc") {
                    "from_state_pc (direct copy)".to_string()
                } else if base_name.contains("mem_ptr_limbs") {
                    "mem_ptr_limbs (computed)".to_string()
                } else if base_name.contains("read_data__") || base_name.contains("prev_data__") {
                    "memory_data (direct copy)".to_string()
                } else if base_name.contains("rs1_data__") || base_name.contains("rs2_data__") {
                    "register_data (byte decomp)".to_string()
                } else if base_name.contains("write_data__") {
                    "write_data (computed)".to_string()
                } else if base_name.contains("flags__") || base_name.contains("is_valid")
                    || base_name.contains("is_load")
                {
                    "flags/opcode_dep (computed)".to_string()
                } else if base_name.contains("_ptr") {
                    "register_ptr (direct copy)".to_string()
                } else if base_name.contains("imm") {
                    "immediate (direct copy)".to_string()
                } else if base_name.contains("mem_as") {
                    "mem_as (direct copy)".to_string()
                } else if base_name.contains("needs_write") {
                    "needs_write (computed)".to_string()
                } else if base_name.contains("a__") || base_name.contains("b__")
                    || base_name.contains("c__") || base_name.contains("d__")
                {
                    "alu_result_limbs (direct/computed)".to_string()
                } else if base_name.contains("cmp_result") {
                    "cmp_result (computed)".to_string()
                } else if base_name.contains("xor_result")
                    || base_name.contains("and_result")
                    || base_name.contains("or_result")
                {
                    "bitwise_result (direct copy)".to_string()
                } else {
                    format!("OTHER: {}", base_name)
                };
                *pattern_counts.entry(pattern.clone()).or_default() += 1;
                let samples = sample_cols.entry(pattern).or_default();
                if samples.len() < 3 {
                    samples.push((sub.original_poly_index, col_name.clone()));
                }
            }
        }
        println!("\n  AIR width={air_width}:");
        let mut sorted: Vec<_> = pattern_counts.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));
        for (pattern, count) in &sorted {
            let samples = sample_cols.get(*pattern).unwrap();
            let sample_str = samples
                .iter()
                .map(|(idx, name)| format!("col{}={}", idx, name))
                .collect::<Vec<_>>()
                .join(", ");
            println!("    {count:>4}x {pattern}  [{sample_str}]");
        }
    }

    // Summary: total across all AIR types
    println!("\n=== GLOBAL PATTERN SUMMARY ===");
    let mut global_patterns: BTreeMap<String, usize> = BTreeMap::new();
    for (i, subs) in pre_opt_subs.iter().enumerate() {
        for sub in subs {
            if !opt_col_ids.contains(&sub.apc_poly_id) {
                continue;
            }
            let col_name = id_to_name
                .get(&sub.apc_poly_id)
                .cloned()
                .unwrap_or_default();
            let base_name = col_name.rsplitn(2, '_').last().unwrap_or(&col_name);
            let category = if base_name.contains("timestamp_lt_aux__lower_decomp") {
                "COMPUTED: timestamp_decomp"
            } else if base_name.contains("mem_ptr_limbs") {
                "COMPUTED: mem_ptr_limbs"
            } else if base_name.contains("write_data__") {
                "COMPUTED: write_data"
            } else if base_name.contains("flags__")
                || base_name.contains("is_valid")
                || base_name.contains("is_load")
                || base_name.contains("needs_write")
                || base_name.contains("cmp_result")
            {
                "COMPUTED: flags/opcode_dep"
            } else {
                "DIRECT: copy/byte_decomp"
            };
            *global_patterns.entry(category.to_string()).or_default() += 1;
        }
    }
    let total_surviving: usize = global_patterns.values().sum();
    for (cat, count) in &global_patterns {
        println!(
            "  {count:>5} ({:>5.1}%)  {cat}",
            *count as f64 / total_surviving as f64 * 100.0
        );
    }
    println!("  Total: {total_surviving}");
}
