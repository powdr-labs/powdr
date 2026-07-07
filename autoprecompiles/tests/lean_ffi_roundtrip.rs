//! Empirical JSON round-trip test for the Leanr FFI.
//!
//! Loads a real APC fixture, serializes `{machine, bus_map}` exactly as the `optimize` Lean path
//! does, runs it through the Lean static library, and asserts the returned string deserializes
//! back into a `SymbolicMachine<BabyBearField>` without error. The Lean FFI path runs a
//! witgen-safe configuration (the re-encoding pass, which would create hint-less columns, is
//! disabled), so the optimized machine contains no fresh columns that powdr witgen cannot fill.
//! `derived_columns` present on the input are carried through verbatim; a dedicated test
//! (`derived_columns_roundtrip`) checks that path with a synthetic hint.
//!
//! This test lives in `powdr-autoprecompiles` (not the FFI crate) because it needs
//! `SymbolicMachine`/`BusMap`; putting it in the FFI crate would create a dependency cycle.

use powdr_autoprecompiles::bus_map::BusMap;
use powdr_autoprecompiles::export::{ApcWithBusMap, SimpleInstruction};
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::{Apc, ColumnAllocator};
use powdr_number::BabyBearField;
use powdr_openvm_bus_interaction_handler::bus_map::OpenVmBusType;

type TestApc = Apc<BabyBearField, SimpleInstruction<BabyBearField>, (), ()>;

fn import_apc_from_gzipped_json(file: &str) -> ApcWithBusMap<TestApc, BusMap<OpenVmBusType>> {
    let file = std::fs::File::open(file).unwrap();
    let reader = flate2::read::GzDecoder::new(file);
    serde_json::from_reader(reader).unwrap()
}

fn roundtrip(fixture: &str) {
    let apc = import_apc_from_gzipped_json(fixture);
    let machine: SymbolicMachine<BabyBearField> = apc.apc.machine;
    let bus_map = apc.bus_map;

    let input = serde_json::json!({ "machine": &machine, "bus_map": &bus_map });
    let input_str = serde_json::to_string(&input).unwrap();

    let output_str = powdr_autoprecompiles_lean_ffi::optimize_json(&input_str)
        .unwrap_or_else(|e| panic!("Lean FFI failed for {fixture}: {e}"));

    // The core assertion: the Lean output deserializes into a SymbolicMachine.
    let optimized: SymbolicMachine<BabyBearField> = serde_json::from_str(&output_str)
        .unwrap_or_else(|e| panic!("deserializing Lean output for {fixture} failed: {e}"));

    // The witgen-safe Lean path emits no new hints, and these fixtures carry none on input, so the
    // output's derived columns match the (empty) input.
    assert_eq!(
        optimized.derived_columns.len(),
        machine.derived_columns.len(),
        "derived_columns count changed for {fixture}"
    );
    // The optimizer reduces size; the result should be non-empty and no larger than the input.
    assert!(
        optimized.constraints.len() <= machine.constraints.len(),
        "constraints grew for {fixture}"
    );
    assert!(
        optimized.bus_interactions.len() <= machine.bus_interactions.len(),
        "bus interactions grew for {fixture}"
    );

    // Reseeding the allocator from the Lean output must not panic (ids are well-formed).
    let _ = ColumnAllocator::from_max_poly_id_of_machine(&optimized);
}

#[test]
fn keccak_roundtrip() {
    roundtrip("tests/keccak_apc_pre_opt.json.gz");
}

#[test]
fn single_div_nondet_roundtrip() {
    roundtrip("tests/single_div_nondet.json.gz");
}

/// A `derived_columns` hint on the input must survive the Lean round-trip byte-for-byte in meaning:
/// the Lean optimizer carries derived columns through verbatim, and both sides agree on the serde
/// shape (`[is_new, "name@id", <ComputationMethod>]`, externally-tagged method).
#[test]
fn derived_columns_roundtrip() {
    // A minimal machine that also carries one `is_new = false` hint computing `q@5 = a@1 / b@2`.
    let input = r#"{"machine":{"constraints":[],"bus_interactions":[],"derived_columns":[[false,"q@5",{"QuotientOrZero":["a@1","b@2"]}]]},"bus_map":{"bus_ids":{}}}"#;

    let output_str = powdr_autoprecompiles_lean_ffi::optimize_json(input)
        .expect("Lean FFI failed for derived-column machine");

    let optimized: SymbolicMachine<BabyBearField> =
        serde_json::from_str(&output_str).expect("deserializing Lean output failed");

    assert_eq!(
        optimized.derived_columns.len(),
        1,
        "derived column was dropped: {output_str}"
    );
    let dc = &optimized.derived_columns[0];
    assert!(!dc.is_new, "is_new flag changed: {output_str}");
    assert_eq!(dc.variable.name.as_ref(), "q", "variable name changed");
    assert_eq!(dc.variable.id, 5, "variable id changed");
    // The allocator must place its next id above the derived column's id (5).
    let alloc = ColumnAllocator::from_max_poly_id_of_machine(&optimized);
    assert!(
        alloc.is_known_id(5),
        "allocator did not account for the derived-column id"
    );
}
