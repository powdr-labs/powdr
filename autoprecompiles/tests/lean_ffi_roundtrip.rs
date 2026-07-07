//! Empirical JSON round-trip test for the Leanr FFI.
//!
//! Loads a real APC fixture, serializes `{machine, bus_map}` exactly as the `optimize` Lean path
//! does, runs it through the Lean static library, and asserts the returned string deserializes
//! back into a `SymbolicMachine<BabyBearField>` without error. Keccak is large enough that the
//! Lean optimizer's re-encoding pass fires, so this also exercises the fresh-variable (`name@id`)
//! serialization path.
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

    // Task 1: no witgen hints, so derived columns stay empty.
    assert!(
        optimized.derived_columns.is_empty(),
        "expected empty derived_columns for {fixture}"
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
