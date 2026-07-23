//! Opt-in path that routes `optimize` through the apc-optimizer (the Lean4 verified optimizer) via
//! FFI, selected at runtime by `POWDR_USE_LEAN_OPTIMIZER`. Compiled only with the `lean-optimizer`
//! feature.

use powdr_autoprecompiles_lean_ffi::KnownVm;
use powdr_number::{FieldElement, KnownField};

use crate::{BusMap, DegreeBound, SymbolicMachine};

/// Returns whether the apc-optimizer should be used instead of the native Rust optimizer,
/// controlled by the `POWDR_USE_LEAN_OPTIMIZER` environment variable (`1`/`true`).
pub(super) fn enabled() -> bool {
    std::env::var("POWDR_USE_LEAN_OPTIMIZER")
        .map(|v| v == "1" || v == "true")
        .unwrap_or(false)
}

/// Map the machine's field to the VM the apc-optimizer should target.
fn known_vm<T: FieldElement>() -> KnownVm {
    match T::known_field() {
        Some(KnownField::BabyBearField) => KnownVm::OpenVm,
        Some(KnownField::KoalaBearField) => KnownVm::Sp1,
        other => {
            panic!("Lean optimizer supports BabyBear (OpenVM) and KoalaBear (SP1), got {other:?}")
        }
    }
}

/// The apc-optimizer's `{machine, next_free_id}` FFI result: the optimized machine plus the
/// allocator cursor advanced past every fresh column id the optimizer assigned.
#[derive(serde::Deserialize)]
struct OptimizerResult<T> {
    machine: SymbolicMachine<T>,
    next_free_id: u64,
}

/// Run the apc-optimizer via FFI: serialize `{machine, bus_map, next_free_id}`, call the Lean
/// static library, and deserialize the `{machine, next_free_id}` result.
///
/// `next_free_id` is the caller's next free column id; the optimizer draws the ids of any columns
/// it introduces (e.g. in re-encoding) starting there and returns the cursor advanced past them, so
/// the caller reseeds its `ColumnAllocator` directly — no rescanning of the machine required.
///
/// Panics (rather than returning the constrained `Error` type) if serialization, the FFI call, or
/// deserialization fails; with a valid powdr export none of these happen.
pub(super) fn optimize<T, BusTypes>(
    machine: &SymbolicMachine<T>,
    bus_map: &BusMap<BusTypes>,
    next_free_id: u64,
    degree_bound: DegreeBound,
) -> (SymbolicMachine<T>, u64)
where
    T: FieldElement,
    BusTypes: serde::Serialize,
{
    let vm = known_vm::<T>();
    let input =
        serde_json::json!({ "machine": machine, "bus_map": bus_map, "next_free_id": next_free_id });
    let input_str = serde_json::to_string(&input).expect("serializing machine for Lean FFI");
    let output_str = powdr_autoprecompiles_lean_ffi::optimize_json(
        vm,
        degree_bound.identities as u64,
        degree_bound.bus_interactions as u64,
        &input_str,
    )
    .unwrap_or_else(|e| panic!("apc-optimizer FFI failed: {e}"));
    let result: OptimizerResult<T> = serde_json::from_str(&output_str)
        .unwrap_or_else(|e| panic!("deserializing apc-optimizer output failed: {e}"));
    (result.machine, result.next_free_id)
}
