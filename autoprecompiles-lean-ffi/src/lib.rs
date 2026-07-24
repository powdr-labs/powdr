//! FFI wrapper around the Lean implementation of the apc-optimizer, a verified circuit optimizer.
//!
//! The apc-optimizer is compiled to a static archive (see `build.rs`) and exposed through a
//! small C shim (`c/ffi_shim.c`). This crate wraps that shim in a safe, JSON-in/JSON-out Rust
//! function. It deliberately knows nothing about powdr's `SymbolicMachine` type — callers do the
//! serde themselves — so that `powdr-autoprecompiles` can depend on this crate without a cycle.
//!
//! The input JSON must be a powdr APC export: `{"machine": <SymbolicMachine>, "bus_map":
//! <BusMap>}`. The output is a bare `SymbolicMachine` JSON string, or `{"error": "..."}` on a
//! parse failure inside Lean.

#[cfg(not(lean_ffi_unavailable))]
use std::ffi::{c_char, CStr, CString};

/// Which known VM to optimize for; the discriminant must match apc-optimizer's `KnownVm`
/// (`ApcOptimizer/Ffi.lean`): `OpenVm` optimizes over BabyBear, `Sp1` over KoalaBear.
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum KnownVm {
    OpenVm = 0,
    Sp1 = 1,
}

#[cfg(not(lean_ffi_unavailable))]
extern "C" {
    fn apc_optimizer_optimize(
        vm: u8,
        degree_identities: u64,
        degree_bus_interactions: u64,
        input: *const c_char,
    ) -> *mut c_char;
    fn apc_optimizer_free(p: *mut c_char);
}

/// Run the apc-optimizer on a powdr APC export JSON string.
///
/// `vm` selects the target VM (and thus the field) apc-optimizer parses and optimizes over;
/// `degree_identities` / `degree_bus_interactions` are the two components of the degree bound the
/// optimizer must respect.
///
/// Returns the optimized `SymbolicMachine` JSON on success. Returns `Err` if the input contains
/// interior NUL bytes, if the Lean side reports a parse error (`{"error": ...}`), or if the
/// returned bytes are not valid UTF-8.
#[cfg(not(lean_ffi_unavailable))]
pub fn optimize_json(
    vm: KnownVm,
    degree_identities: u64,
    degree_bus_interactions: u64,
    input: &str,
) -> Result<String, String> {
    let c_input = CString::new(input).map_err(|e| format!("input contains NUL byte: {e}"))?;

    // SAFETY: `apc_optimizer_optimize` copies the input into a Lean string and returns a freshly
    // malloc'd, NUL-terminated C string that we own and must free with `apc_optimizer_free`.
    let out_ptr = unsafe {
        apc_optimizer_optimize(
            vm as u8,
            degree_identities,
            degree_bus_interactions,
            c_input.as_ptr(),
        )
    };
    if out_ptr.is_null() {
        return Err("apc_optimizer_optimize returned null".to_string());
    }

    let out = unsafe { CStr::from_ptr(out_ptr) }
        .to_str()
        .map(|s| s.to_owned())
        .map_err(|e| format!("apc_optimizer_optimize returned invalid UTF-8: {e}"));
    unsafe { apc_optimizer_free(out_ptr) };

    let out = out?;

    // The Lean entry point returns `{"error": "..."}` when it cannot parse the input.
    if out.starts_with("{\"error\":") {
        return Err(format!("apc-optimizer error: {out}"));
    }
    Ok(out)
}

/// Stub used when the crate was built without a Lean toolchain (see `build.rs`). Compiles so that
/// `--all-features` builds succeed without elan, but panics if the optimizer is actually invoked.
#[cfg(lean_ffi_unavailable)]
pub fn optimize_json(
    _vm: KnownVm,
    _degree_identities: u64,
    _degree_bus_interactions: u64,
    _input: &str,
) -> Result<String, String> {
    panic!(
        "powdr-autoprecompiles-lean-ffi was built without the Lean toolchain (`lean`/`lake` absent \
         at build time), so the Lean apc-optimizer is unavailable. Install elan and rebuild to use \
         it."
    );
}

#[cfg(all(test, lean_ffi_unavailable))]
mod stub_tests {
    use super::*;

    #[test]
    #[should_panic(expected = "built without the Lean toolchain")]
    fn stub_optimize_json_panics_at_runtime() {
        let _ = optimize_json(KnownVm::OpenVm, 3, 2, "{}");
    }
}

#[cfg(all(test, not(lean_ffi_unavailable)))]
mod tests {
    use super::*;

    #[test]
    fn empty_machine_roundtrips() {
        let input = r#"{"machine":{"constraints":[],"bus_interactions":[]},"bus_map":{"bus_ids":{}},"next_free_id":0}"#;
        let out = optimize_json(KnownVm::OpenVm, 3, 2, input).expect("optimize_json failed");
        // The output wraps the machine as `{machine, next_free_id}`. Key order is not significant
        // for serde; just check the shape.
        assert!(out.contains("\"constraints\":[]"), "got: {out}");
        assert!(out.contains("\"bus_interactions\":[]"), "got: {out}");
        assert!(out.contains("\"derived_columns\":[]"), "got: {out}");
        assert!(out.contains("\"next_free_id\""), "got: {out}");
    }

    #[test]
    fn parse_error_is_reported() {
        let err = optimize_json(KnownVm::OpenVm, 3, 2, "not json").unwrap_err();
        assert!(err.contains("apc-optimizer error"), "got: {err}");
    }
}
