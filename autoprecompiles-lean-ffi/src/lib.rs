//! FFI wrapper around the Leanr (Lean4) verified circuit optimizer.
//!
//! The Leanr optimizer is compiled to a static archive (see `build.rs`) and exposed through a
//! small C shim (`c/ffi_shim.c`). This crate wraps that shim in a safe, JSON-in/JSON-out Rust
//! function. It deliberately knows nothing about powdr's `SymbolicMachine` type — callers do the
//! serde themselves — so that `powdr-autoprecompiles` can depend on this crate without a cycle.
//!
//! The input JSON must be a powdr APC export: `{"machine": <SymbolicMachine>, "bus_map":
//! <BusMap>}`. The output is a bare `SymbolicMachine` JSON string, or `{"error": "..."}` on a
//! parse failure inside Lean.

use std::ffi::{c_char, CStr, CString};

extern "C" {
    fn leanr_optimize(input: *const c_char) -> *mut c_char;
    fn leanr_free(p: *mut c_char);
}

/// Run the Leanr optimizer on a powdr APC export JSON string.
///
/// Returns the optimized `SymbolicMachine` JSON on success. Returns `Err` if the input contains
/// interior NUL bytes, if the Lean side reports a parse error (`{"error": ...}`), or if the
/// returned bytes are not valid UTF-8.
pub fn optimize_json(input: &str) -> Result<String, String> {
    let c_input = CString::new(input).map_err(|e| format!("input contains NUL byte: {e}"))?;

    // SAFETY: `leanr_optimize` copies the input into a Lean string and returns a freshly
    // malloc'd, NUL-terminated C string that we own and must free with `leanr_free`.
    let out_ptr = unsafe { leanr_optimize(c_input.as_ptr()) };
    if out_ptr.is_null() {
        return Err("leanr_optimize returned null".to_string());
    }

    let out = unsafe { CStr::from_ptr(out_ptr) }
        .to_str()
        .map(|s| s.to_owned())
        .map_err(|e| format!("leanr_optimize returned invalid UTF-8: {e}"));
    unsafe { leanr_free(out_ptr) };

    let out = out?;

    // The Lean entry point returns `{"error": "..."}` when it cannot parse the input.
    if out.starts_with("{\"error\":") {
        return Err(format!("Leanr optimizer error: {out}"));
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_machine_roundtrips() {
        let input =
            r#"{"machine":{"constraints":[],"bus_interactions":[]},"bus_map":{"bus_ids":{}}}"#;
        let out = optimize_json(input).expect("optimize_json failed");
        // Key order is not significant for serde; just check the shape.
        assert!(out.contains("\"constraints\":[]"), "got: {out}");
        assert!(out.contains("\"bus_interactions\":[]"), "got: {out}");
        assert!(out.contains("\"derived_columns\":[]"), "got: {out}");
    }

    #[test]
    fn parse_error_is_reported() {
        let err = optimize_json("not json").unwrap_err();
        assert!(err.contains("Leanr optimizer error"), "got: {err}");
    }
}
