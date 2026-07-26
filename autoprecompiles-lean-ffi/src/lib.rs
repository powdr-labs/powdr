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
//!
//! The call runs on a thread with a Lean-sized stack ([`LEAN_STACK_SIZE`]), so callers do not have
//! to provision one (e.g. via `RUST_MIN_STACK`).

use std::ffi::{c_char, CStr, CString};

/// Which known VM to optimize for; the discriminant must match apc-optimizer's `KnownVm`
/// (`ApcOptimizer/Ffi.lean`): `OpenVm` optimizes over BabyBear, `Sp1` over KoalaBear.
#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum KnownVm {
    OpenVm = 0,
    Sp1 = 1,
}

extern "C" {
    fn apc_optimizer_optimize(
        vm: u8,
        degree_identities: u64,
        degree_bus_interactions: u64,
        input: *const c_char,
    ) -> *mut c_char;
    fn apc_optimizer_free(p: *mut c_char);
}

/// Stack size for the thread the Lean entry point runs on, mirroring the 1 GiB that Lean's own
/// runtime gives `main` (`lean_run_main`, `lean::lthread::m_thread_stack_size`).
///
/// Lean-compiled code recurses over its data structures — e.g. the optimizer's entry encoding
/// walks the constraint list with one frame per constraint — so the depth grows with the size of
/// the APC, and a 170k-constraint machine overflows the 8 MiB main / 2 MiB spawned-thread stacks
/// Rust provides. It is virtual address space only: pages are committed as they are touched.
const LEAN_STACK_SIZE: usize = 1 << 30;

/// Name of Lean's own stack-size override, honored here with the same semantics as
/// `lean_run_main`: a size in KiB, rounded down to a page, plus a 128 KiB slack.
const LEAN_STACK_SIZE_KB_ENV: &str = "LEAN_STACK_SIZE_KB";

/// The stack size to run the Lean entry point with: [`LEAN_STACK_SIZE`], or the value of
/// `LEAN_STACK_SIZE_KB` if it is set to something that parses to a nonzero number of pages.
fn lean_stack_size() -> usize {
    const PAGE_SIZE: usize = 4096;
    std::env::var(LEAN_STACK_SIZE_KB_ENV)
        .ok()
        .and_then(|kb| kb.trim().parse::<usize>().ok())
        .map(|kb| (kb * 1024) & !(PAGE_SIZE - 1))
        .filter(|size| *size != 0)
        .map_or(LEAN_STACK_SIZE, |size| size + 128 * 1024)
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
///
/// Runs on a dedicated thread with a [`LEAN_STACK_SIZE`] stack, because the Lean side needs far
/// more stack than Rust threads get by default; see there. Blocks until it finishes, so this is
/// still a plain synchronous call.
pub fn optimize_json(
    vm: KnownVm,
    degree_identities: u64,
    degree_bus_interactions: u64,
    input: &str,
) -> Result<String, String> {
    std::thread::scope(|scope| {
        std::thread::Builder::new()
            .name("apc-optimizer".into())
            .stack_size(lean_stack_size())
            .spawn_scoped(scope, || {
                optimize_json_on_this_thread(vm, degree_identities, degree_bus_interactions, input)
            })
            .map_err(|e| format!("spawning the apc-optimizer thread failed: {e}"))?
            .join()
            // Propagate a panic in the Lean call to the caller as if it had happened here.
            .unwrap_or_else(|payload| std::panic::resume_unwind(payload))
    })
}

/// [`optimize_json`], but on the calling thread — which must have a Lean-sized stack.
fn optimize_json_on_this_thread(
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

#[cfg(test)]
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
