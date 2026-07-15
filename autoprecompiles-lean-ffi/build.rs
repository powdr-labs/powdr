//! Build script: compile the apc-optimizer + C shim into one static archive and link it (plus
//! the Lean runtime) into the consuming Rust binary.
//!
//! Strategy (see the crate docs and the Task-1 PR): `lake build` in the apc-optimizer checkout produces a
//! native executable and, crucially, a linker response file (`.lake/build/bin/<exe>.rsp`) that
//! lists every compiled object (`*.c.o.export`, ~1200 of them, including mathlib) and the exact
//! runtime link flags. We reuse that proven-good recipe: bundle all objects (minus the one
//! defining Lean's `main`) together with our C shim into a single `libapc_optimizer_all.a` (a symbol-
//! indexed archive resolves the modules' mutual references), then replay the Lean runtime `-l`
//! flags — all wrapped in one `--start-group` so nothing is missed.
//!
//! apc-optimizer checkout resolution: by default this crate is self-contained — it maintains a managed
//! clone pinned to `APC_OPTIMIZER_REV` under a persistent cache dir, so no local checkout is required.
//! Set `APC_OPTIMIZER_DIR` to point at your own checkout instead (local optimizer development); its git
//! state is then used verbatim.

use std::path::{Path, PathBuf};
use std::process::Command;

/// Upstream apc-optimizer repository and the exact commit this crate builds against. To move to a newer
/// optimizer, bump `APC_OPTIMIZER_REV` here (and refresh the `lean-optimizer` snapshots as needed).
const APC_OPTIMIZER_REPO: &str = "https://github.com/powdr-labs/apc-optimizer.git";
const APC_OPTIMIZER_REV: &str = "e10b10262a8dcc5bc1f94d933ea043d0a5bc10fc";
/// The Lean executable target (`[[lean_exe]]` in apc-optimizer's `lakefile.toml`); its `.rsp` file carries
/// the object list + runtime link flags we replay. If apc-optimizer renames the exe, bump this.
const APC_OPTIMIZER_EXE: &str = "apc-optimizer";

fn run(cmd: &mut Command) -> String {
    let rendered = format!("{cmd:?}");
    let out = cmd
        .output()
        .unwrap_or_else(|e| panic!("failed to spawn {rendered}: {e}"));
    if !out.status.success() {
        panic!(
            "command failed ({}): {rendered}\nstdout:\n{}\nstderr:\n{}",
            out.status,
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr),
        );
    }
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Resolve the apc-optimizer checkout to build against.
///
/// * `APC_OPTIMIZER_DIR` set -> use it verbatim (local optimizer development; whatever is checked out
///   there is built).
/// * otherwise -> maintain a managed clone pinned to `APC_OPTIMIZER_REV`. It lives under a *persistent*
///   cache dir (`APC_OPTIMIZER_CACHE_DIR`, else `$CARGO_HOME/powdr-apc-optimizer`, else `$HOME/.cache/powdr-apc-optimizer`,
///   else `OUT_DIR`) rather than `OUT_DIR` itself, so the ~1200 compiled objects (mathlib) survive
///   `cargo clean` and aren't rebuilt from scratch every time.
fn resolve_apc_optimizer_dir(out_dir: &Path) -> PathBuf {
    if let Ok(dir) = std::env::var("APC_OPTIMIZER_DIR") {
        let dir = PathBuf::from(dir);
        return dir
            .canonicalize()
            .unwrap_or_else(|e| panic!("APC_OPTIMIZER_DIR {dir:?} not found: {e}"));
    }

    let cache_root = std::env::var("APC_OPTIMIZER_CACHE_DIR")
        .map(PathBuf::from)
        .or_else(|_| {
            std::env::var("CARGO_HOME").map(|h| PathBuf::from(h).join("powdr-apc-optimizer"))
        })
        .or_else(|_| {
            std::env::var("HOME").map(|h| PathBuf::from(h).join(".cache/powdr-apc-optimizer"))
        })
        .unwrap_or_else(|_| out_dir.to_path_buf());
    let checkout = cache_root.join("apc-optimizer");

    let git = |args: &[&str]| {
        let mut c = Command::new("git");
        c.arg("-C").arg(&checkout).args(args);
        c
    };

    if !checkout.join(".git").is_dir() {
        std::fs::create_dir_all(&cache_root).unwrap_or_else(|e| {
            panic!("cannot create apc-optimizer cache dir {cache_root:?}: {e}")
        });
        run(Command::new("git")
            .arg("clone")
            .arg(APC_OPTIMIZER_REPO)
            .arg(&checkout));
    }

    // Pin to the exact commit. Fetch only when the cache doesn't already contain it.
    if run(&mut git(&["rev-parse", "HEAD"])) != APC_OPTIMIZER_REV {
        let have_rev = git(&["cat-file", "-e", &format!("{APC_OPTIMIZER_REV}^{{commit}}")])
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false);
        if !have_rev {
            run(&mut git(&["fetch", "origin"]));
        }
        run(&mut git(&["checkout", "--detach", APC_OPTIMIZER_REV]));
    }

    checkout
        .canonicalize()
        .unwrap_or_else(|e| panic!("apc-optimizer checkout {checkout:?} not found: {e}"))
}

fn main() {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());

    let apc_optimizer_dir = resolve_apc_optimizer_dir(&out_dir);

    println!("cargo:rerun-if-env-changed=APC_OPTIMIZER_DIR");
    println!("cargo:rerun-if-env-changed=APC_OPTIMIZER_CACHE_DIR");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=c/ffi_shim.c");

    // --- Toolchain paths -----------------------------------------------------
    // Resolve the toolchain *from the apc-optimizer checkout* so we match its `lean-toolchain` pin (the
    // objects must be linked with the exact runtime they were compiled against).
    let prefix = PathBuf::from(run(Command::new("lean")
        .current_dir(&apc_optimizer_dir)
        .arg("--print-prefix")));
    let include_dir = prefix.join("include");
    let lib_lean = prefix.join("lib").join("lean");
    let lib = prefix.join("lib");
    let llvm_ar = prefix.join("bin").join("llvm-ar");
    // Compile the shim with the *system* C compiler: it has the system headers lean.h pulls in
    // (pthread.h, <stdatomic.h>), which the toolchain's bundled clang sysroot lacks. `-I` points
    // at the Lean include dir so `<lean/lean.h>` resolves.
    let cc = std::env::var("CC").unwrap_or_else(|_| "cc".to_string());

    // --- Build the Lean side (produces objects + the response file) ----------
    run(Command::new("lake")
        .current_dir(&apc_optimizer_dir)
        .arg("build")
        .arg(APC_OPTIMIZER_EXE));

    let rsp_path = apc_optimizer_dir.join(format!(".lake/build/bin/{APC_OPTIMIZER_EXE}.rsp"));
    let rsp = std::fs::read_to_string(&rsp_path)
        .unwrap_or_else(|e| panic!("cannot read {rsp_path:?}: {e}"));

    // Objects are one-per-line, double-quoted, ending in `.c.o.export`. Exclude the top-level
    // `Main.c.o.export` (it defines Lean's `main`, which would clash with Rust's).
    let main_obj = apc_optimizer_dir.join(".lake/build/ir/Main.c.o.export");
    let objects: Vec<PathBuf> = rsp
        .lines()
        .map(|l| l.trim().trim_matches('"'))
        .filter(|l| l.ends_with(".c.o.export"))
        .map(PathBuf::from)
        .filter(|p| p != &main_obj)
        .collect();
    assert!(
        objects.len() > 100,
        "expected many Lean objects in the response file, found {}",
        objects.len()
    );

    // --- Compile the C shim --------------------------------------------------
    let shim_obj = out_dir.join("ffi_shim.o");
    run(Command::new(&cc)
        .arg("-c")
        .arg("-O2")
        .arg("-fPIC")
        .arg("-I")
        .arg(&include_dir)
        .arg(manifest_dir.join("c/ffi_shim.c"))
        .arg("-o")
        .arg(&shim_obj));

    // --- Bundle EVERYTHING into one self-contained, symbol-indexed archive ---
    //
    // `cargo:rustc-link-arg` does NOT propagate to crates that depend on this one, and the Lean
    // runtime archives are mutually recursive (which would otherwise need `--start-group`). Both
    // problems vanish if we merge the shim, all compiled Lean/mathlib objects, and the entire Lean
    // runtime (leancpp/Lean/Std/Init/leanrt + static libc++/c++abi/unwind/gmp/uv) into a single
    // archive: a `.a` with a symbol index is resolved multi-pass by the linker, and pulling it in
    // via a propagating `rustc-link-lib=static` links only the members actually referenced (so it
    // coexists with Rust's own libunwind etc.).
    let archive = out_dir.join("libapc_optimizer_all.a");
    let _ = std::fs::remove_file(&archive);

    let target_os = std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();

    // The Lean runtime + gmp/uv ship as static archives on every platform and are merged here.
    // The C++ runtime differs: on Linux the toolchain provides static libc++/c++abi/unwind, so we
    // bundle them too; on macOS those live in the system SDK and are linked dynamically via
    // `-lc++` (see the sys-lib list below), so they must NOT be part of the merged archive.
    let mut runtime_libs = vec![
        lib_lean.join("libleancpp.a"),
        lib_lean.join("libLean.a"),
        lib_lean.join("libStd.a"),
        lib_lean.join("libInit.a"),
        lib_lean.join("libleanrt.a"),
        lib.join("libgmp.a"),
        lib.join("libuv.a"),
    ];
    if target_os != "macos" {
        runtime_libs.extend([
            lib.join("libc++.a"),
            lib.join("libc++abi.a"),
            lib.join("libunwind.a"),
        ]);
    }

    let mut mri = String::new();
    mri.push_str(&format!("CREATE {}\n", archive.display()));
    mri.push_str(&format!("ADDMOD {}\n", shim_obj.display()));
    for o in &objects {
        mri.push_str(&format!("ADDMOD {}\n", o.display()));
    }
    for l in &runtime_libs {
        assert!(l.exists(), "runtime archive missing: {l:?}");
        mri.push_str(&format!("ADDLIB {}\n", l.display()));
    }
    mri.push_str("SAVE\nEND\n");

    let mri_path = out_dir.join("apc_optimizer_all.mri");
    std::fs::write(&mri_path, &mri).unwrap();
    let out = Command::new(&llvm_ar)
        .arg("-M")
        .stdin(std::fs::File::open(&mri_path).unwrap())
        .output()
        .expect("failed to spawn llvm-ar -M");
    assert!(
        out.status.success(),
        "llvm-ar -M failed: {}\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr)
    );
    // Ensure a fresh symbol index over the merged members.
    run(Command::new(&llvm_ar).arg("s").arg(&archive));

    // --- Emit propagating link directives ------------------------------------
    println!("cargo:rustc-link-search=native={}", out_dir.display());
    println!("cargo:rustc-link-lib=static=apc_optimizer_all");
    // Remaining dependencies are the system dynamic libraries the Lean runtime needs. On macOS
    // libc++ (and, transitively, libc++abi + libunwind) come from the system SDK, while pthread/m
    // live in libSystem and there is no separate rt/dl.
    let sys_libs: &[&str] = if target_os == "macos" {
        &["c++"]
    } else {
        &["pthread", "dl", "rt", "m"]
    };
    for l in sys_libs {
        println!("cargo:rustc-link-lib=dylib={l}");
    }

    // Rerun if the Lean sources change.
    for sub in ["ApcOptimizer", "Main.lean", "lakefile.toml"] {
        let p: &Path = Path::new(&apc_optimizer_dir).as_ref();
        println!("cargo:rerun-if-changed={}", p.join(sub).display());
    }
}
