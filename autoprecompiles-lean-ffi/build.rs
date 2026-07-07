//! Build script: compile the Leanr optimizer + C shim into one static archive and link it (plus
//! the Lean runtime) into the consuming Rust binary.
//!
//! Strategy (see the crate docs and the Task-1 PR): `lake build` in the Leanr checkout produces a
//! native executable and, crucially, a linker response file (`.lake/build/bin/leanr.rsp`) that
//! lists every compiled object (`*.c.o.export`, ~1200 of them, including mathlib) and the exact
//! runtime link flags. We reuse that proven-good recipe: bundle all objects (minus the one
//! defining Lean's `main`) together with our C shim into a single `libleanr_all.a` (a symbol-
//! indexed archive resolves the modules' mutual references), then replay the Lean runtime `-l`
//! flags — all wrapped in one `--start-group` so nothing is missed.
//!
//! Set `LEANR_DIR` to the Leanr checkout (defaults to `../leanr`).

use std::path::{Path, PathBuf};
use std::process::Command;

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

fn main() {
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    let manifest_dir = PathBuf::from(std::env::var("CARGO_MANIFEST_DIR").unwrap());

    let leanr_dir = std::env::var("LEANR_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| manifest_dir.join("..").join("..").join("leanr"));
    let leanr_dir = leanr_dir
        .canonicalize()
        .unwrap_or_else(|e| panic!("LEANR_DIR {leanr_dir:?} not found: {e}"));

    println!("cargo:rerun-if-env-changed=LEANR_DIR");
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=c/ffi_shim.c");

    // --- Toolchain paths -----------------------------------------------------
    // Resolve the toolchain *from the Leanr checkout* so we match its `lean-toolchain` pin (the
    // objects must be linked with the exact runtime they were compiled against).
    let prefix = PathBuf::from(run(Command::new("lean")
        .current_dir(&leanr_dir)
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
    run(Command::new("lake").current_dir(&leanr_dir).arg("build"));

    let rsp_path = leanr_dir.join(".lake/build/bin/leanr.rsp");
    let rsp = std::fs::read_to_string(&rsp_path)
        .unwrap_or_else(|e| panic!("cannot read {rsp_path:?}: {e}"));

    // Objects are one-per-line, double-quoted, ending in `.c.o.export`. Exclude the top-level
    // `Main.c.o.export` (it defines Lean's `main`, which would clash with Rust's).
    let main_obj = leanr_dir.join(".lake/build/ir/Main.c.o.export");
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
    let archive = out_dir.join("libleanr_all.a");
    let _ = std::fs::remove_file(&archive);

    let runtime_libs = [
        lib_lean.join("libleancpp.a"),
        lib_lean.join("libLean.a"),
        lib_lean.join("libStd.a"),
        lib_lean.join("libInit.a"),
        lib_lean.join("libleanrt.a"),
        lib.join("libc++.a"),
        lib.join("libc++abi.a"),
        lib.join("libunwind.a"),
        lib.join("libgmp.a"),
        lib.join("libuv.a"),
    ];

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

    let mri_path = out_dir.join("leanr_all.mri");
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
    println!("cargo:rustc-link-lib=static=leanr_all");
    // Remaining dependencies are the system dynamic libraries the Lean runtime needs.
    for l in ["pthread", "dl", "rt", "m"] {
        println!("cargo:rustc-link-lib=dylib={l}");
    }

    // Rerun if the Lean sources change.
    for sub in ["Leanr", "Main.lean", "lakefile.toml"] {
        let p: &Path = Path::new(&leanr_dir).as_ref();
        println!("cargo:rerun-if-changed={}", p.join(sub).display());
    }
}
