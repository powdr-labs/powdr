#!/usr/bin/env bash
#
# Bump the pinned apc-optimizer (Lean) revision to the latest `main` and
# re-record every snapshot test through the Lean optimizer.
#
# Run this whenever the apc-optimizer is updated: it rewrites the
# `APC_OPTIMIZER_REV` pin in `autoprecompiles-lean-ffi/build.rs` and regenerates
# every snapshot fixture that depends on the optimizer output so CI, which runs
# the test suite with the Lean optimizer enabled, stays green.
#
# The snapshot tests come in two flavours, both re-recorded via `UPDATE_EXPECT=1`:
#   * `.txt` fixtures (openvm-riscv `apc_snapshots`, plus a few files under
#     `autoprecompiles/tests/`)
#   * inline `expect![[ ... ]]` snapshots rewritten in the Rust sources
#     (autoprecompiles optimizer tests + openvm-riscv `*_machine_*` lib tests)
#
# Requires the Lean toolchain (elan: `lean`/`lake`) on PATH, and — for the
# openvm-riscv `*_machine_*` tests, which compile guest programs — the OpenVM
# guest toolchain (`cargo openvm build`).

set -euo pipefail

cd "$(dirname "$0")"

BUILD_RS="autoprecompiles-lean-ffi/build.rs"

# Resolve the latest apc-optimizer main commit and pin build.rs to it.
REV=$(git ls-remote https://github.com/powdr-labs/apc-optimizer.git refs/heads/main | cut -f1)
echo "Pinning apc-optimizer to latest main: $REV"
perl -i -pe "s/(const APC_OPTIMIZER_REV: &str = \")[0-9a-f]+(\";)/\${1}${REV}\${2}/" "$BUILD_RS"
grep "APC_OPTIMIZER_REV: &str" "$BUILD_RS"

# Re-record all snapshots through the Lean optimizer.
#  - lean-optimizer feature: links the Lean apc-optimizer via FFI
#  - POWDR_USE_LEAN_OPTIMIZER: selects it at runtime
#  - UPDATE_EXPECT: overwrite the snapshot fixtures / inline expects
#  - RUST_MIN_STACK: the Lean optimizer recurses deeply; give threads a big stack
# Use `cargo test` (not nextest) so `expect_test`'s inline-snapshot rewriting
# works reliably.
export POWDR_USE_LEAN_OPTIMIZER=1
export UPDATE_EXPECT=1
export RUST_MIN_STACK=536870912

echo "Recording autoprecompiles optimizer snapshots..."
cargo test --release -p powdr-autoprecompiles --features lean-optimizer --test optimizer

echo "Recording openvm-riscv apc_builder snapshots..."
cargo test --release -p powdr-openvm-riscv --features lean-optimizer \
    --test apc_builder_single_instructions \
    --test apc_builder_pseudo_instructions \
    --test apc_builder_complex \
    --test apc_builder_superblocks

echo "Recording openvm-riscv machine-metric snapshots (compiles guests)..."
cargo test --release -p powdr-openvm-riscv --features lean-optimizer --lib -- machine

# NOTE: sp1-benchmarks is its own excluded workspace and its CI job runs on the
# native optimizer (not Lean), so its snapshots are intentionally not recorded
# here.

echo "Done. Review the snapshot diffs before committing."
