#!/bin/bash
#
# Benchmark the native Rust APC optimizer against the Lean apc-optimizer.
#
# Two phases, mirroring the nightly APC pipeline (same env, same Cell PGO, same
# Lean apc-optimizer wiring):
#
#   Phase 1 (dump): run `generate-apcs` for every benchmark with
#   `POWDR_APC_DUMP_DIR` set. In that mode `powdr_autoprecompiles::build`
#   serializes each candidate's *pre-optimization* input circuit to a `.cbor`
#   file and skips optimization, so the dump is cheap. One subdirectory of dumps
#   is produced per benchmark.
#
#   Phase 2 (time): `time-optimizers` reads every dumped circuit across all
#   benchmarks and re-runs BOTH optimizers on each one, recording the input
#   circuit's size (variables / constraints / bus interactions) and each
#   optimizer's runtime. It makes one global rayon pass per optimizer, so only
#   one kind of work is ever in flight, at a fixed `--threads` (default 24).
#   Runtimes are measured under that load, so they are comparable within a
#   report but not across reports; see the module docs in
#   `openvm/src/optimizer_timing.rs`. Timing all benchmarks in one pass keeps
#   cores busy: the few large circuits that dominate any one benchmark run
#   alongside the many small circuits of the others.
#
# The result is a JSON report:
#   {"openvm-eth-version": "<hash>", "parallelism": <threads>, "benchmarks": [
#       {"name": ..., "apcs": [{"constraints", "variables", "bus_interactions",
#                               "rust_runtime", "lean_runtime"}, ...]}]}
#
# Prerequisites (same as the nightly `test_apc_guest` / `test_apc_reth` jobs):
#   * `cargo openvm` + the OpenVM guest toolchain installed.
#   * `lean`/`lake` (elan) on PATH — the `lean-optimizer` feature links the Lean
#     apc-optimizer via FFI.
#   * `APC_OPTIMIZER_REV` optionally set to pin the apc-optimizer commit (nightly
#     pins the latest `main`); defaults to the version vendored in
#     `autoprecompiles-lean-ffi/build.rs`.
#
# Env knobs:
#   DUMP_DIR       where to write input-circuit dumps (default: ./apc-timing/dump)
#   OUT_JSON       final report path (default: ./apc-timing/apc_optimizer_timing.json)
#   OPENVM_ETH_DIR path to a checked-out openvm-eth (default: ./openvm-eth if present)
#   RETH_BLOCK     block number for the openvm-eth benchmark (default: 24171377)
#   SKIP_BUILD=1   reuse an existing release binary instead of rebuilding

set -euo pipefail

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")
REPO_ROOT=$(realpath "$SCRIPTS_DIR/../..")
cd "$REPO_ROOT"

DUMP_DIR="${DUMP_DIR:-$REPO_ROOT/apc-timing/dump}"
OUT_JSON="${OUT_JSON:-$REPO_ROOT/apc-timing/apc_optimizer_timing.json}"
OPENVM_ETH_DIR="${OPENVM_ETH_DIR:-$REPO_ROOT/openvm-eth}"
RETH_BLOCK="${RETH_BLOCK:-24171377}"
FEATURES="metrics,lean-optimizer"
BIN="$REPO_ROOT/target/release/powdr_openvm_riscv"

# Nightly environment (see .github/workflows/nightly-tests.yml).
export JEMALLOC_SYS_WITH_MALLOC_CONF="retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:10000,muzzy_decay_ms:10000,abort_conf:true"
export POWDR_OPENVM_SEGMENT_DELTA="${POWDR_OPENVM_SEGMENT_DELTA:-50000}"
# The Lean apc-optimizer recurses deeply; give worker threads a 512 MiB stack.
export RUST_MIN_STACK="${RUST_MIN_STACK:-536870912}"
export RUST_BACKTRACE="${RUST_BACKTRACE:-1}"

# STARK proving is not run here, but generation still allocates a lot of virtual
# memory under jemalloc `retain:true`; lift any vmem ulimit if we can.
ulimit -v unlimited 2>/dev/null || true

mkdir -p "$DUMP_DIR" "$(dirname "$OUT_JSON")"

# Guest benchmarks: "<result-name> <guest-crate> <profile-input>". Matches the
# APC-enabled guests swept by run_guest_benches.sh (manual-precompile variants
# generate no APCs, so they're excluded).
GUESTS=(
    "keccak         guest-keccak                10000"
    "sha256         guest-sha256                30000"
    "pairing        guest-pairing               0"
    "u256           guest-u256                  0"
    "matmul         guest-matmul                0"
    "ecc-projective guest-ecc-projective        50"
    "ecc-affine     guest-ecc-powdr-affine-hint 50"
    "ecrecover      guest-ecrecover             20"
)

if [[ "${SKIP_BUILD:-0}" != "1" ]]; then
    echo "==== building powdr_openvm_riscv ($FEATURES) ===="
    cargo build --release -p cli-openvm-riscv --bin powdr_openvm_riscv --features "$FEATURES"
fi

echo "==== phase 1: dumping input circuits ===="
for entry in "${GUESTS[@]}"; do
    read -r name guest input <<<"$entry"
    echo "---- dump $name ($guest, profile-input=$input) ----"
    rm -rf "${DUMP_DIR:?}/$name"
    # No --artifacts-dir: the generate-stage cache key ignores POWDR_APC_DUMP_DIR,
    # so a cache hit would skip `build` (and thus the dump). Running uncached
    # guarantees every candidate is built and dumped.
    POWDR_APC_DUMP_DIR="$DUMP_DIR/$name" \
        "$BIN" generate-apcs "$guest" --profile-input "$input"
    echo "   $(find "$DUMP_DIR/$name" -name '*.cbor' | wc -l) circuits dumped"
done

# openvm-eth (reth) benchmark: dump the input circuits produced by a real block.
# Uses Cell PGO (--apc > 0) so the dumped candidates are dropped cleanly. The
# apc-cache is wiped first so `generate` actually re-runs `build`.
OPENVM_ETH_VERSION="unavailable"
if [[ -d "$OPENVM_ETH_DIR" && -f "$OPENVM_ETH_DIR/run.sh" ]]; then
    echo "---- dump openvm-eth (block $RETH_BLOCK) ----"
    OPENVM_ETH_VERSION=$(git -C "$OPENVM_ETH_DIR" rev-parse HEAD 2>/dev/null || echo "unavailable")
    rm -rf "${DUMP_DIR:?}/openvm-eth" "$OPENVM_ETH_DIR/apc-cache"
    (
        cd "$OPENVM_ETH_DIR"
        POWDR_APC_DUMP_DIR="$DUMP_DIR/openvm-eth" \
            ./run.sh --mode compile --apc 30 --block "$RETH_BLOCK"
    ) || echo "   (openvm-eth compile exited non-zero after dumping; continuing)"
    echo "   $(find "$DUMP_DIR/openvm-eth" -name '*.cbor' 2>/dev/null | wc -l) circuits dumped"
else
    echo "---- skipping openvm-eth (no $OPENVM_ETH_DIR) ----"
fi

echo "==== phase 2: timing both optimizers over all circuits ===="
BENCH_JSON="$DUMP_DIR/../benchmarks.json"
"$BIN" time-optimizers "$DUMP_DIR" --output "$BENCH_JSON"

echo "==== assembling $OUT_JSON ===="
POWDR_VERSION=$(git -C "$REPO_ROOT" rev-parse HEAD)
APC_OPTIMIZER_REV="${APC_OPTIMIZER_REV:-vendored-default}"
python3 - "$BENCH_JSON" "$OUT_JSON" "$OPENVM_ETH_VERSION" "$POWDR_VERSION" "$APC_OPTIMIZER_REV" <<'PY'
import json, sys
bench_path, out_path, openvm_eth, powdr, apc_opt = sys.argv[1:6]
with open(bench_path) as f:
    data = json.load(f)
report = {
    "openvm-eth-version": openvm_eth,
    "powdr-version": powdr,
    "apc-optimizer-version": apc_opt,
    # Carries the measurement conditions through: how many threads the runtimes were measured
    # under, which is what makes them reproducible.
    "parallelism": data.get("parallelism"),
    "benchmarks": data["benchmarks"],
}
with open(out_path, "w") as f:
    json.dump(report, f, indent=2)
n = sum(len(b["apcs"]) for b in report["benchmarks"])
print(f"  {len(report['benchmarks'])} benchmarks, {n} circuits -> {out_path}")
PY

echo "==== done ===="
