#!/bin/bash
#
# Run the nightly guest-benchmark suite (the `test_apc_guest` job in
# .github/workflows/nightly-tests.yml) locally, EXACTLY as nightly does, but
# with APC optimization routed through the Lean4 verified apc-optimizer.
#
# The only differences from nightly are the two Lean toggles below
# (`lean-optimizer` feature + POWDR_USE_LEAN_OPTIMIZER=1); every other env var
# is set to the same value nightly uses. Runnable without arguments.
#
# Results land in ./bench-results/results/<timestamp>-lean/ with a readme.md
# that reports how long APC generation took per benchmark.

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$REPO_ROOT"

# --- Environment, matching nightly's `env:` block (global + test_apc_guest) ---
export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export JEMALLOC_SYS_WITH_MALLOC_CONF="retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:10000,muzzy_decay_ms:10000,abort_conf:true"
export POWDR_OPENVM_SEGMENT_DELTA=50000

# --- Lean-optimizer toggles (the only thing that differs from nightly) --------
export POWDR_USE_LEAN_OPTIMIZER=1
export POWDR_BENCH_CARGO_FEATURES="metrics,lean-optimizer"
export POWDR_BENCH_TIME_APCS=1

# apc-optimizer revision this build links against (single source of truth is
# the FFI build script), surfaced in the readme.
APC_REV=$(sed -n 's/^const APC_OPTIMIZER_REV: &str = "\(.*\)";/\1/p' autoprecompiles-lean-ffi/build.rs)

banner() { printf '\n\033[1m\033[35m=== %s ===\033[0m\n\n' "$1"; }

banner "Preflight"
for tool in lean lake cargo python3; do
    command -v "$tool" >/dev/null || { echo "error: '$tool' not found on PATH (needed to build/run the Lean optimizer)"; exit 1; }
done
echo "powdr:         $(git rev-parse HEAD)"
echo "apc-optimizer: ${APC_REV}"

banner "Python venv"
[ -d .venv ] || python3 -m venv .venv
# shellcheck disable=SC1091
source .venv/bin/activate
pip install -q -r openvm-riscv/scripts/requirements.txt
pip install -q -r autoprecompiles/scripts/requirements.txt

banner "Running guest benchmarks (Lean optimizer)"
rm -rf results
mkdir -p results
bash ./openvm-riscv/scripts/run_guest_benches.sh

banner "Publishing results"
DATE=$(date +'%Y-%m-%d-%H%M')-lean
OUT_DIR="bench-results/results/${DATE}"
mkdir -p "$OUT_DIR"
cp -r results/. "$OUT_DIR/"

{
    echo "powdr: $(git rev-parse HEAD)"
    echo "apc-optimizer: ${APC_REV}"
    echo "lean-optimizer: enabled (POWDR_USE_LEAN_OPTIMIZER=1)"
} > "$OUT_DIR/run.txt"

python3 ./openvm-riscv/scripts/generate_lean_bench_readme.py \
    "$OUT_DIR" "$DATE" \
    --powdr-sha "$(git rev-parse HEAD)" \
    --apc-rev "${APC_REV}" \
    --output "$OUT_DIR/readme.md"

banner "Done"
echo "Results:  ${OUT_DIR}"
echo "Readme:   ${OUT_DIR}/readme.md"
echo ""
sed -n '/## APC generation time per benchmark/,$p' "$OUT_DIR/readme.md"
