#!/bin/bash
#
# Local equivalent of the Lean nightly: runs the `test_apc_guest` and
# `test_apc_reth` (CPU) jobs from .github/workflows/nightly-tests.yml
# sequentially on this machine, with APC optimization routed through the Lean4
# apc-optimizer. This is a faithful copy of those two jobs' steps — keep it in
# sync with the workflow. Runnable without arguments (needs RPC_1 for reth).
#
# Results land in ./bench-results/results/<timestamp>-lean/ with the same
# readme the nightly publish job produces (plus the Lean note + timing table).

set -euo pipefail

REPO_ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
cd "$REPO_ROOT"

# --- Workflow `env:` block + the branch's Lean toggles (hardcoded on here) ----
export RUST_MIN_STACK=536870912
export CARGO_TERM_COLOR=always
export RUST_BACKTRACE=1
export JEMALLOC_SYS_WITH_MALLOC_CONF="retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:10000,muzzy_decay_ms:10000,abort_conf:true"
export POWDR_OPENVM_SEGMENT_DELTA=50000
export POWDR_USE_LEAN_OPTIMIZER=1
export POWDR_BENCH_CARGO_FEATURES="metrics,lean-optimizer"
export POWDR_BENCH_TIME_APCS=1
export BLOCK_NUMBER="${BLOCK_NUMBER:-24171377}"                       # test_apc_reth env
OPENVM_ETH_REF=$(awk '/^[[:space:]]*ref:/ {print $2; exit}' .github/actions/patch-openvm-eth/action.yml)
APC_REV=$(sed -n 's/^const APC_OPTIMIZER_REV: &str = "\(.*\)";/\1/p' autoprecompiles-lean-ffi/build.rs)

banner() { printf '\n\033[1m\033[35m=== %s ===\033[0m\n\n' "$1"; }

banner "Preflight"
for tool in lean lake cargo cargo-openvm python3 git perl; do
    command -v "$tool" >/dev/null || { echo "error: '$tool' not found on PATH"; exit 1; }
done
if [ -z "${RPC_1:-}" ] && ! grep -q 'RPC_1' openvm-eth/.env 2>/dev/null; then
    echo "error: the reth benchmark needs an RPC endpoint. Set RPC_1 in the environment or in openvm-eth/.env." >&2
    exit 1
fi
echo "powdr:         $(git rev-parse HEAD)"
echo "apc-optimizer: ${APC_REV}"
echo "openvm-eth:    ${OPENVM_ETH_REF}"
echo "block:         ${BLOCK_NUMBER}"

banner "Setup python venv"
[ -d .venv ] || python3 -m venv .venv
# shellcheck disable=SC1091
source .venv/bin/activate
pip install -q -r openvm-riscv/scripts/requirements.txt
pip install -q -r autoprecompiles/scripts/requirements.txt

# Both jobs share this results/ dir locally (in CI they're separate runners).
rm -rf results
mkdir -p results

# ============================ Job: test_apc_guest ============================
banner "test_apc_guest (Lean)"
bash ./openvm-riscv/scripts/run_guest_benches.sh

# ============================ Job: test_apc_reth =============================
banner "test_apc_reth CPU (Lean)"

# Patch benchmark (mirrors .github/actions/patch-openvm-eth)
if [ ! -d openvm-eth/.git ]; then
    git clone https://github.com/powdr-labs/openvm-eth.git openvm-eth
fi
git -C openvm-eth cat-file -e "${OPENVM_ETH_REF}^{commit}" 2>/dev/null || git -C openvm-eth fetch --quiet origin
git -C openvm-eth checkout -f --quiet --detach "$OPENVM_ETH_REF"
mkdir -p openvm-eth/.cargo
cat > openvm-eth/.cargo/config.toml <<'EOF'
[patch."https://github.com/powdr-labs/powdr.git"]
powdr-openvm-riscv = { path = "../openvm-riscv" }
powdr-openvm = { path = "../openvm" }
powdr-riscv-elf = { path = "../riscv-elf" }
powdr-number = { path = "../number" }
powdr-autoprecompiles = { path = "../autoprecompiles" }
powdr-openvm-riscv-hints-circuit = { path = "../openvm-riscv/extensions/hints-circuit" }
EOF

# Enable the Lean optimizer in openvm-eth's build (mirrors the workflow step).
perl -i -pe 's{^FEATURES="parallel,metrics,jemalloc,unprotected"$}{FEATURES="parallel,metrics,jemalloc,unprotected,powdr-openvm/lean-optimizer,powdr-openvm-riscv/lean-optimizer"}' openvm-eth/run.sh
grep -q 'lean-optimizer' openvm-eth/run.sh || { echo "failed to patch openvm-eth FEATURES for lean-optimizer"; exit 1; }

cd openvm-eth
[ -n "${RPC_1:-}" ] && echo "export RPC_1=${RPC_1}" >> .env

# Prefetch RPC cache (fresh APC cache), then compile every apc count we prove.
rm -rf apc-cache
./run.sh --apc 0 --block "$BLOCK_NUMBER" --mode execute
for apc in 0 3 10 30 100; do
    ./run.sh --apc "$apc" --block "$BLOCK_NUMBER" --mode compile
done

# Run reth benchmark (verbatim from the workflow's CPU prove loop).
RES_DIR=reth
mkdir -p $RES_DIR

./run.sh --block "$BLOCK_NUMBER" --apc 0 --mode prove-stark
mv metrics.json $RES_DIR/apc000.json
python ../openvm-riscv/scripts/plot_trace_cells.py -o $RES_DIR/trace_cells_apc000.png $RES_DIR/apc000.json > $RES_DIR/trace_cells_apc000.txt

./run.sh --block "$BLOCK_NUMBER" --apc 3 --mode prove-stark
mv metrics.json $RES_DIR/apc003.json
python ../openvm-riscv/scripts/plot_trace_cells.py -o $RES_DIR/trace_cells_apc003.png $RES_DIR/apc003.json > $RES_DIR/trace_cells_apc003.txt

./run.sh --block "$BLOCK_NUMBER" --apc 10 --mode prove-stark
mv metrics.json $RES_DIR/apc010.json
python ../openvm-riscv/scripts/plot_trace_cells.py -o $RES_DIR/trace_cells_apc010.png $RES_DIR/apc010.json > $RES_DIR/trace_cells_apc010.txt

./run.sh --block "$BLOCK_NUMBER" --apc 30 --mode prove-stark
mv metrics.json $RES_DIR/apc030.json
python ../openvm-riscv/scripts/plot_trace_cells.py -o $RES_DIR/trace_cells_apc030.png $RES_DIR/apc030.json > $RES_DIR/trace_cells_apc030.txt

# prove with 100 APCs, recording mem usage
psrecord --include-children --interval 1 --log $RES_DIR/psrecord.csv --log-format csv --plot $RES_DIR/psrecord.png "./run.sh --block $BLOCK_NUMBER --apc 100 --mode prove-stark"
mv metrics.json $RES_DIR/apc100.json
python ../openvm-riscv/scripts/plot_trace_cells.py -o $RES_DIR/trace_cells_apc100.png $RES_DIR/apc100.json > $RES_DIR/trace_cells_apc100.txt

mv apcs/apc_candidates.json $RES_DIR/apc_candidates.json
python ../openvm-riscv/scripts/basic_metrics.py summary-table --csv $RES_DIR/apc000.json $RES_DIR/apc003.json $RES_DIR/apc010.json $RES_DIR/apc030.json $RES_DIR/apc100.json > $RES_DIR/basic_metrics.csv
python ../openvm-riscv/scripts/basic_metrics.py plot $RES_DIR/apc000.json $RES_DIR/apc003.json $RES_DIR/apc010.json $RES_DIR/apc030.json $RES_DIR/apc100.json -o $RES_DIR/proof_time_breakdown.png
python ../openvm-riscv/scripts/basic_metrics.py combine $RES_DIR/apc000.json $RES_DIR/apc003.json $RES_DIR/apc010.json $RES_DIR/apc030.json $RES_DIR/apc100.json > $RES_DIR/combined_metrics.json
python ../autoprecompiles/scripts/plot_effectiveness.py $RES_DIR/apc_candidates.json --output $RES_DIR/effectiveness.png

mkdir -p ../results
rm -rf ../results/reth
mv $RES_DIR ../results/
cd "$REPO_ROOT"

# ========================= Publish (publish_bench_results) ===================
banner "Publishing results"
DATE=$(date +'%Y-%m-%d-%H%M')-lean
OUT_DIR="bench-results/results/${DATE}"
mkdir -p "$OUT_DIR"
cp -r results/. "$OUT_DIR/"
{
    echo "powdr: $(git rev-parse HEAD)"
    echo "openvm-eth: ${OPENVM_ETH_REF}"
    echo "apc-optimizer: ${APC_REV}"
    echo "lean-optimizer: enabled (POWDR_USE_LEAN_OPTIMIZER=1)"
} > "$OUT_DIR/run.txt"
python3 ./openvm-riscv/scripts/generate_bench_results_readme.py \
    "$OUT_DIR" "$DATE" \
    --note "Identical to the nightly benchmarks, but APC optimization is routed through the Lean4 verified [apc-optimizer](https://github.com/powdr-labs/apc-optimizer) via FFI (POWDR_USE_LEAN_OPTIMIZER=1) instead of the native Rust optimizer. Proving is unchanged; only APC generation differs." \
    --output "$OUT_DIR/readme.md"

# Push to the bench-results repo (like the workflow's publish job). Requires the
# local bench-results/ checkout to have a writable `origin` on the gh-pages branch.
banner "Pushing to bench-results"
if git -C bench-results rev-parse --is-inside-work-tree >/dev/null 2>&1; then
    git -C bench-results add "results/${DATE}"
    git -C bench-results commit -q -m "Add nightly results for ${DATE}"
    git -C bench-results push origin gh-pages
    echo "Pushed results/${DATE} to bench-results."
else
    echo "warning: bench-results/ is not a git checkout; skipped push (results are in ${OUT_DIR})."
fi

banner "Done"
echo "Results: ${OUT_DIR}"
echo "Readme:  ${OUT_DIR}/readme.md"
