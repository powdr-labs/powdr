#!/bin/bash

# Reth (openvm-eth) APC sweep on CPU, mirroring the nightly `test_apc_reth`
# CPU side and the GPU `run_reth_gpu_bench.sh`, but on CPU and over a
# configurable list of APC counts. This is the script behind the
# `bench-results-cpu/` reth numbers.
#
# Run from the powdr repo root. Expects:
#   - `cargo openvm` installed (see .github/workflows/nightly-tests.yml),
#   - the python venv with openvm-riscv/scripts/requirements.txt +
#     autoprecompiles/scripts/requirements.txt + psrecord activated,
#   - RPC_1 set (env or openvm-eth/.env) pointing at an Ethereum mainnet
#     archive RPC that supports eth_getProof at the target block. Only the
#     first run needs it: block data is cached in openvm-eth/rpc-cache/.
#
# This machine has no GPU, so openvm-eth's run.sh auto-selects the CPU
# (non-cuda) build. Each prove-stark is wrapped in psrecord so we capture
# peak *host* RAM per APC count (the CPU analogue of the GPU run's
# gpu_memory_usage_*.csv). Headline timings come from the binary's own
# metrics.json, not from psrecord's wall clock.
#
# Results land in results/reth/, in the same shape the nightly publishes to
# the bench-results repo. Prove failures (e.g. recursion layout caps at high
# APC counts) don't abort the sweep; they're recorded in
# results/reth/failed_runs.txt. High APC counts that need raised aggregation
# caps (--leaf-log-stacked-height / --internal-log-stacked-height) are run
# separately; see bench-results-cpu/readme.md.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")
REPO_ROOT=$(realpath "$SCRIPTS_DIR/../..")

# Same ref as .github/actions/patch-openvm-eth/action.yml.
OPENVM_ETH_REF="${OPENVM_ETH_REF:-44564802f942d15c6dd782979d280f28065179b5}"
# Same block as the nightly reth benchmarks.
BLOCK_NUMBER="${BLOCK_NUMBER:-24171377}"
# Core sweep. High counts (500/1000) need raised aggregation caps and are run
# by hand afterwards (see the readme), so they're not in the default list.
APC_COUNTS=(${APC_COUNTS:-0 3 10 30 100 300})
# Same segmentation tweak the nightly sets at workflow level.
export POWDR_OPENVM_SEGMENT_DELTA="${POWDR_OPENVM_SEGMENT_DELTA:-50000}"

cd "$REPO_ROOT"

# ---------- openvm-eth checkout at the pinned ref, patched to local powdr ----------
if [ ! -d openvm-eth ]; then
    git clone https://github.com/powdr-labs/openvm-eth.git openvm-eth
fi
git -C openvm-eth checkout "$OPENVM_ETH_REF"
# Append the local-powdr patch only when absent so reruns don't clobber a
# patched checkout (the openvm-eth-bench.patch wiring etc.).
if ! grep -q 'patch."https://github.com/powdr-labs/powdr.git"' openvm-eth/.cargo/config.toml 2>/dev/null; then
    mkdir -p openvm-eth/.cargo
    cat >> openvm-eth/.cargo/config.toml <<'EOF'
[patch."https://github.com/powdr-labs/powdr.git"]
powdr-openvm-riscv = { path = "../openvm-riscv" }
powdr-openvm = { path = "../openvm" }
powdr-riscv-elf = { path = "../riscv-elf" }
powdr-number = { path = "../number" }
powdr-autoprecompiles = { path = "../autoprecompiles" }
powdr-openvm-riscv-hints-circuit = { path = "../openvm-riscv/extensions/hints-circuit" }
EOF
fi

cd openvm-eth
RES_DIR=reth
mkdir -p "$RES_DIR"

# ---------- Prefetch RPC cache ----------
# `--mode execute` populates rpc-cache/ with the block data; later runs load
# from the cache and never touch the RPC again.
./run.sh --block "$BLOCK_NUMBER" --apc 0 --mode execute || exit 1

# ---------- Compile APC caches (also builds the CPU binary once) ----------
# Under the default cell PGO the `generate` blob (build + rank every
# candidate) is keyed independently of --apc, so it's built once and reused;
# only the cheap per-N select+setup blobs accumulate in apc-cache/. Doing
# this in a separate phase keeps the prove-stark metrics clean.
for apc in "${APC_COUNTS[@]}"; do
    ./run.sh --block "$BLOCK_NUMBER" --apc "$apc" --mode compile || exit 1
done

# ---------- Prove sweep (CPU) ----------
# The binary was built by the compile phase; skip rebuilds so psrecord's peak
# memory reflects proving only, not a cargo no-op + linker.
export OPENVM_BENCH_SKIP_BUILD=1
for apc in "${APC_COUNTS[@]}"; do
    label=$(printf 'apc%03d' "$apc")
    rm -f metrics.json
    # psrecord doesn't reliably propagate the wrapped command's exit code, so
    # judge success by the metrics.json the prove writes.
    psrecord --include-children --interval 1 \
        --log "$RES_DIR/psrecord_${label}.csv" --log-format csv \
        --plot "$RES_DIR/psrecord_${label}.png" \
        "./run.sh --block $BLOCK_NUMBER --apc $apc --mode prove-stark" || true
    if [ -s metrics.json ]; then
        echo "Finished proving with $apc APCs"
        mv metrics.json "$RES_DIR/${label}.json"
        python3 ../openvm-riscv/scripts/plot_trace_cells.py -o "$RES_DIR/trace_cells_${label}.png" "$RES_DIR/${label}.json" > "$RES_DIR/trace_cells_${label}.txt"
    else
        echo "PROVE FAILED with $apc APCs (see above); continuing with the next count"
        echo "$apc" >> "$RES_DIR/failed_runs.txt"
        rm -f metrics.json
    fi
done

# The APC candidates are the same for all runs (cell PGO builds the full
# ranking once), so just keep the combined dump and drop the per-block
# snapshot files. Use find: the glob expansion exceeds ARG_MAX for reth's
# ~11k candidate blocks. NOTE: the apc_candidates.json dump only exists when
# openvm-eth-bench.patch (next to this script) is applied — it wires
# POWDR_APC_CANDIDATES_DIR into powdr's GenerateConfig.
if [ -f apcs/apc_candidates.json ]; then
    mv apcs/apc_candidates.json "$RES_DIR/apc_candidates.json"
fi
find apcs -maxdepth 1 -name 'apc_candidate_*' -delete

OK_JSONS=()
for apc in "${APC_COUNTS[@]}"; do
    label=$(printf 'apc%03d' "$apc")
    [ -f "$RES_DIR/${label}.json" ] && OK_JSONS+=("$RES_DIR/${label}.json")
done

python3 ../openvm-riscv/scripts/basic_metrics.py summary-table --csv "${OK_JSONS[@]}" > "$RES_DIR/basic_metrics.csv"
python3 ../openvm-riscv/scripts/basic_metrics.py plot "${OK_JSONS[@]}" -o "$RES_DIR/proof_time_breakdown.png"
python3 ../openvm-riscv/scripts/basic_metrics.py combine "${OK_JSONS[@]}" > "$RES_DIR/combined_metrics.json"
if [ -f "$RES_DIR/apc_candidates.json" ]; then
    python3 ../autoprecompiles/scripts/plot_effectiveness.py "$RES_DIR/apc_candidates.json" --output "$RES_DIR/effectiveness.png"
fi

mkdir -p ../results
rm -rf ../results/reth
mv "$RES_DIR" ../results/
