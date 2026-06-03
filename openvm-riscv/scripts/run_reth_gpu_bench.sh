#!/bin/bash

# Reth (openvm-eth) APC sweep on GPU, mirroring the nightly `test_apc_gpu`
# job but over a configurable list of APC counts (including high counts to
# probe the limit).
#
# Run from the powdr repo root. Expects:
#   - an NVIDIA GPU (run.sh builds openvm-eth with the `cuda` feature),
#   - `cargo openvm` installed (see .github/workflows/nightly-tests.yml),
#   - the python venv with openvm-riscv/scripts/requirements.txt +
#     autoprecompiles/scripts/requirements.txt activated,
#   - RPC_1 set (env or openvm-eth/.env) pointing at an Ethereum mainnet
#     archive RPC that supports eth_getProof at the target block. Only the
#     first run needs it: block data is cached in openvm-eth/rpc-cache/.
#     Free-tier endpoints (e.g. https://eth.drpc.org) drop the occasional
#     heavy call during the ~1h witness fetch; for those, apply
#     openvm-eth-bench.patch (next to this script) to openvm-eth and
#     export RPC_PROOF_CHUNK_SIZE=100. The patch hardens the fetch
#     (retries + smaller eth_getProof chunks) and also wires
#     POWDR_APC_CANDIDATES_DIR into powdr's GenerateConfig so the
#     apc_candidates.json dump (APC analyzer input) gets written;
#     proving itself is unaffected.
#
# Results land in results/reth_gpu/, in the same shape the nightly publishes
# to the bench-results repo. Prove failures (e.g. OOM at high APC counts)
# don't abort the sweep; they're recorded in results/reth_gpu/failed_runs.txt.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")
REPO_ROOT=$(realpath "$SCRIPTS_DIR/../..")

# Same ref as .github/actions/patch-openvm-eth/action.yml.
OPENVM_ETH_REF="${OPENVM_ETH_REF:-44564802f942d15c6dd782979d280f28065179b5}"
# Same block as the nightly reth benchmarks.
BLOCK_NUMBER="${BLOCK_NUMBER:-24171377}"
# Sweep including high counts to probe where proving stops working.
APC_COUNTS=(${APC_COUNTS:-0 3 10 30 100 300 500 1000})
# Same segmentation tweak the nightly sets at workflow level.
export POWDR_OPENVM_SEGMENT_DELTA="${POWDR_OPENVM_SEGMENT_DELTA:-50000}"

cd "$REPO_ROOT"

# ---------- openvm-eth checkout at the pinned ref, patched to local powdr ----------
if [ ! -d openvm-eth ]; then
    git clone https://github.com/powdr-labs/openvm-eth.git openvm-eth
fi
git -C openvm-eth checkout "$OPENVM_ETH_REF"
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

cd openvm-eth
RES_DIR=reth_gpu
mkdir -p "$RES_DIR"

# ---------- Prefetch RPC cache ----------
# `--mode execute` populates rpc-cache/ with the block data; later runs load
# from the cache and never touch the RPC again.
./run.sh --cuda --block "$BLOCK_NUMBER" --apc 0 --mode execute || exit 1

# ---------- Compile APC caches ----------
# Under the default cell PGO the `generate` blob (build + rank every
# candidate) is keyed independently of --apc, so it's built once and reused;
# only the cheap per-N select+setup blobs accumulate in apc-cache/. Doing
# this in a separate CPU phase keeps the prove-stark metrics clean.
for apc in "${APC_COUNTS[@]}"; do
    ./run.sh --cuda --block "$BLOCK_NUMBER" --apc "$apc" --mode compile || exit 1
done

# ---------- Prove sweep (GPU) ----------
for apc in "${APC_COUNTS[@]}"; do
    label=$(printf 'apc%03d' "$apc")
    if ./run.sh --cuda --block "$BLOCK_NUMBER" --apc "$apc" --mode prove-stark; then
        echo "Finished proving with $apc APCs"
        mv metrics.json "$RES_DIR/${label}.json"
        # Keep the GPU memory trace (written by run.sh's gpu monitor) — at
        # high APC counts GPU memory is the expected limit.
        [ -f gpu_memory_usage.csv ] && mv gpu_memory_usage.csv "$RES_DIR/gpu_memory_usage_${label}.csv"
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
# ~11k candidate blocks. NOTE: at the pinned openvm-eth ref the benchmark
# binary doesn't wire POWDR_APC_CANDIDATES_DIR into powdr's GenerateConfig
# (the comment in its run.sh is stale), so the dump only exists when
# openvm-eth-bench.patch (next to this script) is applied.
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
mv "$RES_DIR" ../results/
