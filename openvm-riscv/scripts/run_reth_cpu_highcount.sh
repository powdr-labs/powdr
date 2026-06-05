#!/bin/bash

# High-APC-count reth probes on CPU, with raised aggregation caps, to find the
# CPU limit. Runs *after* run_reth_cpu_bench.sh (which does the core
# {0,3,10,30,100,300} sweep and leaves results/reth/ populated). Appends each
# successful high-count point to results/reth/ and regenerates the aggregate
# files (basic_metrics.csv, combined_metrics.json, proof_time_breakdown.png)
# over all counts; failures (with the caps tried) go to
# results/reth/failed_runs.txt.
#
# Each probe is "APC:LEAF_LOG_STACKED_HEIGHT:INTERNAL_LOG_STACKED_HEIGHT".
# Defaults mirror the GPU run's documented retries (bench-results/reth_gpu):
# 500 needed leaf 22 / internal 20; the >500 counts need leaf 23 / internal 21.
# On CPU there is no 24 GiB-VRAM ceiling (this box has 251 GiB RAM), so counts
# the RTX 4090 ran out of memory on may complete here — that's the point of the
# probe. Run from the powdr repo root with the venv active.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")
REPO_ROOT=$(realpath "$SCRIPTS_DIR/../..")

BLOCK_NUMBER="${BLOCK_NUMBER:-24171377}"
export POWDR_OPENVM_SEGMENT_DELTA="${POWDR_OPENVM_SEGMENT_DELTA:-50000}"
PROBES=(${PROBES:-500:22:20 1000:23:21})

cd "$REPO_ROOT/openvm-eth"
RES_DIR="$REPO_ROOT/results/reth"
mkdir -p "$RES_DIR"
# Binary already built by run_reth_cpu_bench.sh; skip rebuilds so psrecord's
# peak memory reflects proving only.
export OPENVM_BENCH_SKIP_BUILD=1

for probe in "${PROBES[@]}"; do
    apc="${probe%%:*}"; rest="${probe#*:}"; leaf="${rest%%:*}"; internal="${rest##*:}"
    label=$(printf 'apc%03d' "$apc")
    echo "==== probe apc=$apc  leaf-log-stacked-height=$leaf  internal-log-stacked-height=$internal ===="
    # Compile the select+setup blobs for this count (generate is cached from the
    # core sweep under the shared apc-cache/). A compile failure is recorded too.
    if ! ./run.sh --block "$BLOCK_NUMBER" --apc "$apc" --mode compile; then
        echo "${apc}: compile failed" >> "$RES_DIR/failed_runs.txt"
        continue
    fi
    rm -f metrics.json
    psrecord --include-children --interval 1 \
        --log "$RES_DIR/psrecord_${label}.csv" --log-format csv \
        --plot "$RES_DIR/psrecord_${label}.png" \
        "./run.sh --block $BLOCK_NUMBER --apc $apc --mode prove-stark --leaf-log-stacked-height $leaf --internal-log-stacked-height $internal" || true
    if [ -s metrics.json ]; then
        echo "OK apc=$apc"
        mv metrics.json "$RES_DIR/${label}.json"
        python3 "$REPO_ROOT/openvm-riscv/scripts/plot_trace_cells.py" -o "$RES_DIR/trace_cells_${label}.png" "$RES_DIR/${label}.json" > "$RES_DIR/trace_cells_${label}.txt"
    else
        echo "${apc}: prove-stark failed with --leaf-log-stacked-height ${leaf} --internal-log-stacked-height ${internal}" >> "$RES_DIR/failed_runs.txt"
        rm -f metrics.json
    fi
done

# Same APC ranking as the core sweep; refresh the dump if it got rewritten.
[ -f apcs/apc_candidates.json ] && cp apcs/apc_candidates.json "$RES_DIR/apc_candidates.json"
find apcs -maxdepth 1 -name 'apc_candidate_*' -delete 2>/dev/null || true

# Regenerate aggregates over ALL successful counts (core + high), numerically
# ordered so apc1000 sorts after apc300 (not lexicographically before it).
mapfile -t OK_JSONS < <(for f in "$RES_DIR"/apc[0-9]*.json; do
    [ -f "$f" ] || continue
    n=$(basename "$f" .json); echo "${n#apc} $f"
done | sort -n | awk '{print $2}')

python3 "$REPO_ROOT/openvm-riscv/scripts/basic_metrics.py" summary-table --csv "${OK_JSONS[@]}" > "$RES_DIR/basic_metrics.csv"
python3 "$REPO_ROOT/openvm-riscv/scripts/basic_metrics.py" plot "${OK_JSONS[@]}" -o "$RES_DIR/proof_time_breakdown.png"
python3 "$REPO_ROOT/openvm-riscv/scripts/basic_metrics.py" combine "${OK_JSONS[@]}" > "$RES_DIR/combined_metrics.json"
echo "Done. Counts now in results/reth: $(for f in "${OK_JSONS[@]}"; do basename "$f" .json; done | tr '\n' ' ')"