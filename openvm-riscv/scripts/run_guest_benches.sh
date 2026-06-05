#!/bin/bash

# Script to collect some numbers from our OpenVM guest examples.
# Mostly for CI usage, but can be easily modified for manual tests.

# NOTE: The script expects the python environment to be set up with the required
# dependencies. Should be run from the project root, will create a `results`
# directory.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")

# Cargo features for the powdr CLI build. Override to e.g. "metrics,cuda" to
# prove on GPU: BENCH_FEATURES=metrics,cuda ./openvm-riscv/scripts/run_guest_benches.sh
BENCH_FEATURES="${BENCH_FEATURES:-metrics}"

# APC counts swept for every guest (manual-precompile baselines always run
# with 0). With the default cell PGO the expensive generate stage is cached
# per (guest, profile-input), so each additional count only re-runs the cheap
# select+setup stages plus the prove itself.
APC_COUNTS=(0 3 10 30 100 300)

# With BENCH_KEEP_GOING=1 a failed prove (e.g. OOM at a high APC count) is
# recorded in the experiment's failed_runs.txt instead of aborting the whole
# sweep. CI keeps the strict default.
BENCH_KEEP_GOING="${BENCH_KEEP_GOING:-0}"

# The largest (software / 0-APC) guest proofs hold tens of GiB of trace in host
# RAM; without jemalloc's background purging, freed pages accumulate and the
# big workloads OOM even on a 256 GiB box. Match the nightly workflow, which
# exports this for all jobs, so the sweep doesn't silently depend on the
# caller's environment.
export JEMALLOC_SYS_WITH_MALLOC_CONF="${JEMALLOC_SYS_WITH_MALLOC_CONF:-retain:true,background_thread:true,metadata_thp:always,dirty_decay_ms:10000,muzzy_decay_ms:10000,abort_conf:true}"

# With BENCH_SKIP_EXISTING=1 a run whose metrics.json already exists (non-empty)
# is skipped, making the sweep resumable after an interrupted run. Default off
# so CI (which starts from an empty results/) always runs fresh.
BENCH_SKIP_EXISTING="${BENCH_SKIP_EXISTING:-0}"

run_bench() {
    guest="$1"
    input="$2"
    apcs="$3"
    run_name="$4"

    echo ""
    echo "==== ${run_name} ===="
    echo ""

    if [ "$BENCH_SKIP_EXISTING" = "1" ] && [ -s "${run_name}/metrics.json" ]; then
        echo "SKIP ${run_name}: metrics.json already present"
        return 0
    fi

    # `--artifacts-dir` and `--apc-candidates-dir` are shared across all
    # `run_bench` calls with the same (guest, profile-input). For cell PGO
    # the generate stage doesn't depend on `--autoprecompiles`, so sweeping
    # `apcs` (apc010, apc030, apc100, …) hits the generate-stage cache from
    # the second call onward — only the cheap select+setup stages re-run.
    # `--apc-candidates-dir` has to be shared too because it lives in the
    # generate-stage hash; if each run wrote to a different dir the cache
    # would invalidate.
    cache_root=".bench-cache/${guest}-input${input}"
    artifacts_dir="${cache_root}/artifacts"
    candidates_dir="${cache_root}/candidates"
    mkdir -p "${candidates_dir}"

    mkdir -p "${run_name}"

    psrecord --include-children --interval 1 \
        --log "${run_name}"/psrecord.csv \
        --log-format csv \
        --plot "${run_name}"/psrecord.png \
        "cargo run --bin powdr_openvm_riscv -r --features ${BENCH_FEATURES} -- --artifacts-dir \"${artifacts_dir}\" prove \"$guest\" --profile-input \"$input\" --input \"$input\" --autoprecompiles \"$apcs\" --metrics \"${run_name}/metrics.json\" --recursion --apc-candidates-dir \"${candidates_dir}\"" || true

    # psrecord does not reliably propagate the wrapped command's exit code,
    # so judge success by the metrics file the prove must write (the CLI
    # creates it empty up front; non-empty means the run completed).
    if [ ! -s "${run_name}/metrics.json" ]; then
        echo "PROVE FAILED for ${run_name} (no metrics produced)"
        rm -f "${run_name}/metrics.json"
        if [ "$BENCH_KEEP_GOING" = "1" ]; then
            echo "${guest} input=${input} apcs=${apcs} run=${run_name}" >> failed_runs.txt
            return 0
        fi
        return 1
    fi

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # apc_candidates.json is only available when apcs > 0. It lives in the
    # shared candidates_dir, written on the first cache-miss run for this
    # (guest, profile-input) pair.
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${candidates_dir}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up per-block snapshot files we don't want to push. They are
    # written into the shared candidates_dir on the first cache-miss run
    # and not re-created on cache hits, so this is effectively a one-time
    # cleanup per (guest, profile-input). Use find: for guests with many
    # basic blocks the glob expansion exceeds ARG_MAX.
    find "${candidates_dir}" -maxdepth 1 -name 'apc_candidate_*' -delete
}

### Keccak
dir="results/keccak"
input="25000" # ~50 segments at apc000 (same workload as the autoprecompiles blog post)

mkdir -p "$dir"
pushd "$dir"

run_bench guest-keccak-manual-precompile "$input" 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-keccak "$input" "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### SHA256
dir="results/sha256"
input="80000" # ~50 segments at apc000 (same workload as the autoprecompiles blog post)

mkdir -p "$dir"
pushd "$dir"

run_bench guest-sha256-manual-precompile "$input" 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-sha256 "$input" "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Pairing
dir="results/pairing"
input="0" # No input: the guest runs one fixed pairing check (~5 segments at apc000)

mkdir -p "$dir"
pushd "$dir"

run_bench guest-pairing-manual-precompile "$input" 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-pairing "$input" "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### U256
dir="results/u256"
input="0" # No input: the guest runs a fixed 70x70 U256 matmul (~12 segments at apc000)

mkdir -p "$dir"
pushd "$dir"

run_bench guest-u256-manual-precompile "$input" 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-u256 "$input" "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Matmul
dir="results/matmul"

mkdir -p "$dir"
pushd "$dir"

for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-matmul 0 "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 "$SCRIPTS_DIR"/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 "$SCRIPTS_DIR"/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 "$SCRIPTS_DIR"/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECC
dir="results/ecc"
input="100" # ~50 segments at apc000 for guest-ecc-projective

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecc-manual $input 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-ecc-projective "$input" "$apcs" "$(printf 'projective-apc%03d' "$apcs")"
done
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-ecc-powdr-affine-hint "$input" "$apcs" "$(printf 'affine-hint-apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECRECOVER
dir="results/ecrecover"
input="125" # ~50 segments at apc000

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecrecover-manual $input 0 manual
for apcs in "${APC_COUNTS[@]}"; do
    run_bench guest-ecrecover "$input" "$apcs" "$(printf 'apc%03d' "$apcs")"
done

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd