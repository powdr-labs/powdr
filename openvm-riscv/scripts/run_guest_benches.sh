#!/bin/bash

# Script to collect some numbers from our OpenVM guest examples.
# Mostly for CI usage, but can be easily modified for manual tests.

# NOTE: The script expects the python environment to be set up with the required
# dependencies. Should be run from the project root, will create a `results`
# directory.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")

run_bench() {
    guest="$1"
    input="$2"
    apcs="$3"
    run_name="$4"

    echo ""
    echo "==== ${run_name} ===="
    echo ""

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
        "cargo run --bin powdr_openvm_riscv -r --features metrics -- --artifacts-dir \"${artifacts_dir}\" prove \"$guest\" --profile-input \"$input\" --input \"$input\" --autoprecompiles \"$apcs\" --metrics \"${run_name}/metrics.json\" --recursion --apc-candidates-dir \"${candidates_dir}\""

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # apc_candidates.json is only available when apcs > 0. It lives in the
    # shared candidates_dir, written on the first cache-miss run for this
    # (guest, profile-input) pair.
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${candidates_dir}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up per-block snapshot files we don't want to push.
    # Use `find -delete` rather than `rm glob`: large guests emit thousands of
    # `apc_candidate_*` files (e.g. ~7k blocks), and the expanded glob blows
    # past ARG_MAX ("Argument list too long"). `apc_candidates.json` doesn't
    # match `apc_candidate_*` (no `_` after `candidate`), so it's preserved.
    find "${candidates_dir}" -maxdepth 1 -name 'apc_candidate_*' -delete
}

# TODO: Some benchmarks are currently disabled to keep the nightly run below 6h.

### Keccak
dir="results/keccak"
input="10000"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-keccak-manual-precompile "$input" 0 manual
run_bench guest-keccak "$input" 0 apc000
# run_bench guest-keccak "$input" 3 apc003  # Save ~6mins
# run_bench guest-keccak "$input" 10 apc010  # Save ~3mins
run_bench guest-keccak "$input" 30 apc030

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### SHA256
dir="results/sha256"
input="30000"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-sha256-manual-precompile "$input" 0 manual
run_bench guest-sha256 "$input" 0 apc000
# run_bench guest-sha256 "$input" 3 apc003  # Save ~4mins
# run_bench guest-sha256 "$input" 10 apc010  # Save ~5mins
run_bench guest-sha256 "$input" 30 apc030

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Pairing
dir="results/pairing"
input="0" # No input

mkdir -p "$dir"
pushd "$dir"

run_bench guest-pairing-manual-precompile "$input" 0 manual
run_bench guest-pairing "$input" 0 apc000
# run_bench guest-pairing "$input" 3 apc003  # Save ~6mins
# run_bench guest-pairing "$input" 10 apc010  # Save ~4mins
run_bench guest-pairing "$input" 30 apc030
# run_bench guest-pairing "$input" 100 apc100  # Save ~7mins 

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### U256
dir="results/u256"
input="0" # No input

mkdir -p "$dir"
pushd "$dir"

run_bench guest-u256-manual-precompile "$input" 0 manual
run_bench guest-u256 "$input" 0 apc000
# run_bench guest-u256 "$input" 3 apc003  # Save ~10mins
# run_bench guest-u256 "$input" 10 apc010  # Save ~4mins
run_bench guest-u256 "$input" 30 apc030

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Matmul
dir="results/matmul"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-matmul 0 0 apc000
run_bench guest-matmul 0 3 apc003
# run_bench guest-matmul 0 10 apc010  # Save ~1min
run_bench guest-matmul 0 30 apc030

python3 "$SCRIPTS_DIR"/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 "$SCRIPTS_DIR"/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 "$SCRIPTS_DIR"/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECC
dir="results/ecc"
input="50"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecc-manual $input 0 manual
run_bench guest-ecc-projective $input 0 projective-apc000
# run_bench guest-ecc-projective $input 3 projective-apc003  # Save ~17mins
# run_bench guest-ecc-projective $input 10 projective-apc010  # Save ~12mins
run_bench guest-ecc-projective $input 30 projective-apc030
# run_bench guest-ecc-projective $input 100 projective-apc100  # Save ~12mins
run_bench guest-ecc-powdr-affine-hint $input 0 affine-hint-apc000
# run_bench guest-ecc-powdr-affine-hint $input 3 affine-hint-apc003  # Save ~10mins
# run_bench guest-ecc-powdr-affine-hint $input 10 affine-hint-apc010  # Save ~7mins
run_bench guest-ecc-powdr-affine-hint $input 30 affine-hint-apc030
# run_bench guest-ecc-powdr-affine-hint $input 100 affine-hint-apc100  # Save ~7mins

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECRECOVER
dir="results/ecrecover"
input="20"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecrecover-manual $input 0 manual
run_bench guest-ecrecover $input 0 apc000
# run_bench guest-ecrecover $input 3 apc003  # Save ~9mins
# run_bench guest-ecrecover $input 10 apc010  # Save ~6mins
run_bench guest-ecrecover $input 30 apc030
# run_bench guest-ecrecover $input 100 apc100  # Save ~6mins

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd