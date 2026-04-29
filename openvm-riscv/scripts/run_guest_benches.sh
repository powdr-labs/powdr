#!/bin/bash

# Script to collect some numbers from our OpenVM guest examples.
# Mostly for CI usage, but can be easily modified for manual tests.

# NOTE: The script expects the python environment to be set up with the required
# dependencies. Should be run from the project root, will create a `results`
# directory.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")

# Pre-generate APCs for a guest into a shared cache directory once per guest, so
# the per-`apc-count` runs below can reuse them via --apc-cache-dir instead of
# rebuilding APCs from scratch on every invocation.
generate_apcs() {
    guest="$1"
    apcs_dir="$2"

    echo ""
    echo "==== generating APC cache for ${guest} -> ${apcs_dir} ===="
    echo ""

    cargo run --bin powdr_openvm_riscv -r --features metrics -- \
        generate-apcs "$guest" --apcs-dir "$apcs_dir"
}

run_bench() {
    guest="$1"
    input="$2"
    apcs="$3"
    run_name="$4"
    apcs_cache_dir="$5"

    echo ""
    echo "==== ${run_name} ===="
    echo ""

    mkdir -p "${run_name}"

    psrecord --include-children --interval 1 \
        --log "${run_name}"/psrecord.csv \
        --log-format csv \
        --plot "${run_name}"/psrecord.png \
        "cargo run --bin powdr_openvm_riscv -r --features metrics prove \"$guest\" --input \"$input\" --autoprecompiles \"$apcs\" --metrics \"${run_name}/metrics.json\" --recursion --apc-candidates-dir \"${run_name}\" --apc-cache-dir \"${apcs_cache_dir}\""

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # apc_candidates.json is only available when apcs > 0
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${run_name}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up some files that we don't want to to push.
    rm -f "${run_name}"/apc_candidate_*
}

# TODO: Some benchmarks are currently disabled to keep the nightly run below 6h.

### Keccak
dir="results/keccak"
input="10000"

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_keccak"
generate_apcs guest-keccak "$cache"

run_bench guest-keccak-manual-precompile "$input" 0 manual "$cache"
run_bench guest-keccak "$input" 0 apc000 "$cache"
# run_bench guest-keccak "$input" 3 apc003 "$cache"  # Save ~6mins
# run_bench guest-keccak "$input" 10 apc010 "$cache"  # Save ~3mins
run_bench guest-keccak "$input" 30 apc030 "$cache"

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### SHA256
dir="results/sha256"
input="30000"

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_sha256"
generate_apcs guest-sha256 "$cache"

run_bench guest-sha256-manual-precompile "$input" 0 manual "$cache"
run_bench guest-sha256 "$input" 0 apc000 "$cache"
# run_bench guest-sha256 "$input" 3 apc003 "$cache"  # Save ~4mins
# run_bench guest-sha256 "$input" 10 apc010 "$cache"  # Save ~5mins
run_bench guest-sha256 "$input" 30 apc030 "$cache"

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Pairing
dir="results/pairing"
input="0" # No input

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_pairing"
generate_apcs guest-pairing "$cache"

run_bench guest-pairing-manual-precompile "$input" 0 manual "$cache"
run_bench guest-pairing "$input" 0 apc000 "$cache"
# run_bench guest-pairing "$input" 3 apc003 "$cache"  # Save ~6mins
# run_bench guest-pairing "$input" 10 apc010 "$cache"  # Save ~4mins
run_bench guest-pairing "$input" 30 apc030 "$cache"
# run_bench guest-pairing "$input" 100 apc100 "$cache"  # Save ~7mins

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### U256
dir="results/u256"
input="0" # No input

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_u256"
generate_apcs guest-u256 "$cache"

run_bench guest-u256-manual-precompile "$input" 0 manual "$cache"
run_bench guest-u256 "$input" 0 apc000 "$cache"
# run_bench guest-u256 "$input" 3 apc003 "$cache"  # Save ~10mins
# run_bench guest-u256 "$input" 10 apc010 "$cache"  # Save ~4mins
run_bench guest-u256 "$input" 30 apc030 "$cache"

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### Matmul
dir="results/matmul"

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_matmul"
generate_apcs guest-matmul "$cache"

run_bench guest-matmul 0 0 apc000 "$cache"
run_bench guest-matmul 0 3 apc003 "$cache"
# run_bench guest-matmul 0 10 apc010 "$cache"  # Save ~1min
run_bench guest-matmul 0 30 apc030 "$cache"

python3 "$SCRIPTS_DIR"/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 "$SCRIPTS_DIR"/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 "$SCRIPTS_DIR"/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECC
dir="results/ecc"
input="50"

mkdir -p "$dir"
pushd "$dir"

cache_proj="$(pwd)/apcs_cache_ecc_projective"
cache_aff="$(pwd)/apcs_cache_ecc_affine_hint"
generate_apcs guest-ecc-projective "$cache_proj"
generate_apcs guest-ecc-powdr-affine-hint "$cache_aff"

run_bench guest-ecc-manual $input 0 manual "$cache_proj"
run_bench guest-ecc-projective $input 0 projective-apc000 "$cache_proj"
# run_bench guest-ecc-projective $input 3 projective-apc003 "$cache_proj"  # Save ~17mins
# run_bench guest-ecc-projective $input 10 projective-apc010 "$cache_proj"  # Save ~12mins
run_bench guest-ecc-projective $input 30 projective-apc030 "$cache_proj"
# run_bench guest-ecc-projective $input 100 projective-apc100 "$cache_proj"  # Save ~12mins
run_bench guest-ecc-powdr-affine-hint $input 0 affine-hint-apc000 "$cache_aff"
# run_bench guest-ecc-powdr-affine-hint $input 3 affine-hint-apc003 "$cache_aff"  # Save ~10mins
# run_bench guest-ecc-powdr-affine-hint $input 10 affine-hint-apc010 "$cache_aff"  # Save ~7mins
run_bench guest-ecc-powdr-affine-hint $input 30 affine-hint-apc030 "$cache_aff"
# run_bench guest-ecc-powdr-affine-hint $input 100 affine-hint-apc100 "$cache_aff"  # Save ~7mins

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd

### ECRECOVER
dir="results/ecrecover"
input="20"

mkdir -p "$dir"
pushd "$dir"

cache="$(pwd)/apcs_cache_ecrecover"
generate_apcs guest-ecrecover "$cache"

run_bench guest-ecrecover-manual $input 0 manual "$cache"
run_bench guest-ecrecover $input 0 apc000 "$cache"
# run_bench guest-ecrecover $input 3 apc003 "$cache"  # Save ~9mins
# run_bench guest-ecrecover $input 10 apc010 "$cache"  # Save ~6mins
run_bench guest-ecrecover $input 30 apc030 "$cache"
# run_bench guest-ecrecover $input 100 apc100 "$cache"  # Save ~6mins

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
python3 $SCRIPTS_DIR/basic_metrics.py combine **/metrics.json > combined_metrics.json
popd