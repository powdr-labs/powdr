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

    mkdir -p "${run_name}"

    psrecord --include-children --interval 1 \
        --log "${run_name}"/psrecord.csv \
        --log-format csv \
        --plot "${run_name}"/psrecord.png \
        "cargo run --bin powdr_openvm -r prove \"$guest\" --input \"$input\" --autoprecompiles \"$apcs\" --metrics \"${run_name}/metrics.json\" --recursion --apc-candidates-dir \"${run_name}\""

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # apc_candidates.json is only available when apcs > 0
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${run_name}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up some files that we don't want to to push.
    rm debug.pil
    rm -f "${run_name}"/*.cbor
}

### Keccak
dir="results/keccak"
input="10000"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-keccak-manual-precompile "$input" 0 manual
run_bench guest-keccak "$input" 0 noapc
run_bench guest-keccak "$input" 3 3apc
run_bench guest-keccak "$input" 10 10apc
run_bench guest-keccak "$input" 30 30apc

python3 $SCRIPTS_DIR/basic_metrics.py --csv **/metrics.json > basic_metrics.csv
popd

### Matmul
dir="results/matmul"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-matmul 0 0 noapc
run_bench guest-matmul 0 3 3apc
run_bench guest-matmul 0 10 10apc
run_bench guest-matmul 0 30 30apc

python3 "$SCRIPTS_DIR"/basic_metrics.py --csv **/metrics.json > basic_metrics.csv
popd

### ECC
dir="results/ecc"
input="50"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecc-manual $input 0 manual
run_bench guest-ecc-projective $input 0 projective-apc000
run_bench guest-ecc-projective $input 3 projective-apc003
run_bench guest-ecc-projective $input 10 projective-apc010
run_bench guest-ecc-projective $input 30 projective-apc030
run_bench guest-ecc-projective $input 100 projective-apc100
run_bench guest-ecc-powdr-affine-hint $input 0 affine-hint-apc000
run_bench guest-ecc-powdr-affine-hint $input 3 affine-hint-apc003
run_bench guest-ecc-powdr-affine-hint $input 10 affine-hint-apc010
run_bench guest-ecc-powdr-affine-hint $input 30 affine-hint-apc030
run_bench guest-ecc-powdr-affine-hint $input 100 affine-hint-apc100

python3 $SCRIPTS_DIR/basic_metrics.py --csv **/metrics.json > basic_metrics.csv
popd

### ECRECOVER
dir="results/ecrecover"
input="50"

mkdir -p "$dir"
pushd "$dir"

run_bench guest-ecrecover-manual $input 0 manual
run_bench guest-ecrecover $input 0 apc000
run_bench guest-ecrecover $input 3 apc003
run_bench guest-ecrecover $input 10 apc010
run_bench guest-ecrecover $input 30 apc030
run_bench guest-ecrecover $input 100 apc100

python3 $SCRIPTS_DIR/basic_metrics.py --csv **/metrics.json > basic_metrics.csv
popd