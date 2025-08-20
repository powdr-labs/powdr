#!/bin/bash

# Script to collect some numbers from our OpenVM guest examples.
# Mostly for CI usage, but can be easily modified for manual tests.

# NOTE: The script expects the python environment to be set up with the required
# dependencies. Should be run from the project root, will create a `results`
# directory.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")

# function to run using psrecord
with_psrecord() {
    psrecord --include-children --interval 1 --log psrecord.csv --log-format csv --plot psrecord.png "$@"
}

basic_metrics() {
    python3 $SCRIPTS_DIR/basic_metrics.py --csv **/metrics.json > basic_metrics.csv
}

plot_cells() {
    run_name="$1"
    python3 $SCRIPTS_DIR/plot_trace_cells.py -o ${run_name}/trace_cells.png ${run_name}/metrics.json > ${run_name}/trace_cells.txt
}

plot_effectiveness() {
    python3 $SCRIPTS_DIR/../../autoprecompiles/scripts/plot_effectiveness.py $1 --output effectiveness.png
}

run_bench() {
    guest="$1"
    input="$2"
    apcs="$3"
    run_name="$4"

    echo "\n==== ${run_name} ====\n"

    mkdir -p ${run_name}
    # prove with APCs and record memory usage; default Pgo::Cell mode also collects data on all APC candidates
    with_psrecord "cargo run --bin powdr_openvm -r prove $guest --input $input --autoprecompiles $apcs --metrics ${run_name}/metrics.json --recursion --apc-candidates-dir ${run_name}"
    plot_cells ${run_name}
    rm debug.pil
    rm ${run_name}/*.cbor
}

### Keccak
dir="results/keccak"
# TODO: Make 10k again
input="10"

mkdir -p "$dir"
pushd "$dir"

run guest-keccak-manual-precompile "$input" 0 manual
run guest-keccak "$input" 0 noapc
run guest-keccak "$input" 100 100apc

basic_metrics
plot_effectiveness 100apc/apc_candidates.json
popd
