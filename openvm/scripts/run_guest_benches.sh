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
    python3 $SCRIPTS_DIR/basic_metrics.py --csv *.json > basic_metrics.csv
}

plot_cells() {
    python3 $SCRIPTS_DIR/plot_trace_cells.py -o trace_cells.png $1 > trace_cells.txt
}

run_bench() {
    guest="$1"
    guest_manual="$2"
    apcs="$3"
    input="$4"
    dir="results/$guest"
    mkdir -p "$dir"
    pushd "$dir"
    # prove with manual precompile if given
    if [ -n "$guest_manual" ]; then
        cargo run --bin powdr_openvm -r prove $guest_manual --input "$input" --metrics manual.json
    fi
    # prove with no APCs
    cargo run --bin powdr_openvm -r prove $guest --input $input --metrics noapc.json
    # proving with APCs and record memory usage
    with_psrecord "cargo run --bin powdr_openvm -r prove $guest --input "$input" --autoprecompiles $apcs --metrics ${apcs}apc.json"
    # process results
    basic_metrics
    plot_cells ${apcs}apc.json
    rm debug.pil
    popd
}

run_bench guest-keccak guest-keccak-manual-precompile 100 100
run_bench guest-matmul "" 100 0
