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

plot_effectiveness() {
    python3 $SCRIPTS_DIR/plot_effectiveness.py $1 --output effectiveness.png
}

# usage: run_bench guest guest_manual_pcp apc_num input
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
        cargo run --bin powdr_openvm -r prove $guest_manual --input "$input" --metrics manual.json --recursion
    fi
    # prove with no APCs
    mkdir -p ${apcs}apc
    cargo run --bin powdr_openvm -r prove $guest --input $input --metrics noapc.json --recursion --apc-candidates-dir ${apcs}apc
    # proving with APCs and record memory usage
    with_psrecord "cargo run --bin powdr_openvm -r prove $guest --input "$input" --autoprecompiles $apcs --metrics ${apcs}apc.json --recursion"
    # process results
    basic_metrics
    plot_cells ${apcs}apc.json
    plot_effectiveness ${apcs}apc/apc_candidates.json
    rm debug.pil
    rm ${apcs}apc/*.cbor
    popd
}

# keccak for 10000 iterations, 100 apcs
run_bench guest-keccak guest-keccak-manual-precompile 100 10000

# run_bench guest-matmul "" 100 0
