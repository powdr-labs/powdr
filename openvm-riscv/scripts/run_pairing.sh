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
        "cargo run --bin powdr_openvm_riscv -r --features \"metrics,cuda\" prove \"$guest\" --input \"$input\" --autoprecompiles \"$apcs\" --metrics \"${run_name}/metrics.json\" --recursion --apc-candidates-dir \"${run_name}\""

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # apc_candidates.json is only available when apcs > 0
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${run_name}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up some files that we don't want to to push.
    rm -f "${run_name}"/apc_candidate_*
}

# TODO: Some benchmarks are currently disabled to keep the nightly run below 6h.

### Pairing
dir="results/pairing"
input="0" # No input

mkdir -p "$dir"
pushd "$dir"

run_bench guest-pairing-manual-precompile "$input" 0 manual
run_bench guest-pairing "$input" 0 apc000
run_bench guest-pairing "$input" 3 apc003
# run_bench guest-pairing "$input" 10 apc010  # Save ~4mins
#run_bench guest-pairing "$input" 30 apc030
# run_bench guest-pairing "$input" 100 apc100  # Save ~7mins 

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
popd