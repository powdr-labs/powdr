#!/bin/bash

# Script to collect some numbers from our OpenVM guest examples.
# Mostly for CI usage, but can be easily modified for manual tests.

# NOTE: The script expects the python environment to be set up with the required
# dependencies. Should be run from the project root, will create a `results`
# directory.

set -e

SCRIPT_PATH=$(realpath "${BASH_SOURCE[0]}")
SCRIPTS_DIR=$(dirname "$SCRIPT_PATH")

# Build the binary once up front so profilers don't instrument cargo/rustc.
echo "==== Building powdr_openvm_riscv (release, metrics+cuda) ===="
cargo build --bin powdr_openvm_riscv -r --features "metrics,cuda"
PROVE_BIN="$(cargo metadata --format-version 1 --no-deps 2>/dev/null | python3 -c 'import sys,json; print(json.load(sys.stdin)["target_directory"])')/release/powdr_openvm_riscv"
echo "Binary: $PROVE_BIN"

compile_artifact() {
    guest="$1"
    input="$2"
    apcs="$3"
    run_name="$4"
    artifact_path="$5"

    if [ -f "$artifact_path" ]; then
        echo ""
        echo "==== Skipping compilation: ${artifact_path} already exists ===="
        echo ""
        return
    fi

    echo ""
    echo "==== Compiling artifact: ${artifact_path} (apcs=${apcs}) ===="
    echo ""

    mkdir -p "${run_name}"

    "$PROVE_BIN" compile "$guest" --input "$input" --autoprecompiles "$apcs" --output "$artifact_path" --apc-candidates-dir "${run_name}"

    # apc_candidates.json is only available when apcs > 0
    if [ "${apcs:-0}" -ne 0 ]; then
        python3 "$SCRIPTS_DIR"/../../autoprecompiles/scripts/plot_effectiveness.py "${run_name}"/apc_candidates.json --output "${run_name}"/effectiveness.png
    fi

    # Clean up some files that we don't want to push.
    rm -f "${run_name}"/apc_candidate_*
}

run_bench() {
    artifact_path="$1"
    input="$2"
    run_name="$3"

    echo ""
    echo "==== ${run_name} ===="
    echo ""

    mkdir -p "${run_name}"

    # psrecord for memory/CPU tracking
    psrecord --include-children --interval 1 \
        --log "${run_name}"/psrecord.csv \
        --log-format csv \
        --plot "${run_name}"/psrecord.png \
        "$PROVE_BIN prove --artifact $artifact_path --input $input --metrics ${run_name}/metrics.json --recursion"

    python3 "$SCRIPTS_DIR"/plot_trace_cells.py -o "${run_name}"/trace_cells.png "${run_name}"/metrics.json > "${run_name}"/trace_cells.txt

    # Nsight Systems: full application timeline (CPU + GPU activity)
    if command -v nsys &> /dev/null; then
        echo "  -> Running Nsight Systems profiling..."
        nsys profile \
            --output "${run_name}/nsys_report" \
            --force-overwrite true \
            --trace cuda,nvtx,osrt \
            --sample none \
            --stats true \
            -- "$PROVE_BIN" prove --artifact "$artifact_path" --input "$input" --recursion
        # Export summaries to text for quick inspection
        nsys stats --force-export true --report cuda_gpu_trace "${run_name}/nsys_report.nsys-rep" > "${run_name}/nsys_gputrace.txt" 2>&1 || true
        nsys stats --force-export true --report cuda_gpu_kern_sum "${run_name}/nsys_report.nsys-rep" > "${run_name}/nsys_kernelsum.txt" 2>&1 || true
    else
        echo "  -> nsys not found, skipping Nsight Systems profiling."
        echo "     Install with: sudo apt-get install -y nsight-systems-2025.6.3"
    fi

    # Nsight Compute: detailed per-kernel metrics
    # NOTE: ncu currently cannot intercept kernel launches from this binary
    # (likely Driver API / cuModuleLoad). Use nsys kernel summary instead.
    # To retry: ncu --mode launch --set basic -o "${run_name}/ncu_report" -f \
    #   -- "$PROVE_BIN" prove --artifact "$artifact_path" --input "$input" --recursion
}

### Pairing
dir="results/pairing"
input="0" # No input
guest="guest-pairing"

mkdir -p "$dir"
pushd "$dir"

# Phase 1: Compile artifacts for each APC configuration (skipped if artifact exists)
compile_artifact "$guest" "$input" 0   apc000 apc000.cbor
compile_artifact "$guest" "$input" 100 apc100 apc100.cbor
compile_artifact "$guest" "$input" 300 apc300 apc300.cbor

# Phase 2: Prove + profile using pre-compiled artifacts
run_bench apc000.cbor "$input" apc000
run_bench apc100.cbor "$input" apc100
run_bench apc300.cbor "$input" apc300

python3 $SCRIPTS_DIR/basic_metrics.py summary-table --csv **/metrics.json > basic_metrics.csv
python3 $SCRIPTS_DIR/basic_metrics.py plot **/metrics.json -o proof_time_breakdown.png
popd
