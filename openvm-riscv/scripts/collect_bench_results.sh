#!/bin/bash

# Collect a results/ tree (as produced by run_guest_benches.sh and
# run_reth_gpu_bench.sh) into a directory suitable for committing, mirroring
# what the nightly publishes to the bench-results repo:
#   - everything except the .bench-cache dirs (stage blobs, GBs of caches),
#   - plus each guest's apc_candidates.json pulled out of the hidden
#     .bench-cache so the APC analyzer links in the readme work. (The nightly
#     artifact upload silently drops hidden dirs, which is why published guest
#     results lack apc_candidates.json; copying them out fixes that here.)
#
# Usage: collect_bench_results.sh [results-dir] [dest-dir]

set -e

SRC="${1:-results}"
DST="${2:-bench-results}"

rm -rf "$DST"
mkdir -p "$DST"
(cd "$SRC" && tar cf - --exclude='.bench-cache' .) | (cd "$DST" && tar xf -)

# results/<exp>/.bench-cache/<guest>-input<N>/candidates/apc_candidates.json
# -> <dst>/<exp>/candidates/<guest>-input<N>/apc_candidates.json
for cand in "$SRC"/*/.bench-cache/*/candidates/apc_candidates.json; do
    [ -f "$cand" ] || continue
    cache_name=$(basename "$(dirname "$(dirname "$cand")")")
    exp=$(basename "$(dirname "$(dirname "$(dirname "$(dirname "$cand")")")")")
    mkdir -p "$DST/$exp/candidates/$cache_name"
    cp "$cand" "$DST/$exp/candidates/$cache_name/apc_candidates.json"
done
