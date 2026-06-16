#! /bin/bash
set -e

# Image to blur (optional; defaults to the bundled 128x128 sample).
IMG="${1:-assets/example_256.png}"

# Baseline: No APCs
cargo run --release --manifest-path host/Cargo.toml -- --image "$IMG"
mv metrics.json metrics0.json

# 5 APCs
cargo run --release --manifest-path host/Cargo.toml -- --image "$IMG" --apc 5
mv metrics.json metrics5.json

# Combine both runs into a single metrics file (labels: metrics0, metrics5).
python3 ../../openvm-riscv/scripts/basic_metrics.py combine metrics0.json metrics5.json > combined_metrics.json
echo "Combined metrics written to combined_metrics.json"