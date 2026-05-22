# powdr OpenVM — pairing guest, GPU bench across APC counts

Sweep of `guest-pairing` proven on GPU with the powdr OpenVM RISC-V CLI for
**0, 30, 100, 300** autoprecompiles, with a matching Nsight Systems profile
per run.

=> [Metrics viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench%2Fpairing-gpu-apc-sweep%2Fresults%2Fpairing-gpu-bench%2Fcombined_metrics.json&baseline=apc000)

## Files

| file | what |
| --- | --- |
| `combined_metrics.json` | All four `metrics.json` payloads keyed by run label (`apc000`/`apc030`/`apc100`/`apc300`) — produced by `openvm-riscv/scripts/basic_metrics.py combine`. |
| `apc000.nsys-rep`, `apc030.nsys-rep`, `apc100.nsys-rep`, `apc300.nsys-rep` | Nsight Systems trace (`-t cuda,nvtx`) of each prove run. Open with `nsys-ui <file>` or `nsys stats <file>`. |

## Reproducing

Build the workspace with CUDA + metrics:

```sh
cargo build -r --bin powdr_openvm_riscv --features cuda,metrics
```

For each `N` in `0 30 100 300`:

```sh
# Metrics run
./target/release/powdr_openvm_riscv prove guest-pairing \
    --profile-input 0 --input 0 \
    --autoprecompiles $N \
    --recursion \
    --metrics  results/pairing-gpu-bench/apc${N}/metrics.json \
    --artifacts-dir results/pairing-gpu-bench/artifacts

# nsys profile of the same run (artifact cache means only the prove stage re-executes)
# Note: -t osrt was dropped because it deadlocks the cargo+rustc child the CLI
# invokes to (re)build the guest on this host; cuda,nvtx is what we keep.
nsys profile -t cuda,nvtx --force-overwrite=true \
    -o results/pairing-gpu-bench/apc${N} \
    ./target/release/powdr_openvm_riscv prove guest-pairing \
        --profile-input 0 --input 0 \
        --autoprecompiles $N \
        --recursion \
        --artifacts-dir results/pairing-gpu-bench/artifacts
```

The `--artifacts-dir` makes the profile/select/setup stages a cache hit on
later runs (changing `--autoprecompiles` only invalidates the select stage),
so the wall-clock difference between APC counts mostly reflects prove cost.

Combine the metrics:

```sh
python3 openvm-riscv/scripts/basic_metrics.py combine \
    results/pairing-gpu-bench/apc*/metrics.json \
    > results/pairing-gpu-bench/combined_metrics.json
```

## Machine

| | |
| --- | --- |
| CPU | AMD Ryzen 9 7950X, 16C/32T |
| RAM | 124 GiB |
| GPU | NVIDIA GeForce RTX 4090 (24 GiB, sm_89) |
| NVIDIA driver | 570.195.03 |
| CUDA toolkit | 12.8 (nvcc V12.8.93) |
| `ncu` | 2025.1.1.0 (blocked, see above) |
| `nsys` | 2025.6.3 |
| OS | Ubuntu 24.04.1 LTS, kernel 6.8.0-79-generic |
| powdr commit | 5f15093f2 (Use mutable pointers for hint outputs, #3747) |
| rust toolchain (guest) | nightly-2026-01-18 |

## Notes / caveats

- `--recursion` is on, matching `openvm-riscv/scripts/run_guest_benches.sh`.
- Metrics and nsys runs are separated, so timings in `metrics.json` are not
  perturbed by profiling overhead.
- `RmProfilingAdminOnly=1` blocks both `ncu` and `nsys --gpu-metrics`. If you
  want hardware-counter data on this host, set
  `options nvidia NVreg_RestrictProfilingToAdminUsers=0` in
  `/etc/modprobe.d/nvidia.conf` and reboot, or rerun as root.
