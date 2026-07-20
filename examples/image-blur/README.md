# image-blur

A small end-to-end powdr example that proves a 5×5 box blur of a **private**
image. The guest hashes the input image (keccak-256), blurs it, hashes the
result, and reveals a single 32-byte commitment to the two digests
(`keccak(h_in ‖ h_out)`) — so the proof attests *"I know an image whose
input/blur hashes commit to this value"*. The blur's inner loop is a single hot
basic block that dominates the trace, making it an ideal target for powdr's
**autoprecompiles (APCs)**. The host (this binary) decodes the image, drives
powdr as a library to compile/prove/verify, recomputes the blur natively with
the *same* `core::blur` to check the commitment, and writes `blur.png`.

## Usage

```sh
# Prove with one autoprecompile for the blur loop (writes blur.png):
cargo run --release --manifest-path host/Cargo.toml -- --image assets/example_128.png --apc 1

# Just build the APCs and run interpreted (no proof) — prints the instruction
# count, which drops sharply once APCs are enabled:
cargo run --release --manifest-path host/Cargo.toml -- --image assets/example_128.png --apc 1 --execute-only
```

Flags: `--apc N` (number of autoprecompiles; `0`, the default, disables them),
`--execute-only` (skip the STARK proof). Sample images live in `assets/`
(`example_064.png`, `example_128.png`, `example_256.png`). The blur kernel is a
compile-time 5×5 (`RADIUS` in `core/src/lib.rs`; the window sum is unrolled, so
changing it means updating the `unroll!` ranges too).

Outputs are written to the working directory: `blur.png`, `metrics.json`
(trace-cell metrics, for `openvm-riscv/scripts/basic_metrics.py`) and, with
`--apc > 0`, `apc_candidates.json` (for
`autoprecompiles/scripts/plot_effectiveness.py`). APC artifacts are cached under
`apc-cache/`. STARK proving is memory-intensive; use `--execute-only` on small
machines.

## Layout

- `core/` — `no_std` box blur, shared by guest (proven) and host (recomputed).
- `guest/` — the zkVM program: hash → blur → hash → reveal.
- `host/` — decodes the image and drives powdr.

## Troubleshooting

All commands assume the working directory is `examples/image-blur`.

- **Edited `guest/` or `core/` but it has no effect (or a `DeserializeUnexpectedEnd`
  panic).** The guest is compiled by the openvm SDK into its own cache at
  `guest/target`, separate from `host/target`. If the guest-build line finishes
  in ~0.0s, that cache is stale and was not rebuilt. Force it:
  ```sh
  rm -rf guest/target
  ```
- **`cargo run` won't recompile / `cargo clean` seems to do nothing.** The host is
  a detached workspace, so its artifacts live in `host/target`. A plain
  `cargo clean` cleans the parent powdr workspace instead. Target the host:
  ```sh
  cargo clean --manifest-path host/Cargo.toml   # or: touch host/src/main.rs
  ```
- **The proof gets killed / runs out of memory.** Full recursive STARK proving is
  memory-hungry. Use `--execute-only`, a smaller image, or a smaller `--radius`.
- **`--help` exits immediately.** clap prints usage and exits before any guest
  build or proving happens — that's expected, not a hang.
