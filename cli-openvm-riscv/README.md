# cli-openvm-riscv

Command-line interface for the powdr OpenVM RISC-V workflow. Subcommands are
ordered by pipeline stage; each command runs all stages up to its own.

| Command         | Stages run                                                     | Output                       |
| --------------- | -------------------------------------------------------------- | ---------------------------- |
| `generate-apcs` | Profile + build/select APCs (alias for `compile` in this release) | `<guest>_apcs.cbor`       |
| `compile`       | Profile + build/select APCs                                    | `<guest>_apcs.cbor`          |
| `setup`         | … + assemble the program with selected APCs                    | `<guest>_compiled.cbor`      |
| `execute`       | … + run the guest in interpreted mode                          | side effect only             |
| `prove`         | … + STARK proof, optionally with `--recursion` (compression)   | side effect only             |

Each command accepts the arguments of its own stage plus all preceding stages.
For example, `prove` takes everything `setup` takes plus `--mock`,
`--recursion`, `--metrics`, and `--input`.

`--profile-input` (on the profile stage) and `--input` (on `execute` / `prove`)
are deliberately separate: `--profile-input` drives the execution profile used
to pick which basic blocks to accelerate; `--input` is the actual runtime
stdin for the interpreted run or the proof. Splitting them lets you re-prove
the same guest with many runtime inputs without re-running the
profile/compile/setup pipeline when combined with `--artifacts-dir`.

Pass `--artifacts-dir <DIR>` (global) to persist each stage's output and reuse
it on matching reruns. The cache key for each stage hashes only that stage's
own argument struct, so changing a later-stage flag (`--mock`, `--recursion`,
`--input`, `--metrics`) does not invalidate earlier-stage caches. The hash is
intentionally unstable across Rust/dep upgrades — expect the cache to refill
after a toolchain bump.

## Examples

For benchmark numbers, prepend `RUSTFLAGS='-C target-cpu=native'` to each
command. Omitted in the examples below for brevity.

```sh
# Compile + prove a guest with 10 autoprecompiles selected via cell-PGO
cargo run -r --bin powdr_openvm_riscv prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 10 --pgo cell

# Mock-prove (verifies constraints without generating a STARK proof)
cargo run -r --bin powdr_openvm_riscv prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 1 --mock

# Assemble the compiled program and write it to <guest>_compiled.cbor
cargo run -r --bin powdr_openvm_riscv setup guest-keccak \
    --profile-input 100 --autoprecompiles 10
```

Set `RUST_LOG=info` for high-level progress, `RUST_LOG=debug` for benchmarks.
