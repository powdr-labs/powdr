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
For example, `prove` takes everything `setup` takes plus `--mock`, `--recursion`,
`--metrics`, and `--input`.

`--profile-input` (on the profile stage) and `--input` (on `execute` / `prove`)
are deliberately separate: `--profile-input` drives the execution profile used
to pick which basic blocks to accelerate; `--input` is the actual runtime
stdin for the interpreted run or the proof. Splitting them lets you re-prove
the same guest with many runtime inputs without re-running the
profile/compile/setup pipeline (see `--artifacts-dir` below).

## Examples

```sh
# Compile + prove a guest with 10 autoprecompiles selected via cell-PGO
RUSTFLAGS='-C target-cpu=native' cargo run -p cli-openvm-riscv -r -- prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 10 --pgo cell

# Mock-prove (verifies constraints without generating a STARK proof)
RUSTFLAGS='-C target-cpu=native' cargo run -p cli-openvm-riscv -r -- prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 1 --mock

# Assemble the compiled program and write it to <guest>_compiled.cbor
RUSTFLAGS='-C target-cpu=native' cargo run -p cli-openvm-riscv -r -- setup guest-keccak \
    --profile-input 100 --autoprecompiles 10
```

## Caching with `--artifacts-dir`

`--artifacts-dir <DIR>` (global) persists each stage's output under
`<DIR>/<stage>/<hash>/artifact.cbor`. Reruns with matching arguments load from
disk. The hash key is `Debug(<stage args>)`; later-stage flags are not in the
earlier stages' hash, so changing them does not invalidate the earlier caches.

For example, re-proving with a different runtime input reuses everything up to
setup:

```sh
# First run: warm the cache.
cargo run -p cli-openvm-riscv -r -- prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 10 --pgo cell \
    --artifacts-dir /tmp/powdr-cache

# Second run: only the proof step actually runs.
cargo run -p cli-openvm-riscv -r -- prove guest-keccak \
    --profile-input 100 --input 101 --autoprecompiles 10 --pgo cell \
    --artifacts-dir /tmp/powdr-cache
```

Hashes use `DefaultHasher` and `Debug` formatting, both of which are
intentionally unstable across Rust/dep upgrades — expect the cache to refill
after a toolchain bump.

Set `RUST_LOG=info` for high-level progress, `RUST_LOG=debug` for benchmarks.
