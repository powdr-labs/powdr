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
For example, `prove` takes everything `setup` takes plus `--mock`, `--recursion`
and `--metrics`.

## Examples

```sh
# Compile and run the guest in interpreted mode
RUSTFLAGS='-C target-cpu=native' cargo run -r execute guest-keccak --input 100

# Compile + prove a guest with 10 autoprecompiles selected via cell-PGO
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest-keccak \
    --input 100 --autoprecompiles 10 --pgo cell

# Mock-prove (verifies constraints without generating a STARK proof)
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest-keccak \
    --input 100 --autoprecompiles 1 --mock

# Assemble the compiled program and write it to <guest>_compiled.cbor
RUSTFLAGS='-C target-cpu=native' cargo run -r setup guest-keccak \
    --input 100 --autoprecompiles 10
```

Set `RUST_LOG=info` for high-level progress, `RUST_LOG=debug` for benchmarks.
