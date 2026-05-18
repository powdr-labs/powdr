# cli-openvm-riscv

Command-line interface for the powdr OpenVM RISC-V workflow. Subcommands are
ordered by pipeline stage; each command runs all stages up to its own.

| Command         | Stages run                                                     |
| --------------- | -------------------------------------------------------------- |
| `generate-apcs` | Profile + empirical constraints (stub for build APCs)          |
| `compile`       | Profile + build APCs + selection                               |
| `setup`         | … + assemble the program with selected APCs                    |
| `execute`       | … + run the guest in interpreted mode                          |
| `prove`         | … + STARK proof, optionally with `--recursion` (compression)   |

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
