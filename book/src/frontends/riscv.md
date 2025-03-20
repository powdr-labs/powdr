# RISCV

A [RISCV](https://riscv.org/specifications/ratified/) frontend for powdr is already available.

## How to run the Rust-RISCV example

```sh
# Install the riscv target for the rust compiler
rustup target add riscv32imac-unknown-none-elf
# Run the powdr-rs compiler. It will generate files in ./output/
powdr-rs compile riscv/tests/riscv_data/sum -o output
# Run powdr to compile powdr-asm to powdr-PIL and generate the witness
# -i specifies the prover witness input (see below)
powdr pil output/sum.asm -o output -f -i 10,2,4,6
```

The example Rust code verifies that a supplied list of integers sums up to a specified value.

```rust
{{#include ../../../riscv/tests/riscv_data/sum/src/main.rs}}
```

The function `read_u32` reads a number from the list supplied with `-i`.

This is just a first mechanism to provide access to the outside world.
The plan is to be able to call arbitrary user-defined `ffi` functions that will translate to prover queries,
and can then ask for e.g. the value of a storage slot at a certain address or the root hash of a Merkle tree.
