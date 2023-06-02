# powdr

*powdr* is an extended polynomial identity (PIL) and zk-focused assembly (zkASM)
language written in Rust, focused on modularity and excellent developer experience.

WARNING: This is a proof-of-concept implementation. It is missing many internal checks. DO NOT USE FOR PRODUCTION!

## Basic Concept

*powdr* is a toolkit that helps build zkVMs and similar proof frameworks.

It has two main components:

- A polynomial identity language that allows you to define polynomial constraints, lookups, etc.
- An extensible assembly language to perform dynamic executions.
  
Both frontend and backend are highly flexible.

As an example, *powdr* contains a frontend that enables you to write code in (no-std) Rust,
which is compiled to RISCV, then to powdr-asm and finally to PIL.

*powdr*-PIL can be used to generate proofs using multiple backends, such as:

- Halo2
- eSTARKs: *powdr*-PIL is fully compatible with the eSTARKS backend from Polygon Hermez,
  although not yet fully integrated in an automatic way.
- Nova: ongoing work, should be ready after soon.
- other STARKs: maybe?

All stages are fully automatic, which means you do not need to write any
additional code for witness generation besides your Rust code. All witnesses
are automatically inferred from the constraints. Since the witnesses are
inferred, *powdr* can ensure that the system is not underconstrained, i.e.
there are no additional unwanted witnesses.

All artifacts from the compilation pipeline are human-readable. This means you
can inspect the RISCV assembly files, the powdr-asm, and the PIL file.

The assembly language is designed to be extensible. This means that it does not have a single
native instruction. Instead, all instructions are user-defined and because of that,
it is easy to adapt *powdr* assembly to any VM.

## How to run the Rust-RISCV example

```sh
# Install the riscv target for the rust compiler
rustup target add riscv32imc-unknown-none-elf
# Run the compiler. It will generate files in /tmp/.
# -i specifies the prover witness input (see below)
cargo run --release rust riscv/tests/riscv_data/sum.rs -o /tmp -f -i 10,2,4,6 
```

The following example Rust file verifies that a supplied list of integers sums up to a specified value.
Note that this is the full and only input file you need for the whole process!

```rust
#![no_std]

extern crate alloc;
use alloc::vec::Vec;

use runtime::get_prover_input;

#[no_mangle]
pub fn main() {
    // This is the sum claimed by the prover.
    let proposed_sum = get_prover_input(0);
    // The number of integers we want to sum.
    let len = get_prover_input(1) as usize;
    // Read the numbers from the prover and store them
    // in a vector.
    let data: Vec<_> = (2..(len + 2))
        .map(|idx| get_prover_input(idx as u32))
        .collect();
    // Compute the sum.
    let sum: u32 = data.iter().sum();
    // Check that our sum matches the prover's.
    assert_eq!(sum, proposed_sum);
}
```

The function `get_prover_input` reads a number from the list supplied with `-i`.

This is just a first mechanism to provide access to the outside world.
The plan is to be able to call arbitrary user-defined `ffi` functions that will translate to prover queries,
and can then ask for e.g. the value of a storage slot at a certain address or the
root hash of a merkle tree.

### Notes on Efficiency

Currently, the code is extremely wasteful. It generates many unnecessary columns.
The idea is to first see if automatic witness generation is possible in general.
If this is confirmed, various optimizer stages will be built to reduce the
column (and row) count automatically.

### Project structure

For an overview of the project structure, run:
```
cargo doc --workspace --no-deps --open
```
