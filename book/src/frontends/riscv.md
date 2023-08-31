# RISCV

A [RISCV](https://riscv.org/technical/specifications/) frontend for powdr is already available.

## How to run the Rust-RISCV example

```sh
# Install the riscv target for the rust compiler
rustup target add riscv32imac-unknown-none-elf
# Run the compiler. It will generate files in /tmp/.
# -i specifies the prover witness input (see below)
powdr rust riscv/tests/riscv_data/sum.rs -o /tmp -f -i 10,2,4,6 
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
and can then ask for e.g. the value of a storage slot at a certain address or the root hash of a Merkle tree.
