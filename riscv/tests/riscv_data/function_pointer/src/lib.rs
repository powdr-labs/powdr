#![no_std]

use powdr_riscv_runtime::input::get_prover_input;

// Never inline to make sure the function is not optimized away, and public to
// make sure op argument is not removed, forcing a function pointer to be loaded
// and passed.
#[inline(never)]
pub fn apply_op(op: fn(u32, u32) -> u32, a: u32, b: u32) -> u32 {
    op(a, b)
}

fn sub(a: u32, b: u32) -> u32 {
    a - b
}

#[no_mangle]
fn main() {
    let a = get_prover_input(0);
    let b = get_prover_input(1);
    let expected = get_prover_input(2);

    // As of this writing, this will generate a dynamic load of sub's label into
    // a0 register. If we had other functions to choose dynamically depending on
    // input, their labels would be placed on a jump table in a data section,
    // which is a more common case that we want to avoid in this test.
    let result = apply_op(sub, a, b);

    assert_eq!(result, expected);
}
