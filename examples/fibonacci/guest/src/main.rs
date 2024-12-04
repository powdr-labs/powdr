use powdr_riscv_runtime;
use powdr_riscv_runtime::io::{read, write};

fn fib(n: u32) -> u32 {
    if n <= 1 {
        return n;
    }
    fib(n - 1) + fib(n - 2)
}

fn main() {
    // Read input from stdin.
    let n: u32 = read(0);
    let r = fib(n);
    // Write result to stdout.
    write(1, r);
}
