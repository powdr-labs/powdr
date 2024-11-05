use powdr_riscv_runtime;
use powdr_riscv_runtime::io::read;

fn fib(n: u32) -> u32 {
    if n <= 1 {
        return n;
    }
    fib(n - 1) + fib(n - 2)
}

fn main() {
    let n: u32 = read(1);
    let _ = fib(n);
}
