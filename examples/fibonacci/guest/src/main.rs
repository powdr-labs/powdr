use powdr_riscv_runtime;
use powdr_riscv_runtime::commit;
use powdr_riscv_runtime::io::{ProverDataReader, write};

fn fib(n: u32) -> u32 {
    if n <= 1 {
        return n;
    }
    fib(n - 1) + fib(n - 2)
}

fn main() {
    // Read input from prover data.
    let mut reader = ProverDataReader::new();
    let n: u32 = reader.next().unwrap();
    println!("n = {}", n);
    let r = fib(n);
    // Write result to stdout.
    write(1, r);
    // Commit the result as a public.
    commit::commit(r);
}
