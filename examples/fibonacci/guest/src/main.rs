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
    // postcard is an overkill for this example, but it is serde compatible
    // and can be used for structs, arrays and other complex types.
    let n: u32 = postcard::from_bytes(reader.next().unwrap()).unwrap();
    powdr_riscv_runtime::print!("n = {n}\n");
    let r = fib(n);
    // Write result to stdout.
    write(1, r);
    // Commit the result as a public.
    commit::commit(r);
}
