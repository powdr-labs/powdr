# cli-openvm

Use command `execute` to run the program only, and `prove` to prove.
The `prove` command has a `mock` option to only check the constraints.

Examples:

```sh
# Run the original program
RUSTFLAGS='-C target-cpu=native' cargo run -r execute guest
# Prove the original program
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest
# Check the constraints and witness of the original program
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest --mock
# Run the program with autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r execute guest --skip 37 --autoprecompiles 1
# Run the program with optimized autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r execute guest --skip 37 --autoprecompiles 1 --optimize
# Prove the program with autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest --skip 37 --autoprecompiles 1
# Prove the program with optimized autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest --skip 37 --autoprecompiles 1 --optimize
# Check the constraints and witness of the program with autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest --skip 37 --autoprecompiles 1 --mock
# Check the constraints and witness of the program with optimized autoprecompiles
RUSTFLAGS='-C target-cpu=native' cargo run -r prove guest --skip 37 --autoprecompiles 1 --mock --optimize
```

It is recommended to use at least `RUST_LOG=info` for information, and `RUST_LOG=debug` for benchmarks.

