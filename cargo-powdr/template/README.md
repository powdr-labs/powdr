# powdrVM Usage Template

This is a foundational template for generating zero-knowledge proofs with powdrVM. You write the code to be proven as a guest program for the zkVM host. This template includes a structure for host/guest interaction, ZKP setup, and artifact generation.

Guest programs are written in Rust. When creating your guest program, you can write Rust code in the usual way, including using std and importing packages others have written. We provide some additional powdrVM specific functionalities via system calls, such as IO operations for host <-> guest communication and precompiles to accelerate complex programs via optimized circuits.

## Dependencies

- Rust/cargo

## Usage

This will run the host and generate ZK proofs.

```bash
cargo run -r
```

## AVX / Neon

You can enable AVX or Neon support by using the `simd` feature and running the host with extra flags:

```bash
RUSTFLAGS='-C target-cpu=native' cargo run --features simd -r
```

## Structure

- `src/main.rs`: the host code. This is where you create a powdr `Session`, prepare data to be shared with the guest, and run the prover.
- `guest`: this is the guest crate. It contains the code that will be run inside the powdrVM.
- `powdr-target`: this is where all generated artifacts reside. This includes the compiled guest code to powdr-asm, the compiled PIL constraints, setup artifacts such as proving and verifying keys, and the final ZK proofs.

## Troubleshooting

powdrVM uses a default chunk size of 2^20 cycles for production cases. In some machines, key generation can run out-of-memory. To solve this, use

```bash
export MAX_DEGREE_LOG=20
```

and use a chunk size of 2^18 in the host, as explained in the section below.
This is being improved at the moment.

## Workflow

Let's look at `src/main.rs` line by line:

Here we create some data we want to share with the guest:

```rust
let some_data = vec![1, 2, 3, 4, 5];
```

Create a new powdr session where we'll be running crate `guest` in powdrVM
and all artifacts will be stored in `powdr-target`:
```rust
let mut session = Session::new("./guest", "powdr-target")
```

Write `some_data` to channel 1 and the sum of `some_data` to channel 2.
Note that any `serde` type can be used to share data between host and guest.

The guest will read this data from the channels:
```rust
.write(1, &some_data).write(2, &some_data.iter().sum::<u32>());
```

The line below also creates a powdr `Session`, but tells powdrVM to use 2^18 rows
per chunk, if needed. In that case, make sure to also use `export MAX_DEGREE_LOG=20`.
This tells the powdr compiler to generate smaller circuits than the default production
size of 2^22. This is being improved at the moment.
```rust
let mut session = Session::new_with_chunk_size("./guest", "powdr-target", 18)
```

Run the session without generating a proof. Useful for testing the guest code:
```rust
session.run();
```

Generate the ZK proof:
```rust
session.prove();
```

Before generating a proof, powdrVM has to create the proving and verifying keys (setup) for the given guest program. When run for the first time, this can take a while. Subsequent runs will be faster as the setup only changes if the guest changes.

You can also run the host with INFO logs to have a deeper look at what's happening:
```bash
RUST_LOG=info cargo run -r
```
