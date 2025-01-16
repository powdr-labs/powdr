# Using publics

Public values are a small but important part of verifying ZK proofs. Often, the
verifier is interested in inputs and/or outputs to a public function.

In the toy example below, the prover can show that they know the square root of
a public value that is published with the proof.

You can also run this example directly in the [powdr
repository](https://github.com/powdr-labs/powdr):

```bash
cargo run --example sqrt_with_public
```

You can also enable logs to know what is happening internally:

```bash
RUST_LOG=info cargo run --example sqrt_with_public
```

```rust
{{#include ../../test_data/asm/sqrt_with_public.asm}}
```

This example uses a small VM with `jump` and a `square` instructions.  The
program reads the private input from the prover, squares it, and enters an
infinite loop to ensure that all the remaining rows are filled with the result of `A^2`.
Since the length of our execution trace is fixed and equals 8, we can tag the
8-th row of `A` (`A[7]`) as the publicly exposed number.

Let's run all steps needed to generate and verify a proof that 3<sup>2</sup> = 9:

1. Setup step:

```console
powdr setup 8 --backend halo2 --field bn254
```

2. Witness generation:

```console
powdr pil test_data/asm/sqrt_with_public.asm --field bn254 -i 3
```

3. Verification Key generation:

```console
powdr verification-key test_data/asm/sqrt_with_public.asm --field bn254 --backend halo2 --params params.bin
```

4. Proof generation:

```console
powdr prove test_data/asm/sqrt_with_public.asm --field bn254 --backend halo2 --params params.bin --vkey vkey.bin
```

5. Proof verification:

```console
powdr verify test_data/asm/sqrt_with_public.asm --field bn254 --backend halo2 --params params.bin --vkey vkey.bin --proof sqrt_with_public_proof.bin --publics 9
```

