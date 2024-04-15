# Hello World

Let's write [a minimal VM](https://github.com/powdr-labs/powdr/blob/main/test_data/asm/book/hello_world.asm) and generate a SNARK!

```
{{#include ../../test_data/asm/book/hello_world.asm}}
```

Then let's generate a proof of execution for the valid prover input `0` (since for `0 + 1 - 1 == 0`)

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --inputs 0 --prove-with halo2
```

We observe that a proof was created at `hello_world_proof.bin`.
Now let's try for the invalid input `1`

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --inputs 1 --prove-with halo2
```

We observe that witness generation fails, and no proof is created.

# Setup & Verification

The example above omits some important steps in proof generation: Setup and
Verification key generation.  Some proof systems, such as Halo2 (and other
SNARKs), require a Setup to be performed before the proof. Such Setup can be
specific to the program or universal, where its artifact is a binary usually
called `parameters` or `params`. STARKs do not require a previous Setup.

Another step required before the proof is computed is key generation. A
`proving key` and a `verification key` are generated taking into account the
constraints and potentially Setup parameters. The `proving key` is used by the
prover to generate the proof, and the verification key is used by the verifier
to verify such proof. A single verification key can be used to verify any
number of different proofs for a given program.

Therefore, when computing a proof, it is important that the Setup parameters
and the verification key are also available as artifacts.

Below we reproduce the same proof as in the example above, keeping the
artifacts needed for verification:

First we run the Setup, where the number given must match the `degree` of our
source file (`degree 8`). The command below generates file `params.bin`.

```console
powdr setup 8 --backend halo2 --field bn254
```

We can now compute the verification key, output in `vkey.bin`:

```console
powdr verification-key test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --params "params.bin"
```

The next command compiles and optimizes the given source, generating the file
`hello_world_opt.pil`. It also computes both the fixed data and the witness
needed for the proof, stored respectively in `hello_world_constants.bin` and
`hello_world_commits.bin`.

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --force --inputs 0
```

We can now generate the proof:

```console
powdr prove test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --params "params.bin" --vkey "vkey.bin"
```

The proof can be verified by anyone via:

```console
powdr verify test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --vkey "vkey.bin" --params "params.bin" --proof "hello_world_proof.bin"
```

> Note that CLI proof verification works analogously for eSTARK, without the setup step and using the Goldilocks field instead of Bn254.