# Hello World using the CLI

Let's generate a proof of execution for the valid prover input `0` (since `0 + 1 - 1 == 0`)

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --inputs 1 --prove-with halo2
```

We observe that several artifacts are created in the current directory:
- `hello_world.pil`: the compiled PIL file.
- `hello_world_opt.pil`: the optimized PIL file.
- `hello_world_constants.bin`: the computed fixed columns which only have to be computed once per PIL file.
- `hello_world_commits.bin`: the computed witness which needs to be computed for each proof.
- `hello_world_proof.bin`: the ZK proof!

> Note that the output directory can be specified with option `-o|--output`, and `.` is used by default.

Now let's try for the invalid input `1`:

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --inputs 2 --prove-with halo2
```

In this case witness generation fails, and no proof is created.

# Setup & Verification

The example above omits some important steps in proof generation: Setup and
Verification key generation. Some proof systems such as Halo2 , require a Setup to be performed before the proof. Such Setup can be
specific to the program or universal, where its artifact is a binary usually
called `parameters` or `params`. STARKs do not require a Setup.

Another step required before the proof is computed is key generation. A
`proving key` and a `verification key` are generated taking into account the
constraints and potentially Setup parameters. The `proving key` is used by the
prover to generate the proof, and the verification key is used by the verifier
to verify such a proof. A single verification key can be used to verify any
number of different proofs for a given program.

Therefore, when computing a proof, it is important that the Setup parameters
and the verification key are available as artifacts.

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

> The command above can read previously generated constants from the directory
specified via `-d|--dir`, where `.` is used by default. If the constants are not present
it computes them before generating the verification key.

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

Another aspect that was omitted in this example is the fact that this proof
uses a Poseidon transcript and cannot be verified in a cheap way on Ethereum,
even though we can verify it efficiently via powdr.
There are two ways to enable verification on Ethereum:

1. Use a different transcript when generating this proof. See section
   [Hello World on Ethereum](./hello_world_ethereum.md) for the same example targeting EVM verification.
2. Use proof aggregation to compress the proof using a
   circuit that can be verified on Ethereum. See section
   [Hello World on Ethereum via proof aggregation](./hello_world_ethereum_aggregation.md) to learn how to do that.
