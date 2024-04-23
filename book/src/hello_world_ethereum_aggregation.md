# Hello World on Ethereum via proof aggregation

This example is yet another variation of the previous `Hello World` (TODO
link), still targeting verification on Ethereum but supporting more complex
programs. As noted in the previous section, complex programs can lead to large
Solidity verifiers that exceed the contract size limit on Ethereum. One
solution to that problem is to create proofs using the Poseidon transcript, as
we did in the first example (TODO put link/ref here), and then use proof
recursion to create a proof that we know we will be able to verify on Ethereum.

A recursive SNARK works by generating a proof for a circuit that verifies
another proof. The circuit we are using here to prove our initial proof
recursively is [PSE's snark-verifier](https://github.com/privacy-scaling-explorations/snark-verifier/). This circuit is large enough
to be able to prove complex programs that were proven initial with the Poseidon
transcript, like our first example. Because of that our aggregation setup
`params` and `verification key` are going to be larger than before and take
longer to compute.  The good news are that (i) we can use a pre-computed setup
from a previous ceremony, and (ii) the verification key only has to be computed
once per program.

First, we need a setup of "size" 22. This is the log2 of the maximum execution
trace length of the recursion circuit.

You can generate it using the command line below, or download a pre-computed
one [here](https://drive.google.com/file/d/1xG_O_KqooiRR3QJno3tXIiNELSB2AQMH/view?usp=drive_link).

This will take a couple minutes if you decide to compute it yourself:

```console
powdr setup 22 --backend halo2 --field bn254
```

We can re-use the new large `params.bin` for both initial and recursive proofs.
Let's start by re-computing our Poseidon proof like in the first example (TODO: link),
but using our new setup file:

```console
powdr pil test_data/asm/book/hello_world.asm --field bn254 --inputs 0 --prove-with halo2 --backend-options "poseidon" -f --params params.bin
```

This generates the initial proof `hello_world_proof.bin`.
We'll also need a verification key:

```console
powdr verification-key test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "poseidon" --params params.bin
```

Let's verify the proof with our fresh verification key to make sure we're on the right track:

```console
powdr verify test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "poseidon" --params params.bin --vkey vkey.bin --proof hello_world_proof.bin
```

In order to avoid confusion between the application's artifacts
that we've just generated and the recursion one we're going to generate
later, let's rename them:

```console
mv vkey.bin vkey_app.bin
mv hello_world_proof.bin hello_world_proof_app.bin
```

We can now generate a verification key for the Halo2 circuit that verifies our proofs recursively. This might take up to a minute.

```console
powdr verification-key test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "snark_aggr" --params params.bin --vkey-app vkey_app.bin
```

> Note that this verification key can only be used to verify recursive proofs that verify other proofs using the application's key `vkey_app.bin`.

We can now generate the recursive proof (can take 3 or more minutes and use 28gb RAM):

```console
powdr prove test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "snark_aggr" --params params.bin --vkey vkey.bin --vkey-app vkey_app.bin --proof hello_world_proof_app.bin
```