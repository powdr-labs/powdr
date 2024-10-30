# Hello World on Ethereum with proof aggregation

As noted in the previous section, complex VMs can lead to large
Solidity verifiers that exceed the contract size limit on Ethereum. One
solution to that problem is to create proofs using the Poseidon transcript, as
we did in the [first example](./hello_world.md), and then use proof
recursion to create a proof that we know we will be able to verify on Ethereum.

A recursive SNARK works by generating a proof for a circuit that verifies
another proof. The circuit we are using here to prove our initial proof
recursively is [PSE's snark-verifier](https://github.com/privacy-scaling-explorations/snark-verifier/). This circuit is large enough
to be able to prove complex programs that were proven initial with the Poseidon
transcript, like our first example. Because of that our aggregation setup
`params` and `verification key` are going to be larger than before and take
longer to compute. The good news are that (i) we can use a pre-computed setup
from a previous ceremony, and (ii) the verification key only has to be computed
once per program.

First, we need a setup of "size" 2^22. This is the maximum execution trace length of the recursion circuit.

You can generate it using the command line below, or download a pre-computed
one [here](https://drive.google.com/file/d/1xG_O_KqooiRR3QJno3tXIiNELSB2AQMH/view?usp=drive_link).

This will take a couple minutes if you decide to compute it yourself:
```console
powdr setup 4194304 --backend halo2 --field bn254
```

We can re-use the new large `params.bin` for both initial and recursive proofs.
Let's start by re-computing our Poseidon proof like in the [first example](./hello_world.md),
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

We can now generate the recursive proof (this typically takes a few minutes and uses around 28gb RAM):
```console
powdr prove test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "snark_aggr" --params params.bin --vkey vkey.bin --vkey-app vkey_app.bin --proof hello_world_proof_app.bin
```

We have a proof! Note that it contains two fields, `proof` and
`publics`.  The `proof` object contains the binary encoding of the proof
points, and the `publics` object contains the public accumulator limbs that we
need in order to verify the recursive proof.

We can now verify the proof, using the `publics` object as input (your numbers will be different):
```console
powdr verify test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "snark_aggr" --params params.bin --vkey vkey.bin --proof hello_world_proof_aggr.bin --publics "269487626280642378794,9378970522278219882,62304027188881225691,811176493438944,234778270138968319485,3212529982775999134,171155758373806079356,207910400337448,188563849779606300850,155626297629081952942,194348356185923309508,433061951018270,34598221006207900280,283775241405787955338,79508596887913496910,354189825580534"
```

Since the goal of the recursive proof was to be able to verify it on Ethereum, let's do that!
```console
powdr export-verifier test_data/asm/book/hello_world.asm --field bn254 --backend halo2 --backend-options "snark_aggr" --params params.bin --vkey vkey.bin
```

A Solidity verifier is created in `verifier.sol`. The contract expects an array of the accumulators' limbs followed by a tightly packed proof, where each accumulator limb uses 32 bytes, and there is no function signature.

Note that this verifier can be used to verify any recursive proof that verifies exactly one Poseidon proof of the given circuit.