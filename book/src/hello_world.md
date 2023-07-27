# Hello World

Let's write [a minimal VM](https://github.com/powdr-labs/powdr/blob/main/test_data/asm/book/hello_world.asm) and generate a SNARK!

```
{{#include ../../test_data/asm/book/hello_world.asm}}
```

Then let's generate a proof of execution for the valid prover input `0` (since for `0 + 1 - 1 == 0`)

```console
powdr pil hello_world.asm --field bn254 --force --inputs 0 --prove-with halo2
```

We observe that a proof was created at `proof.bin`.
Now let's try for the invalid input `1`

```console
powdr pil hello_world.asm --field bn254 --force --inputs 1 --prove-with halo2
```

We observe that witness generation fails, and no proof is created.