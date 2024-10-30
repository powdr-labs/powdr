# powdrVM tinykeccak example

This example demonstrates how to use the `powdrVM` to run a simple keccak hash function using the `tinykeccak` crate.

We want to prove that we know the pre-image of a hash.
The host takes in a 32-byte challenge hash `C` as a hex string, and the pre-image string `P`, such that `Keccak(P) = C`.

The guest receives the same data from the host and verifies that the claim is true.

For a valid hash example, you can run:

```console
cargo run -r "56c73097b157bbe90a5b273a6bb93eb5e89ab1ac0364a73a4e107187c63f7256" "my powdr hash"
```
