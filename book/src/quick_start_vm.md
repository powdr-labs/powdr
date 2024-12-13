# Quick Start

First, create a new powdrVM project with

```console
cargo-powdr new my-host
```

- Your new crate `my-host` is the host of the virtual machine's execution,
and is responsible for preparing data and running the prover.
- The `guest` crate in your new project contains the Rust code whose execution will be proven.

Most of these details are abstracted by the `powdr` library.

Now that your project is set up, just run
```console
cargo run -r
```

The host manages a `powdr::Session` which can be used to share data
with the guest, test the execution, and generate ZK proofs.

```rust
{{#include ../../cargo-powdr/template/src/main.rs}}
```

The guest contains the custom logic that should be proved.

```rust
{{#include ../../cargo-powdr/template/guest/src/main.rs}}
```
