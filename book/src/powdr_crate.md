# Hello World using powdr as a library

Besides the CLI, powdr can also be used as a Rust library.
The [powdr crate](https://github.com/powdr-labs/powdr/tree/main/powdr)
exposes internal crates and data structures needed to compile code,
generate witnesses, and compute proofs.

Add `powdr` to your crate's dependencies:

```toml
[dependencies]
powdr = { git = "https://github.com/powdr-labs/powdr", branch = "main" }
```

The following Rust code has the same workflow as our previous "Hello World"
example. The full project can be found
[here](https://github.com/powdr-labs/powdr-hello-world), and as an example in
the powdr crate. To run the example in the [powdr
repository](https://github.com/powdr-labs/powdr), run:

```bash
cargo run --example powdr_crate_usage
```

You can also enable logs to know what is happening internally:

```bash
RUST_LOG=info cargo run --example powdr_crate_usage
```

```rust
{{#include ../../powdr-test/examples/hello_world.rs}}
```

```rust
{{#include ../../powdr-test/src/lib.rs}}
```
