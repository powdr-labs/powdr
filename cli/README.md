# Cargo OpenVM CLI

The `cargo openvm` CLI is useful for various tasks related to OpenVM, such as compiling programs, benching proofs of programs, and more.
It is taken from the openvm repo and extended to support powdr extensions.

## Development

To run the CLI locally, you can use the following command:

```bash
cargo run --bin cargo-openvm -- --help
```

### Installing the CLI locally from source

You can install the CLI locally from source by running the following command:

```bash
cd cli
cargo install --force --locked --path .
```

### Running the CLI after installing

After installing the CLI, you can run it by simply running the following command:

```bash
cargo openvm
```
