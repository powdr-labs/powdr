# Installation

The only way to install powdr currently is to build it from source.
There are two binaries, 

- `powdr` compiles powdr-asm files to powdr-PIL and generates
witnesses and proofs.
- `powdr-rs` compiles Rust crates to powdr-asm via RISCV, and executes
powdr-asm code with given inputs.

## Prerequisites

You will need the [Rust](https://rust-lang.org) compiler and Cargo, the Rust package manager.
The easiest way to install both is with [`rustup.rs`](https://rustup.rs/).

On Windows, you will also need a recent version of [Visual Studio](https://visualstudio.microsoft.com/downloads/),
installed with the "Desktop Development With C++" Workloads option.

## Building *powdr*

Using a single Cargo command, enabling the Halo2 and Plonky3 backends:

```sh
cargo install --git https://github.com/powdr-labs/powdr --features halo2,plonky3 powdr-cli
```

With SIMD support for the provers that support it:

```sh
RUSTFLAGS='-C target-cpu=native' cargo install --git https://github.com/powdr-labs/powdr --features halo2,plonky3,plonky3-simd powdr-cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr-cli
cargo install --features halo2,plonky3 --path ./cli
# install powdr-cli with SIMD support (only for the crates that support it)
RUSTFLAGS='-C target-cpu=native' cargo install --features halo2,plonky3,plonky3-simd --path ./cli
```

## Building *powdr-rs*

Using a single Cargo command:

```sh
cargo install --git https://github.com/powdr-labs/powdr powdr-rs-cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr-rs-cli
cargo install --path ./cli-rs
```
