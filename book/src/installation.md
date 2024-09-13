# Installation

The only way to install powdr currently is to build it from source.
There are two binaries, `powdr` and `powdr-rs`.

The former can compile powdr-asm files to powdr-PIL, as well as generate
witnesses and proofs.

The latter compiles Rust crates to powdr-asm via RISCV, and can execute
powdr-asm code with given inputs.

## Prerequisites

You will need the [Rust](https://rust-lang.org) compiler and Cargo, the Rust package manager.
The easiest way to install both is with [`rustup.rs`](https://rustup.rs/).

On Windows, you will also need a recent version of [Visual Studio](https://visualstudio.microsoft.com/downloads/),
installed with the "Desktop Development With C++" Workloads option.

If you want to enable feature `estark-polygon`, you also need the following
runtime dependencies installed on the system:
- `gcc`
- `nlohmann-json3-dev`

You will also need the following build time dependencies:
- `make`
- `pkg-config`
- `libpqxx-dev` (Ubuntu) | `libpqxx` (Arch Linux)
- `nasm`

## Building *powdr*

Using a single Cargo command (enable the Halo2 & Plonky3 backends to use it with the cli):

```sh
RUSTFLAGS='-C target-cpu=native' cargo install --git https://github.com/powdr-labs/powdr --features halo2,plonky3 powdr-cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr-cli
RUSTFLAGS='-C target-cpu=native' cargo install --features halo2,plonky3 --path ./cli
```

Note that setting `RUSTFLAGS='-C target-cpu=native'` builds a binary optimized for the host CPU.

## Building *powdr-rs*

Using a single Cargo command:

```sh
RUSTFLAGS='-C target-cpu=native' cargo install --git https://github.com/powdr-labs/powdr powdr-rs-cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr-rs-cli
RUSTFLAGS='-C target-cpu=native' cargo install --path ./cli-rs
```
