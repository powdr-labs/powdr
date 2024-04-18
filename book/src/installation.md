# Installation

The only way to install powdr currently is to build it from source.

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

## Building

Using a single Cargo command (enable the Halo2 backend to use it with the cli):

```sh
cargo install --git https://github.com/powdr-labs/powdr --features halo2 powdr-cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr-cli
cargo install --features halo2 --path ./cli
```
