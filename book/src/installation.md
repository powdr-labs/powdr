# Installation

The only way to install powdr currently is to build it from source.

## Prerequisites

You will need the [Rust](https://rust-lang.org) compiler and Cargo, the Rust package manager.
The easiest way to install both is with [`rustup.rs`](https://rustup.rs/).

On Windows, you will also need a recent version of [Visual Studio](https://visualstudio.microsoft.com/downloads/),
installed with the "Desktop Development With C++" Workloads option.

## Building

Using a single Cargo command:

```sh
cargo install --git https://github.com/powdr-labs/powdr powdr_cli
```

Or, by manually building from a local copy of the [powdr repository](https://github.com/powdr-labs/powdr):

```sh
# clone the repository
git clone https://github.com/powdr-labs/powdr.git
cd powdr
# install powdr_cli
cargo install --path ./powdr_cli
```
