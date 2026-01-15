# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

powdr is a zkVM enhancement toolkit that provides autoprecompiles (automated synthesis of guest-specific precompiles) and a constraint solver. The codebase is tightly integrated with OpenVM and stark-backend from powdr-labs forks, providing autoprecompiles for the RISC-V zkVM OpenVM.

## Build Commands

```bash
# Build the workspace (CPU)
cargo build --all-targets --features metrics

# Build with GPU support
cargo build --all-targets --features cuda,metrics

# Check compilation
cargo check --all-targets

# Format code
cargo fmt --all

# Lint
cargo clippy --all --all-targets --features metrics -- -D warnings
```

## Testing

```bash
# Run all default tests
cargo nextest run

# Run a single test
cargo nextest run <test_name>

# Run ignored (longer) tests
cargo nextest run --run-ignored only

# Run only large tests
cargo nextest run -E 'test(_large)' --run-ignored only

# Run tests in specific package
cargo nextest run -p powdr-openvm
```

## CLI Usage

The main CLI is `powdr-openvm` (in `cli-openvm/`):

```bash
# Compile a guest program with autoprecompiles
cargo run -p cli-openvm -- compile guest-keccak --autoprecompiles 10 --pgo instruction --input 100

# Execute a compiled program
cargo run -p cli-openvm -- execute guest-keccak --autoprecompiles 10 --input 100

# Prove (generate ZK proof)
cargo run -p cli-openvm -- prove guest-keccak --autoprecompiles 1 --input 10

# Mock prove (debug mode, verifies constraints without full proof)
cargo run -p cli-openvm -- prove guest-keccak --mock --autoprecompiles 1 --input 10
```

## Architecture

### Core Crates

- **autoprecompiles** (`autoprecompiles/`): The main precompile synthesis engine. Analyzes basic blocks of agnostic assembly instructions and synthesizes optimized circuits (APCs - Autoprecompiles). Key modules:
  - `optimizer.rs`: Constraint optimization pipeline
  - `constraint_optimizer.rs`: Eliminates redundant constraints
  - `symbolic_machine_generator.rs`: Converts instruction sequences to symbolic machines
  - `pgo/`: Profile-guided optimization for APC selection

- **constraint-solver** (`constraint-solver/`): Algebraic constraint analysis and solving. Provides:
  - `grouped_expression.rs`: Expression representation for efficient manipulation
  - `indexed_constraint_system.rs`: Efficient constraint system indexing
  - `range_constraint.rs`: Range analysis for variables
  - `inliner.rs`: Constraint inlining with degree bounds

- **openvm** (`openvm/`): OpenVM integration layer. Connects powdr optimizations to the OpenVM zkVM:
  - `customize_exe.rs`: Modifies OpenVM executables to use APCs
  - `powdr_extension/`: OpenVM circuit extension for APCs
  - `trace_generation.rs`: Generates execution traces for proving

### Supporting Crates

- **expression** (`expression/`): Core algebraic expression types (`AlgebraicExpression`, operators)
- **number** (`number/`): Field element abstractions
- **riscv-elf** (`riscv-elf/`): ELF file parsing for RISC-V binaries
- **cli-openvm** (`cli-openvm/`): Command-line interface

### Guest Programs

Example guest programs in `openvm/guest-*` directories (keccak, sha256, ecc, pairing, etc.) are used for testing and benchmarking.

## Key Concepts

- **APC (Autoprecompile)**: An optimized circuit for a basic block of assembly instructions (often RISC-V)
- **PGO (Profile-Guided Optimization)**: Uses execution profiling to select which basic blocks to optimize
  - `PgoConfig::Cell`: Optimizes based on total cell count savings
  - `PgoConfig::Instruction`: Optimizes based on instruction execution frequency
- **Symbolic Machine**: Intermediate representation of constraints and bus interactions
- **Bus Interactions**: Communication between different chips/machines in the OpenVM architecture

## Rust Toolchain

Uses nightly-2025-10-01. The `rust-toolchain.toml` sets this automatically.
