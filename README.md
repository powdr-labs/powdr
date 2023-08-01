# powdr

[![Matrix Chat](https://img.shields.io/badge/Matrix%20-chat-brightgreen?style=plastic&logo=matrix)](https://matrix.to/#/#powdr:matrix.org)
[![Twitter Follow](https://img.shields.io/twitter/follow/powdr_labs?style=plastic&logo=twitter)](https://twitter.com/powdr_labs)

*powdr* is an extended polynomial identity (PIL) and zk-focused assembly (zkASM)
language written in Rust, focused on modularity and excellent developer experience.

WARNING: This is a proof-of-concept implementation. It is missing many internal checks. DO NOT USE FOR PRODUCTION!

## Basic Concept

*powdr* is a toolkit that helps build zkVMs and similar proof frameworks.

It has two main components:

- A polynomial identity language that allows you to define polynomial constraints, lookups, etc.
- An extensible assembly language to perform dynamic executions.
  
Both frontend and backend are highly flexible.

As an example, *powdr* contains a frontend that enables you to write code in (no-std) Rust,
which is compiled to RISCV, then to powdr-asm and finally to PIL.

*powdr*-PIL can be used to generate proofs using multiple backends, such as:

- Halo2
- eSTARKs: *powdr*-PIL is fully compatible with the eSTARKS backend from Polygon Hermez,
  although not yet fully integrated in an automatic way.
- Nova: ongoing work, should be ready after soon.
- other STARKs: maybe?

All stages are fully automatic, which means you do not need to write any
additional code for witness generation besides your Rust code. All witnesses
are automatically inferred from the constraints. Since the witnesses are
inferred, *powdr* can ensure that the system is not underconstrained, i.e.
there are no additional unwanted witnesses.

All artifacts from the compilation pipeline are human-readable. This means you
can inspect the RISCV assembly files, the powdr-asm, and the PIL file.

The assembly language is designed to be extensible. This means that it does not have a single
native instruction. Instead, all instructions are user-defined and because of that,
it is easy to adapt *powdr* assembly to any VM.

### Notes on Efficiency

Currently, the code is extremely wasteful. It generates many unnecessary columns.
The idea is to first see if automatic witness generation is possible in general.
If this is confirmed, various optimizer stages will be built to reduce the
column (and row) count automatically.

### Project structure

For an overview of the project structure, run:
```
cargo doc --workspace --no-deps --open
```
