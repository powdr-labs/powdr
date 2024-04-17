<p align="center">
  <img src="book/src/powdr_wires.png" width="600">
</p>

# powdr

[![Matrix Chat](https://img.shields.io/badge/Matrix%20-chat-brightgreen?style=plastic&logo=matrix)](https://matrix.to/#/#powdr:matrix.org)
[![Twitter Follow](https://img.shields.io/twitter/follow/powdr_labs?style=plastic&logo=twitter)](https://twitter.com/powdr_labs)<!-- markdown-link-check-disable-line -->

> WARNING: This codebase is experimental and has not been audited. DO NOT USE FOR PRODUCTION!

For detailed documentation please visit [the powdr book](https://docs.powdr.org/).

If you have any questions or want to contribute, feel free to write us in our [Matrix Chat](https://matrix.to/#/#powdr:matrix.org).

*powdr* is a toolkit that helps build zkVMs and similar proof frameworks.

It has two main components:

- powdr-asm: an extensible assembly IR language to perform dynamic executions.
- powdr-PIL: a low level constraint language that allows you to define arithmetic constraints, lookups, etc.
  
Both frontend and backend are highly flexible.

As an example, *powdr* contains a frontend that enables you to write code in (no-std) Rust,
which is compiled to RISCV, then to powdr-asm and finally to powdr-PIL.

*powdr*-pil can be used to generate proofs using multiple backends, such as:

- Halo2: via [polyexen](https://github.com/Dhole/polyexen) and [snark-verifer](https://github.com/privacy-scaling-explorations/snark-verifier/)
- eSTARK: via [Eigen's starky](https://github.com/0xEigenLabs/eigen-zkvm/)
- SuperNova: ongoing work (https://github.com/powdr-labs/powdr/pull/453)

All stages are fully automatic, which means you do not need to write any
additional code for witness generation besides your Rust code. All witnesses
are automatically inferred from the constraints. Since the witnesses are
inferred, *powdr* can ensure that the system is not underconstrained, i.e.
there are no additional unwanted witnesses.

All artifacts from the compilation pipeline are human-readable. This means you
can inspect the RISCV assembly files, the powdr-asm IR, and the compiled PIL file.

The assembly language is designed to be extensible. This means that it does not have a single
native instruction. Instead, all instructions are user-defined and because of that,
it is easy to adapt *powdr* assembly to any VM.

### Notes on Efficiency

The current focus of the project is VM support and developer experience.  The
compiler generates many unnecessary columns. We will soon start writing
optimizer steps that should bring performance closer to existing production
systems.

### Project structure

For an overview of the project structure, run:
```
cargo doc --workspace --no-deps --open
```
