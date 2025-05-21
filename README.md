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
  It includes a functional meta-constraint language to describe how constraints are generated.
  
Both frontend and backend are highly flexible.

The [powdrVM](https://docs.powdr.org/quick_start_vm.html) is a zkVM
implementation that builds on *powdr*.
Users can write code in std Rust which is compiled to RISCV,
then to powdr-asm and finally to powdr-PIL.

*powdr*-PIL can be used to generate proofs using multiple backends, such as:

- [Plonky3](https://github.com/Plonky3/Plonky3)
- Halo2, using PSE's
    - [Halo2](https://github.com/privacy-scaling-explorations/halo2)
    - [snark-verifier](https://github.com/privacy-scaling-explorations/snark-verifier/)
    - [halo2-solidity-verifier](https://github.com/privacy-scaling-explorations/halo2-solidity-verifier)
- [Stwo](https://github.com/starkware-libs/stwo/) (under development)

All stages are fully automatic, which means you do not need to write any
additional code for witness generation besides your Rust code. All witnesses
are automatically inferred from the constraints. Since the witnesses are
inferred, *powdr* can ensure that the system is not underconstrained, i.e.,
there are no additional unwanted witnesses.

All artifacts from the compilation pipeline are human-readable.
You can inspect the powdr-asm IR, the compiled powdr-PIL file,
and its final optimized version.

The assembly language is designed to be extensible and does not have a single
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

## Contributing

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be
dual licensed as below, without any additional terms or conditions.

## License

This project is licensed under either of

<!-- markdown-link-check-disable -->
- [Apache License, Version 2.0](https://www.apache.org/licenses/LICENSE-2.0) ([`LICENSE-APACHE`](LICENSE-APACHE))
- [MIT license](https://opensource.org/licenses/MIT) ([`LICENSE-MIT`](LICENSE-MIT))
<!-- markdown-link-check-enable -->

at your option.
