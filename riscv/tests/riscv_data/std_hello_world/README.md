My best shot at compiling this so far is:

```
RUSTFLAGS="-C link-arg=--emit-relocs -C link-arg=-Tpowdr.x -C passes=lower-atomic -g" cargo build -Zbuild-std=std,panic_abort -Zbuild-std-features=default,compiler-builtins-mem -r --target=riscv32im-risc0-zkvm-elf
```

The explanation for the more exotic options we are using:

`-C link-arg=--emit-relocs`: this is a requirement from Powdr ELF translator, it
tells the linker to leave in the final executable the linkage relocation tables.
The ELF translator uses this information to lift references to text address into
labels in the Powdr assembly.

`-C link-arg=-Tpowdr.x`: tells the linker to use the `powdr.x` linker script,
provided by `powdr-riscv-runtime` crate. It configures things like memory layout
of the program and the entry point function.

`-C passes=loweratomic`: risc0 target does not support atomic instructions. When
they are needed, LLVM makes calls to software emulation functions it expects to
exist, such as `__atomic_fetch_add_4`, etc. This option adds an LLVM pass that
converts atomic instructions into non-atomic variants, so that the atomic
functions are not need anymore. It works because we have a single-threaded
non-interrupting implementation.

`-Zbuild-std=std,panic_abort`: there are no pre-packaged builds of standard
libraries for risc0 target, so we have to instruct cargo to build the ones we
will be using.

`-Zbuild-std-features=default,compiler-builtins-mem`: rust's `std` has features
that can be enabled or disabled, like any normal rust crate. We are telling that
we need the default features, but also we need to build and use the memory
related functions from `compiler_builtins` crate, which provides `memcpy`,
`memcmp`, etc, for systems that doesn't already have them, like ours, as LLVM
assumes these functions to be available. We also use `compiler_builtins` for
`#[no_std]` programs, but in there it is enabled by default.
