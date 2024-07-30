My best shot at compiling this so far is:

```
RUSTFLAGS="-C link-arg=-Tmemory.x -C link-arg=-Tlink.x -C passes=loweratomic" cargo build -Zbuild-std=std,panic_abort -Zbuild-std-features=default,compiler-builtins-mem -r --target=riscv32im-risc0-zkvm-elf
```

The explanation for the more exotic options we are using:

`-C link-arg=-Tmemory.x -C link-arg=-Tlink.x`: tells the linker to use these two
linker scripts, in this order, as per required by `riscv-rt` documentation. The
`memory.x`, located at the root of the present crate, indicates the memory
layout of Powdr. `link.x` is provided by the `riscv-rt` crate and configures the
final binary disposition of the program.

`-C passes=loweratomic`: risc0 target does not support atomic instructions. When
they are needed, LLVM makes calls to software emulation functions it expects to
exist, such as `__atomic_fetch_add_4`, etc. This option adds an LLVM pass that
converts atomic calls into non-atomic variants, so that these functions are not
need anymore. It works because we have a single-threaded non-interrupting
implementation.

`-Zbuild-std=std,panic_abort`: there are no pre-packaged builds of standard
libraries for risc0 target, so we have to instruct cargo to build the ones we
will be using.

`-Zbuild-std-features=default,compiler-builtins-mem`: rust's `std` has features
that can be enabled or disabled, like any normal rust crate. We are telling that
we need the default features, but also we need to build and use the memory
related functions from `compiler_builtins` crate, which provides `memcpy` and
family for systems that doesn't already have them, like ours, as LLVM assumes
these functions are provided. We were already using `compiler_builtins` in
Powdr, but they are automatically enabled in `#[no_std]` crates.
