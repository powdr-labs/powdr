# RISCV and ZK-Continuations

ZK-continuations can be used to make proofs for unbounded execution traces of
Rust programs, where the trace is split into many different `chunks`. A proof
is computed for each chunk, and all proofs can be combined with
recursion/aggregation until a single proof remains.

For the details of how memory is handled in the ZK-continuations case please
see [this](https://github.com/powdr-labs/powdr/issues/814).

`powdr-rs` has experimental support to ZK-continuations, which can be used as follows.

Let's use as example test `many chunks` from the `riscv` crate:

```rust
{{#include ../../../riscv/tests/riscv_data/many_chunks/src/main.rs}}
```

First we need to compile the Rust code to powdr-asm:
```console
powdr-rs compile riscv/tests/riscv_data/many_chunks
```

Now we can use powdr's RISCV executor to estimate how many cycles are needed:
```console
powdr-rs execute many_chunks.asm
```
```console
...
Execution trace length: 750329
```

By default, powdr-RISCV uses chunks of length 2^18. That means we will need at
least 3 chunks.

For the continuations case, the compiled assembly code looks different because
of the external memory commitments, so we need to recompile using the
`--continuations` flag:
```console
powdr-rs compile riscv/tests/riscv_data/many_chunks --continuations
```

We can now execute the program with continuations enabled:
```console
powdr-rs -- execute many_chunks.asm --continuations
```

The output now is longer:
```console
Running chunk 0...
Building bootloader inputs for chunk 0...
26 unique memory accesses over 2 accessed pages: {31, 32}
Estimating the shutdown routine to use 1362 rows.
Bootloader inputs length: 1285
Simulating chunk execution...
Initial memory root hash: 44cd91c12033ad4c6a6b19793b73f1a66d99a0e0bf63494c12ceb1f451ec9452
Final memory root hash: e6a6fd63d864c734d6a4df4988f733738790262af9c7b014a1c036679baf1125
Chunk trace length: 260782
Validating chunk...
Bootloader used 3134 rows.
  => 257648 / 262144 (98%) of rows are used for the actual computation!
Proved 257647 rows.

Running chunk 1...
Building bootloader inputs for chunk 1...
0 unique memory accesses over 0 accessed pages: {}
Estimating the shutdown routine to use 42 rows.
Bootloader inputs length: 83
Simulating chunk execution...
Initial memory root hash: e6a6fd63d864c734d6a4df4988f733738790262af9c7b014a1c036679baf1125
Final memory root hash: e6a6fd63d864c734d6a4df4988f733738790262af9c7b014a1c036679baf1125
Chunk trace length: 262102
Validating chunk...
Bootloader used 62 rows.
  => 262040 / 262144 (99%) of rows are used for the actual computation!
Proved 262039 rows.

Running chunk 2...
Building bootloader inputs for chunk 2...
2 unique memory accesses over 1 accessed pages: {31}
Estimating the shutdown routine to use 702 rows.
Bootloader inputs length: 684
Simulating chunk execution...
Initial memory root hash: e6a6fd63d864c734d6a4df4988f733738790262af9c7b014a1c036679baf1125
Final memory root hash: e6a6fd63d864c734d6a4df4988f733738790262af9c7b014a1c036679baf1125
Chunk trace length: 232057
Validating chunk...
Bootloader used 1610 rows.
  => 259832 / 262144 (99%) of rows are used for the actual computation!
Done!
```

The step above is informational, but for the proofs we also need the full witness:
```console
powdr-rs execute many_chunks.asm --witness
```

The witnesses are written in `./chunk_0/commits.bin`, `./chunk_1/commits.bin`
and `./chunk_2/commits.bin`.

Now that we have the witnesses for all chunks we can use `powdr` (instead of
`powdr-rs`) to compute proofs for each chunk:

```console
powdr prove many_chunks.asm -d chunk_0 --backend mock
powdr prove many_chunks.asm -d chunk_1 --backend mock
powdr prove many_chunks.asm -d chunk_2 --backend mock
```

These proofs are mock proofs for the sake of the example, but any backend
should work here.

After generating real proofs, each specific proof system can be used for the
recursion/aggregation parts.  A follow-up tutorial on that is coming soon.
