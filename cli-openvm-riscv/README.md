# cli-openvm-riscv

Command-line interface for the powdr OpenVM RISC-V workflow. Subcommands are
ordered by pipeline stage; each command runs all stages up to its own.

| Command         | Stages run                                                     |
| --------------- | -------------------------------------------------------------- |
| `generate-apcs` | Profile + build & rank APC candidates                          |
| `select-apcs`   | … + trim the ranking to `--autoprecompiles` (after `--skip`)   |
| `setup`         | … + assemble the program with selected APCs                    |
| `execute`       | … + run the guest in interpreted mode                          |
| `prove`         | … + STARK proof, optionally with `--recursion` (compression)   |

Stage outputs are persisted only when `--artifacts-dir` is set (see below).

## Generate vs. select

`generate-apcs` does most of what `select-apcs` used to do — it builds APC
candidates **and** ranks them. The ranking strategy depends on `--pgo`:

- `--pgo cell` (default): build all eligible candidates and rank them by a
  greedy density score that updates as candidates are picked.
- `--pgo instruction`: rank by `execution_frequency × n_instructions`
  (descending), then build the top of the ranking.
- `--pgo none`: rank by `n_instructions` (descending), then build the top.

`select-apcs` is then a pure trim of that ranking: `skip(--skip).take(--autoprecompiles)`.

### `--apc-candidates`

`--apc-candidates N` caps how many candidates `generate-apcs` builds (and
hence the ranking length).

- `--pgo cell` **always builds every eligible candidate**. A user-set
  `--apc-candidates` is logged with a warning and otherwise ignored — Cell's
  dynamic density ranking needs the full post-opt cost of every candidate.
- `--pgo instruction|none`:
  - Standalone `generate-apcs <guest>`: unset = build all.
  - Fused pipeline (`select-apcs` and beyond): unset defaults to
    `--autoprecompiles + --skip`. Setting it explicitly lets you over-build
    for later selection sweeps.

Each command accepts the arguments of its own stage plus all preceding stages.
For example, `prove` takes everything `setup` takes plus `--mock`,
`--recursion`, `--metrics`, and `--input`.

`--profile-input` (on the profile stage) and `--input` (on `execute` / `prove`)
are deliberately separate: `--profile-input` drives the execution profile used
to pick which basic blocks to accelerate; `--input` is the actual runtime
stdin for the interpreted run or the proof. Splitting them lets you re-prove
the same guest with many runtime inputs without re-running the
profile/generate/select/setup pipeline when combined with `--artifacts-dir`.

## Caching

Pass `--artifacts-dir <DIR>` (global) to persist each stage's output and reuse
it on matching reruns. The cache key for each stage hashes that stage's own
argument struct plus a hash of the transpiled guest `VmExe`, so:

- changing a later-stage flag (`--mock`, `--recursion`, `--input`, `--metrics`)
  does not invalidate earlier-stage caches, but
- editing the guest source (or anything else that changes the built ELF) does
  invalidate every cache that depends on it.

Cache stages: `generate → select → setup`. Sweeping `--autoprecompiles`
under `--pgo cell` re-runs only the (cheap) `select` stage; the `generate`
artifact is reused.

The hash is intentionally unstable across Rust/dep upgrades — expect the cache
to refill after a toolchain bump.

## Examples

For benchmark numbers, prepend `RUSTFLAGS='-C target-cpu=native'` to each
command. Omitted in the examples below for brevity.

```sh
# Compile + prove a guest with 10 autoprecompiles selected via cell-PGO.
cargo run -r --bin powdr_openvm_riscv prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 10 --pgo cell

# Mock-prove (verifies constraints without generating a STARK proof).
cargo run -r --bin powdr_openvm_riscv prove guest-keccak \
    --profile-input 100 --input 100 --autoprecompiles 1 --mock

# Assemble the compiled program (cached under --artifacts-dir if set).
cargo run -r --bin powdr_openvm_riscv setup guest-keccak \
    --profile-input 100 --autoprecompiles 10
```

Set `RUST_LOG=info` for high-level progress, `RUST_LOG=debug` for benchmarks.
