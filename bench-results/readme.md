# Bench results — 2026-06-03, CUDA (RTX 4090)

GPU rerun of the [Accelerating Ethereum with Autoprecompiles](https://powdr.org/blog/accelerating-ethereum-with-autoprecompiles)
benchmarks on the `bench-cuda-apc-sweep-2026-06` branch of powdr — powdr
`main` after the OpenVM 2.0 update plus
[powdr-labs/powdr#3763](https://github.com/powdr-labs/powdr/pull/3763)
(staged `generate → select → setup` pipeline with `--artifacts-dir` caching;
benched at `be183c60e`, scripts committed on top as `c2342ae6e` — the Rust
code is identical). Every guest is swept over {0, 3, 10, 30, 100, 300}
autoprecompiles; reth (openvm-eth, mainnet block 24171377) over
{0, 3, 10, 30, 100, 300, 500, 750, 1000}.

**ecc**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/ecc) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fecc%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fecc%2Fcandidates%2Fguest-ecc-powdr-affine-hint-input50%2Fapc_candidates.json)

**ecrecover**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/ecrecover) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fecrecover%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fecrecover%2Fcandidates%2Fguest-ecrecover-input20%2Fapc_candidates.json)

**keccak**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/keccak) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fkeccak%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fkeccak%2Fcandidates%2Fguest-keccak-input10000%2Fapc_candidates.json)

**matmul**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/matmul) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fmatmul%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fmatmul%2Fcandidates%2Fguest-matmul-input0%2Fapc_candidates.json)

**pairing**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/pairing) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fpairing%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fpairing%2Fcandidates%2Fguest-pairing-input0%2Fapc_candidates.json)

**reth_gpu**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/reth_gpu) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Freth_gpu%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Freth_gpu%2Fapc_candidates.json)

**sha256**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/sha256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fsha256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fsha256%2Fcandidates%2Fguest-sha256-input30000%2Fapc_candidates.json)

**u256**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results/u256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fu256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results%2Fu256%2Fcandidates%2Fguest-u256-input0%2Fapc_candidates.json)

## Environment

| | |
|---|---|
| CPU | AMD Ryzen 9 7950X (16C/32T) |
| RAM | 125 GiB |
| GPU | NVIDIA GeForce RTX 4090, 24 GiB, driver 570.195.03, CUDA 12.8 |
| OS | Linux 6.8.0-79-generic |
| powdr | `be183c60e` (+ bench-script commits of this branch) |
| openvm-eth | [`44564802`](https://github.com/powdr-labs/openvm-eth/commit/44564802f942d15c6dd782979d280f28065179b5) (ref from `.github/actions/patch-openvm-eth`), patched to local powdr crates + [`openvm-eth-bench.patch`](../openvm-riscv/scripts/openvm-eth-bench.patch) |
| toolchains | powdr host: `nightly-2025-10-01`; openvm-eth host + guests: `nightly-2026-01-18`; `cargo-openvm` v2.0.0-beta.2 (tag `v2.0.0-beta.2-powdr`, installed with Rust 1.91.1) |

Both proving paths run with `POWDR_OPENVM_SEGMENT_DELTA=50000` exported, as
the nightly workflow does for all jobs.

## How to reproduce

From a checkout of this branch with an NVIDIA GPU:

```sh
# One-time setup (mirrors .github/workflows/nightly-tests.yml)
rustup toolchain install nightly-2026-01-18 && rustup component add rust-src --toolchain nightly-2026-01-18
rustup toolchain install 1.91.1
cargo +1.91.1 install --git 'https://github.com/powdr-labs/openvm.git' --tag "v2.0.0-beta.2-powdr" cargo-openvm
python3 -m venv .venv && source .venv/bin/activate
pip install -r openvm-riscv/scripts/requirements.txt -r autoprecompiles/scripts/requirements.txt psrecord

export POWDR_OPENVM_SEGMENT_DELTA=50000

# Guest benchmarks -> results/{keccak,sha256,pairing,u256,matmul,ecc,ecrecover}
BENCH_FEATURES=metrics,cuda BENCH_KEEP_GOING=1 ./openvm-riscv/scripts/run_guest_benches.sh

# Reth sweep -> results/reth_gpu. Needs an Ethereum mainnet *archive* RPC
# for the first run only; the fetched witness is cached under
# openvm-eth/rpc-cache/ afterwards. This run used the free
# https://eth.drpc.org endpoint, which needs the fetch hardening from
# openvm-eth-bench.patch + smaller eth_getProof chunks (a paid/CI-grade
# endpoint like the nightly's RPC_1 does not):
export RPC_1=<archive rpc url>
export RPC_PROOF_CHUNK_SIZE=100
git clone https://github.com/powdr-labs/openvm-eth.git openvm-eth
git -C openvm-eth checkout 44564802f942d15c6dd782979d280f28065179b5
git -C openvm-eth apply ../openvm-riscv/scripts/openvm-eth-bench.patch
./openvm-riscv/scripts/run_reth_gpu_bench.sh   # reuses the existing checkout

# The 500-APC point needs raised aggregation caps (see Notes), and the
# limit probes beyond it fail on a 24 GiB card:
cd openvm-eth
./run.sh --cuda --block 24171377 --apc 500  --leaf-log-stacked-height 22 --internal-log-stacked-height 20 --mode prove-stark
./run.sh --cuda --block 24171377 --apc 750  --leaf-log-stacked-height 22 --internal-log-stacked-height 21 --mode prove-stark  # LayoutHeightExceeded(23 > 22)
./run.sh --cuda --block 24171377 --apc 1000 --leaf-log-stacked-height 23 --internal-log-stacked-height 21 --mode prove-stark  # CUDA OOM in leaf agg
cd ..

# Collect results/ into this directory's layout
./openvm-riscv/scripts/collect_bench_results.sh results bench-results
```

Note: `openvm-eth-bench.patch` also wires `POWDR_APC_CANDIDATES_DIR` into
powdr's `GenerateConfig` — at the pinned openvm-eth ref the benchmark binary
otherwise never writes `apc_candidates.json` (the comment in its `run.sh` is
stale). The patch does not touch proving. Applying it changes the
generate-stage cache hash, so the candidate ranking is rebuilt once (~12 min).

## reth (GPU, prove-stark = app proof + leaf + internal recursion)

| APCs | segments | total proof time | excl. trace gen | peak GPU mem |
|---|---|---|---|---|
| 0 | 44 | 33.7 s | 20.7 s | 16.6 GiB |
| 3 | 43 | 33.2 s | 20.1 s | 16.5 GiB |
| 10 | 42 | 33.4 s | 20.2 s | 16.6 GiB |
| 30 | 42 | 33.9 s | 20.3 s | 16.6 GiB |
| 100 | 40 | 36.2 s | 21.3 s | 16.6 GiB |
| 300 | 37 | 42.1 s | 24.9 s | 16.7 GiB |
| 500 ¹ | 35 | 46.9 s | 28.1 s | 20.2 GiB |

¹ 500 APCs needs `--leaf-log-stacked-height 22 --internal-log-stacked-height 20`
(defaults overflow: `LayoutHeightExceeded { log_height: 22, log_stacked_height: 21 }`
in leaf aggregation, then `{ 20, 19 }` in `internal_for_leaf`).

**Where the APC-count limit is on this card** (see `failed_runs.txt`):

- **≤ 300**: proves with default aggregation parameters.
- **500**: proves with the raised caps above; peak GPU memory 20.2 GiB.
- **750 / 1000**: the leaf verifier trace needs a 2^23-row stacked layout,
  and proving such a leaf runs out of the 4090's 24 GiB
  (`vpmm_create_physical ... out of memory` during `logup_zerocheck_prover`).
  Notably the **app proof itself completes at 1000 APCs** — the recursion
  stack, not the app circuit or candidate supply (reth has 11 099 eligible
  basic blocks), is the binding constraint.

For reference, today's CPU nightly on `main`
([2026-06-03-0516](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-06-03-0516),
Xeon-class runner, same block): apc000 = 1382.6 s → apc100 = 1229.7 s
(−11 %). On the 4090 the same sweep is ~41× faster in absolute terms, but
**APCs do not reduce wall-clock proof time on this GPU** — trace cells and
segment count drop (44 → 35 segments at 500), yet total time *rises* with
APC count. With cell costs this cheap on a 4090, the extra per-AIR fixed
overheads (more chips per segment to commit/open) outweigh the saved cells
at this block's profile.

## Guests (GPU, prove with `--recursion`)

| guest | apc000 | best APC count | best | manual precompile |
|---|---|---|---|---|
| keccak | 17.6 s | 10 | 9.2 s | 2.2 s |
| sha256 | 15.5 s | 300 ² | 6.9 s | 3.3 s |
| u256 | 12.4 s | 10 | 7.2 s | 5.9 s |
| matmul | 1.8 s | 30 ² | 1.3 s | — |
| pairing | 5.0 s | 30 | 4.9 s | 0.9 s |
| ecc (projective) | 25.3 s | 30 | 16.4 s | 1.0 s |
| ecc (affine-hint) | 9.6 s | 30 | 7.1 s | 1.0 s |
| ecrecover | 8.4 s | 30 | 5.8 s | 1.0 s |

² Differences between the top counts are noise once the candidate ranking
saturates: keccak has only 61 eligible candidates, sha256 30, u256 78,
matmul 11 — sweep points beyond those sizes select the same APC set
(identical cells/columns in the metrics). ecc (425/643), ecrecover (493)
and pairing (7275) are genuine selections at every swept count.

Reading the totals: on GPU the interpreted PGO/preflight execution dominates
(e.g. keccak apc010: 5.3 s of the 9.2 s total). On pure proving time
(`total_proof_time_excluding_trace_ms`) APCs get much closer to the manual
precompiles and beat them for sha256 (0.9 s vs 1.6 s at 30 APCs) and u256
(2.1 s vs 3.3 s at 10 APCs) — consistent with the blog's CPU findings.

**Failures** (`ecc/failed_runs.txt`): `guest-ecc-projective` and
`guest-ecc-powdr-affine-hint` at **300 APCs** panic in GPU leaf aggregation
with `LayoutHeightExceeded { log_height: 22, log_stacked_height: 21 }` —
the same recursion ceiling as reth-at-500, but the powdr CLI does not expose
the leaf/internal stacked-height knobs, so those two points are absent.

## Files

Per experiment: `apcNNN/` (or `apcNNN.json` for reth) with `metrics.json`,
`psrecord` memory profiles, `trace_cells` plots; `basic_metrics.csv`,
`combined_metrics.json` (metrics-viewer input), `proof_time_breakdown.png`,
`effectiveness.png`, and `apc_candidates.json` (APC-analyzer input; for
guests under `candidates/<guest>-input<N>/`). reth additionally has
`gpu_memory_usage_apcNNN.csv` traces and `failed_runs.txt`.
