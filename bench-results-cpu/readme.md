# Bench results — 2026-06-06, CPU (Xeon Gold 5412U)

CPU rerun of the [Accelerating Ethereum with Autoprecompiles](https://powdr.org/blog/accelerating-ethereum-with-autoprecompiles)
benchmarks on the `bench-cuda-apc-sweep-2026-06` branch of powdr — powdr
`main` after the OpenVM 2.0 update plus
[powdr-labs/powdr#3763](https://github.com/powdr-labs/powdr/pull/3763)
(staged `generate → select → setup` pipeline with `--artifacts-dir` caching).
The Rust code is `be183c60e`, identical to the GPU
[`bench-results/`](../bench-results/readme.md) run — only the CPU bench
scripts are added on top. This is the **CPU counterpart** of the GPU
`bench-results/` (RTX 4090) and `bench-results-autoopt/` runs, on the same
single-socket Xeon-class machine the blog post used. Every guest is swept
over {0, 3, 10, 30, 100, 300} autoprecompiles; reth (openvm-eth, mainnet
block 24171377) over {0, 3, 10, 30, 100, 300} plus high-count probes
{500, 1000, 2000} (raised aggregation caps; see Notes). Guest workloads are
sized to ~50 segments in the software (0 APCs) version, matching the blog
post's inputs (25 000 keccaks, 80 000 sha256 hashes; 100 ecc scalar mults
and 125 ecrecovers hit the same segment target). pairing, u256 and matmul
run fixed workloads baked into the guest (input is ignored).

**reth**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/reth) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Freth%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Freth%2Fapc_candidates.json)

**ecc**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/ecc) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fecc%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fecc%2Fcandidates%2Fguest-ecc-powdr-affine-hint-input100%2Fapc_candidates.json)

**ecrecover**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/ecrecover) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fecrecover%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fecrecover%2Fcandidates%2Fguest-ecrecover-input125%2Fapc_candidates.json)

**keccak**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/keccak) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fkeccak%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fkeccak%2Fcandidates%2Fguest-keccak-input25000%2Fapc_candidates.json)

**matmul**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/matmul) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fmatmul%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fmatmul%2Fcandidates%2Fguest-matmul-input0%2Fapc_candidates.json)

**pairing**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/pairing) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fpairing%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fpairing%2Fcandidates%2Fguest-pairing-input0%2Fapc_candidates.json)

**sha256**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/sha256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fsha256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fsha256%2Fcandidates%2Fguest-sha256-input80000%2Fapc_candidates.json)

**u256**: 📂 [Raw data](https://github.com/powdr-labs/powdr/tree/bench-cuda-apc-sweep-2026-06/bench-results-cpu/u256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fu256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fpowdr%2Fblob%2Fbench-cuda-apc-sweep-2026-06%2Fbench-results-cpu%2Fu256%2Fcandidates%2Fguest-u256-input0%2Fapc_candidates.json)

## Environment

| | |
|---|---|
| CPU | Intel Xeon Gold 5412U (24C/48T, single socket) |
| RAM | 251 GiB (shared box; ~21 GiB used by other tenants during the run — see the ecc/ecrecover OOM note) |
| GPU | none (CPU-only run) |
| OS | Linux 6.18.9-arch1-2 |
| powdr | `be183c60e` (+ bench-script commits of this branch; Rust identical to the GPU run) |
| openvm-eth | [`44564802`](https://github.com/powdr-labs/openvm-eth/commit/44564802f942d15c6dd782979d280f28065179b5) (ref from `.github/actions/patch-openvm-eth`), patched to local powdr crates + [`openvm-eth-bench.patch`](../openvm-riscv/scripts/openvm-eth-bench.patch) |
| toolchains | powdr host: `nightly-2025-10-01`; openvm-eth host + guests: `nightly-2026-01-18`; `cargo-openvm` v2.0.0-beta.2 (tag `v2.0.0-beta.2-powdr`, installed with Rust 1.91.1) |

All proving runs with `POWDR_OPENVM_SEGMENT_DELTA=50000` exported, as the
nightly workflow does for all jobs. openvm-eth's `run.sh` auto-selects the
CPU (non-`cuda`) prover because there is no GPU on this box; the powdr CLI is
built with `--features metrics` (no `cuda`). The guest sweep needs
`JEMALLOC_SYS_WITH_MALLOC_CONF` set (the nightly exports it for all jobs);
`run_guest_benches.sh` now defaults it, since without jemalloc's background
purge thread the large software (0-APC) proofs OOM on this box.

## How to reproduce

From a checkout of this branch (no GPU needed):

```sh
# One-time setup (mirrors .github/workflows/nightly-tests.yml)
rustup toolchain install nightly-2026-01-18 && rustup component add rust-src --toolchain nightly-2026-01-18
rustup toolchain install 1.91.1
cargo +1.91.1 install --git 'https://github.com/powdr-labs/openvm.git' --tag "v2.0.0-beta.2-powdr" cargo-openvm
python3 -m venv .venv && source .venv/bin/activate
pip install -r openvm-riscv/scripts/requirements.txt -r autoprecompiles/scripts/requirements.txt psrecord

export POWDR_OPENVM_SEGMENT_DELTA=50000

# Guest benchmarks (CPU) -> results/{keccak,sha256,pairing,u256,matmul,ecc,ecrecover}
# BENCH_FEATURES defaults to "metrics" (CPU); BENCH_KEEP_GOING=1 records a
# failed prove (OOM / recursion cap) instead of aborting the sweep.
# Guests pull deps from public github repos over https (openvm-org/uint,
# powdr-labs/elliptic-curves-k256). If your git config rewrites https->ssh and
# you have no ssh key, give cargo plain https first, e.g.:
#   git config --global --unset url."git@github.com:".insteadOf   # if present
#   cargo with net.git-fetch-with-cli = true (~/.cargo/config.toml)
BENCH_KEEP_GOING=1 ./openvm-riscv/scripts/run_guest_benches.sh

# Reth core sweep (CPU) -> results/reth. Needs an Ethereum mainnet *archive*
# RPC for the first run only; the fetched witness is cached under
# openvm-eth/rpc-cache/ afterwards. Put it in openvm-eth/.env as
# `export RPC_1=<archive rpc url>` (the script's run.sh sources it):
git clone https://github.com/powdr-labs/openvm-eth.git openvm-eth
git -C openvm-eth checkout 44564802f942d15c6dd782979d280f28065179b5
git -C openvm-eth apply ../openvm-riscv/scripts/openvm-eth-bench.patch
printf 'export RPC_1=%s\n' "<archive rpc url>" > openvm-eth/.env
./openvm-riscv/scripts/run_reth_cpu_bench.sh           # {0,3,10,30,100,300}

# Reth high-count probes (CPU) with raised aggregation caps, appended to
# results/reth. Defaults to 500 (leaf 22/int 20) + 1000 (leaf 23/int 21);
# the 2000 point used leaf 24/int 22:
./openvm-riscv/scripts/run_reth_cpu_highcount.sh                       # 500 + 1000
PROBES="2000:24:22" ./openvm-riscv/scripts/run_reth_cpu_highcount.sh   # 2000

# Collect results/ into this directory's layout
./openvm-riscv/scripts/collect_bench_results.sh results bench-results-cpu
```

Note: `openvm-eth-bench.patch` wires `POWDR_APC_CANDIDATES_DIR` into powdr's
`GenerateConfig` — at the pinned openvm-eth ref the benchmark binary
otherwise never writes `apc_candidates.json` (the comment in its `run.sh` is
stale). The patch also hardens the RPC fetch (retries + an optional
`RPC_PROOF_CHUNK_SIZE` for free-tier endpoints); it does not touch proving.
A paid/archive RPC (e.g. Alchemy) needs neither the chunk-size override nor
extra retries.

## reth (CPU, prove-stark = app proof + leaf + internal recursion)

| APCs | segments | total proof time | excl. trace gen | app cells | peak host RAM |
|---|---|---|---|---|---|
| 0 | 44 | 1969.1 s | 1849.3 s | 19.77 B | 49.5 GiB |
| 3 | 43 | 1872.6 s | 1739.6 s | 18.50 B | 50.5 GiB |
| 10 | 42 | 1838.6 s | 1703.8 s | 18.46 B | 50.0 GiB |
| 30 | 42 | 1788.0 s | 1647.4 s | 18.05 B | 50.0 GiB |
| 100 | 40 | 1644.9 s | 1486.7 s | 16.47 B | 49.5 GiB |
| 300 | 37 | **1575.3 s** | **1390.9 s** | 14.88 B | 50.6 GiB |
| 500 ¹ | 35 | 1668.8 s | 1473.7 s | 14.01 B | 53.9 GiB |
| 1000 ¹ | 34 | 1681.5 s | 1460.4 s | 13.50 B | 63.4 GiB |
| 2000 ¹ | 33 | 2257.2 s | 1990.2 s | 12.96 B | 115.6 GiB |

¹ High-count probes with raised aggregation caps (`--leaf-log-stacked-height`
/ `--internal-log-stacked-height`): 500 → 22/20, 1000 → 23/21, 2000 → 24/22.
With default caps these fail in leaf aggregation with `LayoutHeightExceeded`,
exactly as on the GPU.

**On CPU, autoprecompiles reduce proving time** — the opposite of the GPU
runs. Total proof time falls monotonically from apc000 to a **minimum at
~300 APCs** (1969 s → 1575 s, **−20%**; excluding trace generation 1849 s →
1391 s, **−25%**), as APCs fold base-instruction work into wider precompile
AIRs and drop the segment count 44 → 37. App trace cells fall monotonically
the whole way (19.77 B → 12.96 B at 2000 APCs, −34%).

Past ~300 the trend reverses: the per-AIR fixed overheads (more distinct
chips to commit and open — app columns grow 233 k → 2.33 M from 0 to 2000
APCs) start to outweigh the saved cells, so proving time *rises* again, and
by 2000 APCs it is **slower than the 0-APC baseline** (2257 s vs 1969 s).
This is the same per-AIR-overhead effect the GPU run hit, but on CPU cells
are expensive enough that it only bites well past the 300-APC optimum,
whereas on the 4090 it dominated from the very first APCs. So the **useful**
APC range on CPU is wide (every count up to ~300 is a net win); beyond that
APCs still shrink the trace but cost more to prove.

For absolute-scale context, the same sweep on the RTX 4090
([`bench-results/`](../bench-results/readme.md)) runs ~40–60× faster in
wall-clock (apc000 ≈ 33.7 s vs 1969 s here) but shows APCs *increasing* GPU
proof time; the CPU here is the regime the blog post measured, and APCs help.

**Where the APC-count limit is on CPU** (`reth/failed_runs.txt` is empty —
no run failed):

- **≤ 300**: proves with default aggregation parameters.
- **500 / 1000 / 2000**: prove with progressively raised leaf/internal
  stacked-height caps (22/20, 23/21, 24/22). Peak host RAM climbs 50 → 54 →
  63 → **116 GiB** (of 251 GiB).
- The 4090 run's binding limit was the **24 GiB of VRAM**: 500 was its
  practical ceiling and **1000 OOM'd in leaf aggregation** (the app proof
  completed, but the 2²³-row stacked leaf verifier exceeded VRAM). With
  251 GiB of system RAM the CPU has none of that ceiling — **1000 and even
  2000 complete here**. The binding constraint on CPU is *usefulness*, not
  feasibility: proving time is minimised at ~300 and only grows above it, so
  there is no reason to go higher (reth has 11 099 eligible basic blocks, so
  candidate supply is not the limit either). Extrapolating the RAM curve,
  ~4000 APCs would approach the box's memory, but the proof would be far
  slower than the 300-APC optimum regardless.

## Guests (CPU, prove with `--recursion`)

Total proof time (segment count in parentheses); "best" is the APC count with
the lowest total proof time.

| guest | input | software (0 APCs) | best | manual precompile |
|---|---|---|---|---|
| keccak | 25000 | 2747 s (48 seg) | 407 s @ 30 APCs | 133 s |
| sha256 | 80000 | 2534 s (44 seg) | 387 s @ 10 APCs | 234 s |
| u256 | fixed 70×70 | 709 s (12 seg) | 213 s @ 100 APCs | 181 s |
| matmul | fixed | 93.5 s (2 seg) | 38.5 s @ 10 APCs | — |
| pairing | fixed | 170 s (5 seg) | 150 s @ 100 APCs | 27 s |
| ecc (projective) | 100 | 3079 s (52 seg) | 1067 s @ 100 APCs | 41 s |
| ecc (affine-hint) | 100 | OOM ² | 381 s @ 30 APCs | 41 s |
| ecrecover | 125 | OOM ² | 931 s @ 30 APCs | 77 s |

² Software (and the lowest APC counts) of the two wide-AIR k256 guests OOM on
this box — see below.

**On CPU, APCs cut proving time substantially** — much more than on the GPU,
because CPU cells are expensive so removing them pays off. Best-vs-software
total-time reductions: keccak **6.8×** (2747 → 407 s), sha256 **6.5×**
(2534 → 387 s), ecc-projective **2.9×** (3079 → 1067 s), u256 **3.3×**,
matmul **2.4×**, pairing 1.1×. On pure STARK proving time
(`total_proof_time_excluding_trace_ms`) the gains are even larger (keccak
2656 → 200 s, **13×**; sha256 2456 → 170 s, **14×**), because APCs trade
cheaper-to-prove cells for somewhat more expensive trace generation.

Versus the **manual precompiles**: on *total* time the hand-written
precompiles are still faster everywhere (e.g. keccak 133 s vs 407 s), since
the autoprecompile path spends more in trace generation / preflight. But on
*pure proving time* (excl. trace) APCs **beat** the manual precompiles for
sha256 (170 s vs 222 s) and u256 (108 s vs 169 s), and trail on keccak and
the EC guests — consistent with the blog's CPU findings (APCs competitive on
hashing/U256, behind on elliptic-curve ops).

**Why `software` and the lowest APC counts OOM for ecc (affine-hint) and
ecrecover** (`ecc/failed_runs.txt`, `ecrecover/failed_runs.txt`): these two
guests use the k256 affine-hint circuits, whose AIRs are far wider than the
hashing guests'. Their 0-APC / few-APC versions need ~230 GiB of host RAM
during trace generation, which exceeds what is free on this **shared** box
(251 GiB total, ~21 GiB held by other tenants), so they die with an
allocator/`mmap` failure. The GPU run completed these because its prover
streams segment traces to VRAM (low host-memory peak); the CPU prover holds
them in RAM. Adding APCs shrinks the per-segment footprint enough to fit:
ecc-affine-hint proves from **10 APCs** up, ecrecover from **30 APCs** up. So
on CPU, APCs don't just speed these up — they make them provable at all. (The
CLI exposes no per-segment-size knob and this box has no spare RAM/swap, so the
software baselines can't be recovered here without changing the segmentation;
`ecc (projective)`, whose AIRs are narrow, proves fine at 0 APCs.)

**Why there is no 300-APC point for ecc / ecrecover**: at 300 APCs
`guest-ecc-projective`, `guest-ecc-powdr-affine-hint` and `guest-ecrecover`
select large enough precompile AIRs that the leaf-verifier trace needs a 2²²
stacked layout, above the prover's default leaf cap of 2²¹, and crash in leaf
aggregation with `LayoutHeightExceeded { log_height: 22, log_stacked_height:
21 }`. This is the **same recursion ceiling as reth-at-500** (rescued there by
`--leaf-log-stacked-height 22 --internal-log-stacked-height 20` on openvm-eth's
`run.sh`); the powdr guest CLI (`powdr_openvm_riscv prove`) does **not** expose
those knobs, so it can't be raised here. Device-independent — the GPU run hits
the identical panic at 300 for exactly these three.

## Files

Per experiment: `apcNNN/` (or `apcNNN.json` for reth) with `metrics.json`,
`psrecord` memory profiles, `trace_cells` plots; `basic_metrics.csv`,
`combined_metrics.json` (metrics-viewer input), `proof_time_breakdown.png`,
`effectiveness.png`, and `apc_candidates.json` (APC-analyzer input; for
guests under `candidates/<guest>-input<N>/`). reth additionally has per-count
`psrecord_apcNNN.{csv,png}` host-memory traces. `failed_runs.txt` records the
OOM / recursion-cap failures with the reason per line.
