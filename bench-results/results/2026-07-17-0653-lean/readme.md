# Bench results — 2026-07-17-0653-lean

Identical to the nightly benchmarks, but APC optimization is routed through the Lean4 verified [apc-optimizer](https://github.com/powdr-labs/apc-optimizer) via FFI (POWDR_USE_LEAN_OPTIMIZER=1) instead of the native Rust optimizer. Proving is unchanged; only APC generation differs.

**reth**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/reth) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Freth%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Freth%2Fapc_candidates.json)

**ecc**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/ecc) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fecc%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fecc%2Faffine-hint-apc030%2Fapc_candidates.json)

**ecrecover**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/ecrecover) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fecrecover%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fecrecover%2Fapc030%2Fapc_candidates.json)

**keccak**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/keccak) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fkeccak%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fkeccak%2Fapc030%2Fapc_candidates.json)

**matmul**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/matmul) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fmatmul%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fmatmul%2Fapc003%2Fapc_candidates.json)

**pairing**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/pairing) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fpairing%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fpairing%2Fapc030%2Fapc_candidates.json)

**sha256**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/sha256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fsha256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fsha256%2Fapc030%2Fapc_candidates.json)

**u256**: 📂 [Raw data](https://github.com/powdr-labs/bench-results/tree/gh-pages/results/2026-07-17-0653-lean/u256) &nbsp;|&nbsp; 📊 [Metrics Viewer](https://powdr-labs.github.io/powdr/openvm/metrics-viewer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fu256%2Fcombined_metrics.json) &nbsp;|&nbsp; 🔍 [APC Analyzer](https://powdr-labs.github.io/powdr/autoprecompile-analyzer/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-07-17-0653-lean%2Fu256%2Fapc030%2Fapc_candidates.json)

## APC generation time per benchmark

Wall-clock time of the APC generation stage (build + rank + optimize
candidates) per benchmark:

| Benchmark | Run | APC generation time |
| --- | --- | --- |
| ecc | affine-hint-apc030 | 23m28s |
| ecc | projective-apc030 | 24m04s |
| ecrecover | apc030 | 23m39s |
| keccak | apc030 | 3m02s |
| matmul | apc003 | 9m44s |
| matmul | apc030 | 1s |
| pairing | apc030 | 4m21s |
| sha256 | apc030 | 308m55s |
| u256 | apc030 | 24s |
| **total** | | **397m38s** |
