# Metrics Viewer

Single-page web app for visualizing proof metrics from OpenVM benchmarks. This is a web port of the Python scripts [`basic_metrics.py`](../../openvm-riscv/scripts/basic_metrics.py) and [`plot_trace_cells.py`](../../openvm-riscv/scripts/plot_trace_cells.py), following the same pattern as the [autoprecompile-analyzer](../../autoprecompile-analyzer/index.html).

The goal is to make benchmark results shareable via URL without needing a Python environment.

## Project Structure
```
index.html          # SPA with embedded JS/CSS (D3.js v7, Bootstrap 5.3)
spec.py             # Python reference implementation of metric computations (for auditing)
CLAUDE.md           # This file
```

## Data Format

Input can be either of these formats:

1. **Combined metrics JSON** — produced by `basic_metrics.py combine`. It maps run names to raw metrics objects:

```json
{
  "<run_name>": {
    "counter": [
      { "labels": [["group", "app_proof"], ["air_name", "SomeAir"], ["segment", "0"], ...], "metric": "cells", "value": "123456" },
      ...
    ],
    "gauge": [
      { "labels": [["group", "app_proof"], ...], "metric": "total_proof_time_ms", "value": "45678" },
      ...
    ]
  },
  "<run_name_2>": { ... }
}
```

2. **Raw metrics JSON** — a single experiment object with top-level `counter` and `gauge` keys:

```json
{
  "counter": [ ... ],
  "gauge": [ ... ]
}
```

If the top-level object has both `counter` and `gauge`, the viewer treats it as a raw metrics file and renders a single experiment. Otherwise it treats the object as combined metrics and validates each experiment entry.

Each entry in `counter` / `gauge` must have:
- `labels`: Array of `[key, value]` pairs.
- `metric`: Metric name string.
- `value`: String-encoded numeric value (or numeric in practice; the UI accepts both).

### OpenVM 1 Schema

Produced by OpenVM 1 (STARK-based prover with FRI).

**Label keys**: `group`, `air_name`, `air_id`, `segment`, `idx`, `trace_height_constraint`.

**Group values**: `app_proof`, `leaf_*` (e.g. `leaf_0`), `internal_*` (e.g. `internal_0`).

**Counter metrics** (with `group`):
- `cells`, `rows`, `main_cols`, `prep_cols`, `perm_cols` — trace dimensions (per AIR/segment)
- `total_cells`, `total_cells_used`, `main_cells_used` — cell counts including padding (per segment, no `air_name`)
- `constraints`, `interactions` — per-AIR constraint/interaction counts (no `group`/`segment` labels)
- `quotient_deg`, `fri.log_blowup`, `num_children` — FRI/quotient parameters
- `threshold`, `weighted_sum`, `execute_metered_insns`, `execute_preflight_insns`

**Gauge metrics** (timing, with `group`):
- `total_proof_time_ms` — total time per group (app/leaf/internal)
- `stark_prove_excluding_trace_time_ms` — STARK prover time minus trace generation
- `trace_gen_time_ms`, `system_trace_gen_time_ms`, `single_trace_gen_time_ms`
- `execute_preflight_time_ms`, `execute_metered_time_ms`
- `main_trace_commit_time_ms`, `perm_trace_commit_time_ms`
- `generate_perm_trace_time_ms`, `memory_to_vec_partition_time_ms`
- `quotient_poly_compute_time_ms`, `quotient_poly_commit_time_ms`, `pcs_opening_time_ms`
- `single_leaf_agg_time_ms`, `single_internal_agg_time_ms`, `agg_layer_time_ms`
- `app_prove_time_ms`, `prove_segment_time_ms`
- `total_apc_gen_time_ms`, `memory_finalize_time_ms`, `compute_user_public_values_proof_time_ms`
- `dummy_proof_and_keygen_time_ms`

### OpenVM 2 Schema

Produced by OpenVM 2 (uses GKR/LogUp-based prover with WHIR).

**Label keys**: All V1 keys plus `air`, `module`, `phase`.

**Group values**: `app_proof`, `leaf`, `compression`, `internal_for_leaf`, `internal_recursive.0`, `internal_recursive.1`, `internal_recursive.2`.

Key differences from V1:
- `compression` is a new proving phase (not present in V1)
- `leaf` has no numeric suffix (V1 used `leaf_*`)
- Internal groups split into `internal_for_leaf` and `internal_recursive.N`

**Counter metrics**:
- Same as V1: `cells`, `rows`, `main_cols`, `prep_cols`, `perm_cols`, `total_cells`, `constraints`, `interactions`
- New: `constraint_deg` (replaces V1's `quotient_deg`)
- Removed: `total_cells_used`, `main_cells_used`, `quotient_deg`, `fri.log_blowup`, `num_children`

**Gauge metrics** — timing breakdown is hierarchical with `prover.*` prefix:
- Top-level (same as V1): `total_proof_time_ms`, `stark_prove_excluding_trace_time_ms`, `trace_gen_time_ms`, `execute_preflight_time_ms`, `execute_metered_time_ms`
- New `prover.*` sub-metrics:
  - `prover.main_trace_commit_time_ms` — trace commitment
  - `prover.rap_constraints_time_ms` — constraint evaluation (parent)
    - `prover.rap_constraints.logup_gkr_time_ms` — LogUp GKR
    - `prover.rap_constraints.logup_gkr.input_evals_time_ms`
    - `prover.rap_constraints.round0_time_ms`, `prover.rap_constraints.ple_round0_time_ms`, `prover.rap_constraints.mle_rounds_time_ms`
  - `prover.openings_time_ms` — opening proofs (parent)
    - `prover.openings.stacked_reduction_time_ms`, `prover.openings.whir_time_ms`
    - `prover.openings.stacked_reduction.round0_time_ms`, `prover.openings.stacked_reduction.mle_rounds_time_ms`
- New GPU metrics: `fractional_sumcheck_gpu_time_ms`, `prove_zerocheck_and_logup_gpu_time_ms`, `compute_merkle_precomputation_cuda_time_ms`
- New GPU memory: `gpu_mem.current_bytes`, `gpu_mem.local_peak_bytes`, `gpu_mem.reserved_bytes`, `gpu_mem.timestamp_ms`
- New phases: `compression_time_ms`, `generate_cached_trace_time_ms`, `generate_proving_ctxs_time_ms`, `generate_blob_time_ms`, `set_initial_memory_time_ms`
- `module` label: prover sub-module breakdown (e.g. `prover.merkle_tree`, `prover.openings`, `frac_sumcheck.*`, `tracegen.*`)
- `air` label: used by `generate_cached_trace_time_ms` and `single_trace_gen_time_ms` (contains full Rust type name, distinct from `air_name`)
- Removed: `dummy_proof_and_keygen_time_ms`, `generate_perm_trace_time_ms`, `perm_trace_commit_time_ms`, `quotient_poly_*_time_ms`, `pcs_opening_time_ms`, `memory_to_vec_partition_time_ms`

### Version Detection

The viewer auto-detects the OpenVM version by checking for `logup_gkr` in metric names (V2-only). The detected version is displayed as a badge in the navbar.

### Proof Time Hierarchy

**V1**: `total_proof_time_ms` (per group) is the top-level time. `execute_metered_time_ms` is inside it.
```
total = app.total_proof_time_ms + leaf.total_proof_time_ms + internal.total_proof_time_ms
app.total_proof_time_ms ≈ stark_prove_excluding_trace + trace_gen + preflight + metered + other
```

**V2**: `app_prove_time_ms` is the wall-clock app time. `execute_metered_time_ms` sits *outside* `total_proof_time_ms` (which is per-segment) but inside `app_prove_time_ms`. The viewer uses `app_prove_time_ms` for app and `total_proof_time_ms` for other groups.
```
total = app.app_prove_time_ms + leaf.total_proof_time_ms + internal.total_proof_time_ms + compression.total_proof_time_ms
app.app_prove_time_ms ≈ metered + sum_per_segment(preflight + set_initial_memory + trace_gen + stark_excl) + overhead
stark_excl ≈ prover.main_trace_commit + prover.rap_constraints + prover.openings
```

The V2 stacked bar chart breaks STARK into three sub-components (constraints, openings, trace commit) plus a small "STARK other" residual.

Generate a combined file with:
```bash
python3 openvm-riscv/scripts/basic_metrics.py combine **/metrics.json > combined_metrics.json
```

Example input files:
- OpenVM 1 — Keccak: https://github.com/powdr-labs/bench-results/blob/gh-pages/results/2026-03-23-0535/keccak/combined_metrics.json
- OpenVM 1 — Reth (older format, no constraints/interactions): https://github.com/powdr-labs/bench-results/blob/gh-pages/results/2026-03-23-0535/reth/combined_metrics.json
- OpenVM 2 — Pairing: https://gist.githubusercontent.com/leonardoalt/3074cb729c03470b1116674618b97267/raw/eec5e5a086bf07a57e2215843f0a3f1ada9d0d5c/metrics_v2_pairing_combined.json

## Testing

Start server and open with example data:
```bash
cd openvm/metrics-viewer && python3 -m http.server 8000
```

Load data via file upload (drag-drop) or URL parameter:
```
http://localhost:8000/?data=<url>&run=<name>
```

For raw metrics JSON loaded from a URL, the viewer infers the experiment name from the path (for example `/apc030/metrics.json` becomes `apc030`).

Example, using the data above and pre-selecting the `apc030` run:
```
http://localhost:8000/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-03-19-0538%2Fkeccak%2Fcombined_metrics.json&baseline=apc000&run=apc030
```

Verify:
- Summary table shows key metrics for all runs
- Stacked bar chart shows proof time breakdown; "By Component" tab shows grouped bars
- Clicking a run shows experiment details (details table + trace cell pie chart)
- URL updates with selected run and data source
- Version badge in navbar shows "OpenVM 1" or "OpenVM 2"
- For OpenVM 2: compression time appears in the breakdown, "App Cells (without padding)" row is hidden

## URL Parameters

```
?data=<url>           # Data source (loads raw or combined metrics JSON; GitHub blob URLs auto-converted to raw)
&run=<name>           # Pre-select a run by name
```

## Code Structure

The JavaScript in `index.html` is organized into clearly separated sections:

1. **Data Processing** — ports of Python logic, these are the core functions that compute all displayed numbers:
   - `normalizeMetricsData(json, sourceLabel)` — validates the incoming JSON shape, distinguishes raw-vs-combined input, and wraps raw files as a single experiment.
   - `detectOpenVmVersion(combinedData)` — returns `1` or `2` by checking for `logup_gkr` in metric names (V2-only).
   - `loadMetricsDataframes(json)` — port of [`metrics_utils.py:load_metrics_dataframes`](../../openvm-riscv/scripts/metrics_utils.py). Flattens `counter`+`gauge` arrays into entries, splits by `group` prefix into `app`, `leaf`, `internal`, `compression`.
   - `isNormalInstructionAir(name)` — port of [`metrics_utils.py:is_normal_instruction_air`](../../openvm-riscv/scripts/metrics_utils.py). Classifies AIR names as normal RISC-V instructions vs. precompiles.
   - `getMetric(entries, name)` — sums `value` for all entries matching a metric name.
   - `extractMetrics(runName, json)` — port of [`basic_metrics.py:extract_metrics`](../../openvm-riscv/scripts/basic_metrics.py). Computes all summary metrics (proof times, cell counts, ratios) from raw JSON.
   - `computeCellsByAir(json)` — port of [`plot_trace_cells.py:compute_cells_by_air`](../../openvm-riscv/scripts/plot_trace_cells.py). Aggregates cells by AIR name with 1.5% threshold.

2. **Metric Descriptions** — `METRIC_INFO` object (search for `const METRIC_INFO`). Single source of truth for human-readable descriptions and Python code snippets for every computed metric. Displayed as info-icon tooltips in the detail tables. When adding a new metric to the detail rows, add a corresponding entry here.

3. **Constants** — `COMPONENTS_V1`/`COMPONENTS_V2` (proof time breakdown components with colors), `TABLE_COLUMNS`, detail row arrays (`BASIC_STATS_ROWS_V1`/`V2`, `PROOF_TIME_ROWS_V1`/`V2`). Version-aware getters (`getComponents()`, `getBasicStatsRows()`, `getProofTimeRows()`) return the right variant.

4. **Chart Components** — `createBarChart()`, `createGroupedBarChart()`, `createPieChart()`, each rendering into their container.

5. **Table Components** — `createSummaryTable()`, `renderDetails()`.

6. **Data Loading & URL Handling** — file upload, URL fetch, parameter sync.
