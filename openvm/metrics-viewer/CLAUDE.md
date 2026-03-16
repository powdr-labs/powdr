# Metrics Viewer

Single-page web app for visualizing proof metrics from OpenVM benchmarks. This is a web port of the Python scripts [`basic_metrics.py`](../../openvm-riscv/scripts/basic_metrics.py) and [`plot_trace_cells.py`](../../openvm-riscv/scripts/plot_trace_cells.py), following the same pattern as the [autoprecompile-analyzer](../../autoprecompile-analyzer/index.html).

The goal is to make benchmark results shareable via URL without needing a Python environment.

## Project Structure
```
index.html          # SPA with embedded JS/CSS (D3.js v7, Bootstrap 5.3)
CLAUDE.md           # This file
```

## Data Format

Input is a combined metrics JSON, produced by `basic_metrics.py combine`. It maps run names to raw metrics objects:

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

Each entry in `counter` / `gauge` has:
- `labels`: Array of `[key, value]` pairs. Common keys: `group` (`app_proof*`, `leaf*`, `internal*`), `air_name`, `segment`.
- `metric`: Metric name (e.g. `cells`, `total_proof_time_ms`, `trace_gen_time_ms`, `rows`, `main_cols`).
- `value`: String-encoded numeric value.

Generate with:
```bash
python3 openvm-riscv/scripts/basic_metrics.py combine **/metrics.json > combined_metrics.json
```

Example input file: https://gist.githubusercontent.com/georgwiese/b146800a3b5eb633a6d5157f8aff1123/raw/e02ba2cec6a4cc063e4bff117cf46c69ff775e1e/keccak_combined.json

## Testing

Start server and open with example data:
```bash
cd openvm/metrics-viewer && python3 -m http.server 8000
```

Load data via file upload (drag-drop) or URL parameter:
```
http://localhost:8000/?data=<url>&run=<name>
```

Example, using the data above and pre-selecting the `apc030` run:
```
http://localhost:8000/?data=https%3A%2F%2Fgist.githubusercontent.com%2Fgeorgwiese%2Fb146800a3b5eb633a6d5157f8aff1123%2Fraw%2Fe02ba2cec6a4cc063e4bff117cf46c69ff775e1e%2Fkeccak_combined.json&run=apc030
```

Verify:
- Summary table shows key metrics for all runs
- Stacked bar chart shows proof time breakdown; "By Component" tab shows grouped bars
- Clicking a run shows experiment details (details table + trace cell pie chart)
- URL updates with selected run and data source

## URL Parameters

```
?data=<url>           # Data source (loads combined JSON; GitHub blob URLs auto-converted to raw)
&run=<name>           # Pre-select a run by name
```

## Code Structure

The JavaScript in `index.html` is organized into clearly separated sections:

1. **Data Processing** — ports of Python logic, these are the core functions that compute all displayed numbers:
   - `loadMetricsDataframes(json)` — port of [`metrics_utils.py:load_metrics_dataframes`](../../openvm-riscv/scripts/metrics_utils.py). Flattens `counter`+`gauge` arrays into entries, splits by `group` prefix into `app`, `leaf`, `internal`.
   - `isNormalInstructionAir(name)` — port of [`metrics_utils.py:is_normal_instruction_air`](../../openvm-riscv/scripts/metrics_utils.py). Classifies AIR names as normal RISC-V instructions vs. precompiles.
   - `getMetric(entries, name)` — sums `value` for all entries matching a metric name.
   - `extractMetrics(runName, json)` — port of [`basic_metrics.py:extract_metrics`](../../openvm-riscv/scripts/basic_metrics.py). Computes all summary metrics (proof times, cell counts, ratios) from raw JSON.
   - `computeCellsByAir(json)` — port of [`plot_trace_cells.py:compute_cells_by_air`](../../openvm-riscv/scripts/plot_trace_cells.py). Aggregates cells by AIR name with 1.5% threshold.

2. **Constants** — `COMPONENTS` (7 proof time components with colors matching Python), `TABLE_COLUMNS`, `DETAIL_ROWS`.

3. **Chart Components** — `createBarChart()`, `createGroupedBarChart()`, `createPieChart()`, each rendering into their container.

4. **Table Components** — `createSummaryTable()`, `renderDetails()`.

5. **Data Loading & URL Handling** — file upload, URL fetch, parameter sync.
