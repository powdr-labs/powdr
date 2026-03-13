# Metrics Viewer

Single-page web app for visualizing proof metrics from OpenVM benchmarks. Port of `basic_metrics.py` and `plot_trace_cells.py`.

## Project Structure
```
index.html          # SPA with embedded JS/CSS
CLAUDE.md           # This file
```

## Data Format

Combined metrics JSON, produced by `basic_metrics.py combine`:
```json
{
  "run_name": {
    "counter": [{ "labels": [["group", "app_proof"], ...], "metric": "cells", "value": "123" }],
    "gauge": [{ "labels": [["group", "app_proof"], ...], "metric": "total_proof_time_ms", "value": "456" }]
  },
  ...
}
```

Generate with:
```bash
python3 openvm-riscv/scripts/basic_metrics.py combine **/metrics.json > combined_metrics.json
```

## Testing

Start server:
```bash
python3 -m http.server 8000 &
```

Load data via file upload (drag-drop) or URL parameter:
```
http://localhost:8000/?data=<url>
http://localhost:8000/?data=<url>&run=apc030
```

Verify:
- Stacked bar chart shows proof time breakdown for all runs
- Summary table shows key metrics
- Clicking a run shows trace cell pie chart
- URL updates with selected run

## URL Parameters

```
?data=<url>           # Data source (loads combined JSON)
&run=<name>           # Pre-select a run by name
```

## Development Notes

**Three panes**: Proof time breakdown (stacked bar chart), summary table, trace cell pie chart (shown on run selection).

**JS ports Python logic from**: `metrics_utils.py` (load_metrics_dataframes, is_normal_instruction_air), `basic_metrics.py` (extract_metrics), `plot_trace_cells.py` (compute_cells_by_air).
