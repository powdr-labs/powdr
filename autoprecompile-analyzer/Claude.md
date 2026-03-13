# APC Effectiveness Analyzer

Single-page web app for visualizing Automatic Precompile (APC) candidate effectiveness in zkVM systems. Port of `plot_effectiveness.py` from powdr.

## Project Structure
```
index.html          # SPA with embedded JS/CSS (~2000 lines)
CLAUDE.md           # This file
```

## Data Format

Current version:

**Version 4** (current): Each APC is a *superblock* composed of one or more basic blocks.
```json
{
  "version": 4,
  "apcs": [{
    "execution_frequency": 50000,
    "original_blocks": [
      { "start_pc": 12345, "instructions": ["instr1", "instr2"] },
      { "start_pc": 12360, "instructions": ["instr3"] }
    ],
    "stats": {
      "before": { "main_columns": 100, "constraints": 200, "bus_interactions": 50 },
      "after": { "main_columns": 50, "constraints": 100, "bus_interactions": 25 }
    },
    "width_before": 100,
    "value": 5000,
    "cost_before": 1000.0,
    "cost_after": 500.0,
  }],
  "labels": { "2099200": ["memset"], "2099448": ["memcpy"] }
}
```

All older formats are normalized to `original_blocks` on load:
- **Versions 2 & 3**: `original_block: { start_pc, instructions }` → wrapped in a 1-element array
- **Version 1** (no `version` field): `original_block` with `statements` → `original_blocks[0]` with `instructions`
- **Version 0** (bare array): same as v1 without wrapper, no labels

**Visualization model**: A block's identity is its `block_id` — a comma-separated list of hex PCs (e.g., `0x3000,0x3050`). `start_pc` is the first basic block's PC (used for sorting/display). Multiple blocks may share the same basic block PC.

## Testing

Start server:
```bash
python3 -m http.server 8000 &
```

Test URL with real data (~11,300 APCs):
```
http://localhost:8000/?data=https%3A%2F%2Fgithub.com%2Fpowdr-labs%2Fbench-results%2Fblob%2Fgh-pages%2Fresults%2F2026-01-27-0453%2Freth%2Fapc_candidates.json
```

Verify:
- Data loads (GitHub URLs auto-convert to raw)
- Bar chart shows ~3.28x mean effectiveness
- Value-cost plot reaches ~80% savings at 1000 APCs
- Labels table expands with function names
- Block selection syncs across all views

Cache-bust: append `&_t=1` to URL.

## URL Parameters

```
?data=<url>           # Data source (required to load data)
&plot=value-cost      # Show value-cost plot (omit for default bar chart)
&block=0x2008f8       # Select block by PC address (hex)
```

Example - jump directly to value-cost plot with a block selected:
```
http://localhost:8000/?data=<url>&plot=value-cost&block=0x200af0
```

URL updates automatically as you interact with the app, enabling easy sharing of specific views.

## Development Notes

**D3.js chart redraw**: Charts are fully recreated on metric switch. Ensure `.remove()` is called on exit selections to prevent memory leaks.

**State persistence**: `selectedBlock` must survive metric changes. Check selection still exists in new processed data.

**GitHub URL conversion**: `loadFromUrl()` has regex converting blob URLs to raw URLs. Brittle - test after GitHub URL format changes.

**Grouping threshold**: Blocks <0.1% of total cells grouped as "Other". Hardcoded in `createChart()`.

**Weighted mean**: `sum(effectiveness * traceCells) / sum(traceCells)` - weights by trace cells, not block count.

### Common Errors
- **CORS**: GitHub blob URLs must convert to raw URLs
- **D3 selections**: Use enter/update/exit patterns; don't forget `.remove()`
- **Event handlers**: Remove old handlers when recreating charts
- **Test with full dataset**: ~11K items, not small test data
