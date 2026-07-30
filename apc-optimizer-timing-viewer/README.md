# APC Optimizer Timing Viewer

Single-file web app for exploring the Rust-vs-Lean APC optimizer timing report
produced by [`openvm-riscv/scripts/compare_apc_optimizers.sh`](../openvm-riscv/scripts/compare_apc_optimizers.sh)
(the `time-optimizers` CLI subcommand). Follows the same "shareable via URL, no
Python needed" pattern as the [metrics-viewer](../openvm/metrics-viewer/) and
[autoprecompile-analyzer](../autoprecompile-analyzer/).

## Usage

Open `index.html` in a browser and either:
- drag & drop the timing JSON anywhere on the page,
- click **Open file**, or
- paste a link to a hosted JSON (GitHub blob URLs are auto-converted to raw) and
  press Enter. The URL is encoded into `?data=<url>` so the view is shareable.

```
index.html?data=<url-encoded-link-to-json>
```

## Layout

- **Left** — a sortable table of every APC: benchmark, APC index, lean runtime,
  powdr (Rust) runtime, the `lean / powdr` slowdown factor, and how the runtimes
  were measured (`isolated` = re-timed alone on the machine and comparable across
  optimizer versions; `loaded` = measured with the whole pool busy). Click any
  header to sort (toggles ascending/descending). A benchmark filter narrows the
  rows.
- **Right, top** — histogram of slowdown factors across the (filtered) APCs,
  with a reference line at 1× (optimizers equal).
- **Right, bottom** — scatter plot with selectable x/y axes (constraints, bus
  interactions, variables, powdr runtime, lean runtime, slowdown factor) and a
  log-scale toggle for both axes. Points are colored per benchmark; clicking a
  point or a table row cross-highlights the other.

## Input format

```json
{"openvm-eth-version": "<hash>",
 "parallelism": 48,
 "benchmarks": [{"name": "keccak",
                 "apcs": [{"variables": 62, "constraints": 53, "bus_interactions": 31,
                           "rust_runtime": 0.05, "lean_runtime": 0.11,
                           "isolated": false}, ...]}]}
```

"powdr" in the UI is the native Rust optimizer (`rust_runtime`); "lean" is the
apc-optimizer (`lean_runtime`).

`parallelism` is how many threads the benchmark ran with, and `isolated` marks
the circuits it re-timed serially afterwards. Contention inflates runtimes by up
to ~3.4x, unevenly across circuits and across the two optimizers, so only the
`isolated` ones can be compared across optimizer versions — the rest are
comparable within a report. Both fields are optional: reports produced before
they existed render as `loaded` with no thread count shown.
