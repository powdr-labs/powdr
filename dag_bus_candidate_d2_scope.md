# Candidate D-2: Batched bus DAG kernel (single launch per segment)

## Goal
Replace the per-chip `apc_apply_bus_dag_kernel<<<…>>>` launches with **one
batched launch** per segment, dispatching one block per (chip, row-tile)
across all powdr chips in the segment. The driving hypothesis is that
launch overhead + low-occupancy small-chip launches account for a
non-trivial fraction of the 135.8 ms bus_kernel total.

## Where the launches happen today
- `openvm/src/powdr_extension/trace_generator/cuda/mod.rs:721-739` —
  per-chip `cuda_abi::apc_apply_bus_dag(...)` inside
  `try_generate_witness`, called once per `PowdrChipGpu` from
  `generate_proving_ctx` (`mod.rs:766-779`).
- Each call uses the same periphery buffers
  (`range_checker.count`, `tuple_range_checker.count`,
  `bitwise_lookup_8.count`), so a batched kernel can share those pointers
  across all chips in the segment.

## Profiling baseline (option-2 winner, no D1)
| Stat | Value |
|---|---|
| Launches per RSP proof | 704 (≈88 chips × 8 segments) |
| Total bus_kernel time | 135.83 ms |
| Average per launch | 192.9 µs |
| Min / median / max | 13 µs / 123 µs / 1 975 µs |
| Bus_kernel share of total proof | 2.3 % (135.8 / 5 860 ms) |

## Realistic ceiling

Launch overhead is ≈5 µs/launch on RTX 5090:
- `704 × 5 µs = 3.5 ms` removable launch overhead
- Plus possible occupancy gain on the ~200 small launches (≤30 µs) where
  grid_dim is below the SM count (170). Optimistic: 3–4 ms recovered.

**Best plausible D-2 win: ~7 ms (~5 % of bus_kernel, ~0.1 % of total proof).**

This is small. Stating it up front because the scope and risk below are
not small — see "Alternatives" at the end.

## Architectural risk (carry-over from option-2 review)

The reviewer flagged `openvm/.../extensions.rs:696-715` (now in the
openvm git dep, not in this repo's tree) as the hook needed to defer
per-chip work. The honest situation:

1. `generate_proving_ctx` is called per-chip by openvm's prover. There
   is **no existing "before-periphery / after-all-powdr-chips" hook** we
   can attach to. The order is: each `PowdrChipGpu::generate_proving_ctx`
   runs (does tracegen + bus_kernel), then periphery chips
   (`VariableRangeCheckerChipGPU`, `BitwiseOperationLookupChipGPU`,
   `RangeTupleCheckerChipGPU`) run their proving ctx.
2. To batch, we have to:
   - **Defer** each per-chip bus_kernel launch into a `Vec<ChipBatch>`
     shared across the segment.
   - **Drain** that Vec into a single launch *before* any periphery
     chip's `generate_proving_ctx` runs (the periphery chips consume the
     histograms our kernel writes).
3. The cleanest place is to wrap the openvm `ChipInventory` proving
   pipeline, or to add a `flush_pending_bus_kernels()` call inside the
   first periphery chip's `generate_proving_ctx`. The latter requires a
   shared `Arc<Mutex<...>>` on the periphery wrapper struct
   (`PowdrPeripheryInstancesGpu` at `periphery.rs:30-39`) — feasible but
   intrusive.

## Proposed kernel signature

```cpp
struct ChipBatch {
    const Fp     *trace;         // d_trace per chip
    int           height;
    uint32_t      width;
    const Rule   *rules;
    uint32_t      rules_len;
    DevInteractionDag dag;
    const OutputDesc *outputs;
    uint32_t      num_outputs;
    Fp           *intermediates; // sized for this chip's slot count
};

__global__ void apc_apply_bus_dag_batched(
    const ChipBatch       *chips,
    const uint32_t        *block_offsets,   // prefix sum of grid_dim per chip
    uint32_t               num_chips,
    uint32_t               var_range_bus_id,
    uint32_t              *var_range_hist,
    uint32_t               tuple2_bus_id,
    uint32_t              *tuple2_hist,
    uint8_t                tuple2_sizes_x,
    uint8_t                tuple2_sizes_y,
    uint32_t               bitwise_bus_id,
    uint32_t              *bitwise_hist);
```

Grid: `total_blocks = sum(grid_dim_per_chip)`. Per block: binary-search
`block_offsets` to find `chip_idx`, then local block index
`bid - block_offsets[chip_idx]` drives the row-tile. Everything else
mirrors the existing per-chip kernel body.

## Host changes

1. **Defer launches**:
   `try_generate_witness` no longer calls `apc_apply_bus_dag`. Instead,
   it pushes `(d_rules, d_dag, d_outputs, d_intermediates, height, width,
   trace_ptr)` into a `Vec<ChipBatch>` on a segment-scoped
   `Mutex<Vec<...>>`.
2. **Flush hook**: a `flush_bus_kernels()` call wired into whichever
   periphery chip runs first in `generate_proving_ctx` order — practical
   approach is `VariableRangeCheckerChipGPU`, which is always present.
   Requires either:
   - A small openvm-side patch (1-2 LOC) to call a powdr-provided
     callback before the first periphery proving ctx, **or**
   - A "monitor" trait method on `PowdrPeripheryInstancesGpu` that the
     first periphery chip's wrapper invokes — but periphery chips are
     openvm types we don't own.
   - **Or**: a one-shot `OnceLock` flushed from a custom `Drop` on the
     last `PowdrChipGpu` in segment order. Fragile (depends on drop
     order) — avoid.
3. **Concat host data**: build `block_offsets` Vec, copy ChipBatch array
   H2D once per segment.

Net LOC: ~120 host + ~80 device + 1 openvm patch.

## Risks

| # | Risk | Likelihood | Mitigation |
|---|---|---|---|
| R1 | No clean openvm hook; have to patch openvm git dep | High | Use periphery wrapper; if not feasible, fork openvm with a thin `before_periphery` callback (already forked). |
| R2 | Per-chip intermediates buffer must stay live until flush. Currently freed when `try_generate_witness` returns. | Medium | Move intermediates ownership into the deferred ChipBatch Vec. |
| R3 | Atomic contention on shared histograms increases with batched grid (more concurrent blocks → more atomic ops on same `bitwise_count`). | Low | Already atomics; current code has same contention but staggered across launches. May actually *worsen* perf. |
| R4 | Achievable speedup is ~5 % of bus_kernel = ~3 ms wall time | Certain | Bound it up-front; do not pursue unless A1/A2 below are also blocked. |

## Alternatives that likely outperform D-2

Given the 3–7 ms ceiling, listing alternatives before committing to D-2:

- **A1. Fuse `apc_tracegen` + `bus_kernel` into one kernel**.
  Currently the bus kernel re-reads the trace matrix that was just
  written by tracegen. Fusing eliminates an entire global read/write
  round-trip of the trace. Expected: ~20–40 % bus_kernel reduction
  (~30 ms). Higher impact, similar refactor cost.

- **A2. Shared-memory `inter[]` for small-slot chips** (Candidate A in
  earlier scope). Chips with `LOCAL_K ≤ 256` get shared-mem dispatch
  instead of slot-major global. GKR uses this. Expected: ~10–20 % on
  small-to-medium chips. Bounded by `LOCAL_K` distribution — needs to
  be profiled first.

- **A3. Move `bus_compile_h2d` to APC build time** (already noted in
  memory). Saves ~3 ms host work; near-zero risk. Should ship
  independently regardless of D-2 decision.

## Recommendation

**Defer D-2.** Do A3 first (free), then A2 (medium effort, bigger win
than D-2), then re-measure. D-2's ceiling (~5 % of bus_kernel) is
below the noise floor of the other prover sub-stages and the
architectural intrusion into openvm's chip lifecycle is significant.

If D-2 still looks worth it after A2 + A3, the scope above is
implementable in ~200 LOC with one openvm patch.
