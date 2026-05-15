# Candidate D: increase concurrent warp count for small-chip bus_kernel launches

## Problem (data-supported)

ncu data from `apc_apply_bus_dag_kernel` at pairing APC=500, launches sampled at skip=100 and skip=500:

```
grid                        = (3, 1, 1) and (4, 1, 1)    — 24–32 warps total
Active Warps Per Scheduler  = 1  (of ~12 max)
No Eligible                 = 94.86% – 95.27%
Memory Throughput           = 1.94 GB/s   (peak ≈ 1 TB/s ⇒ 0.2%)
Mem Busy                    = 0.80%
L1/TEX Hit Rate             = 66.84%
L2 Hit Rate                 = 79.75%
```

The small-chip kernels are **launch-bound, not memory-bound**. Memory subsystem sits idle while schedulers stall waiting for runnable warps. There's no point optimizing memory access patterns (shmem, etc.) — the bottleneck is "not enough warps shipped to the GPU per launch."

Time distribution (nsys, n=704 launches):
- avg 192 µs, median 122 µs, max 1.97 ms
- Small/medium launches (≤200 µs) account for ~70% of total bus_kernel time
- Big launches (>1 ms) account for ~10-15%
→ Small-chip optimization is where the gains are. Big-chip optimization is a secondary lever.

## Two structural fixes

### Option D1 — multi-stream concurrent execution (smaller change)

Each PowdrAir's bus_kernel runs on its own stream. CUDA's hardware scheduler concurrently dispatches multiple bus_kernels onto unused SMs. A 24-warp launch can run alongside 9 other 24-warp launches → 240 concurrent warps → SMs fill up.

**Implementation:**
- Allocate a small pool of streams (e.g. 8 or 16) in `PowdrPeripheryInstancesGpu` (or thread-local in the dispatcher).
- `apc_apply_bus_dag` takes a `stream` parameter; passes via `kernel<<<grid, block, 0, stream>>>`.
- Same for the H2D copies on the same stream.
- Each PowdrChipGpu rotates through the pool (chip i → stream i % pool_size).
- Before periphery chips process histograms, `cudaDeviceSynchronize` to ensure all per-chip bus_kernels finished.

**Cost:** ~50 LOC across `cuda_abi.rs`, `apc_apply_bus_dag.cu`, `mod.rs`. Stream lifecycle managed by openvm-cuda-common — there's an existing `Stream` abstraction (need to verify).

**Risk:** periphery histograms (`d_var_hist`, `d_tuple2_hist`, `d_bitwise_hist`) are written via atomicAdd from every stream. Atomic contention scales linearly with concurrency. For hot bins (e.g. `is_valid=1` writes from every chip every row), this could nullify the win or make it slower. Need to measure.

**Best case:** 4-8× concurrent execution → small-chip kernel time drops 50-75%.
**Worst case:** atomic contention dominates → no improvement or regression.

### Option D2 — batched bus_kernel across all chips in a segment (bigger change)

Defer per-chip bus_kernel launches; at end of segment (after all PowdrAirs traced), launch ONE batched kernel that walks all chips' rules and dispatches all chips' interactions.

**Kernel signature sketch:**
```cpp
__global__ void apc_apply_bus_dag_batched_kernel(
    const ChipMetadata* d_chip_meta,         // per-chip headers
    const uint32_t* d_block_chip_map,        // block_idx → chip_idx
    const Rule* d_rules_concat,              // concat'd from all chips
    const DevInteractionDag* d_interactions_concat,
    const OutputDesc* d_output_descs_concat,
    Fp* d_intermediates_concat,
    // periphery histograms unchanged
    ...
)
{
    uint32_t chip_idx = d_block_chip_map[blockIdx.x];
    ChipMetadata m = d_chip_meta[chip_idx];
    uint32_t row = (blockIdx.x - m.block_start) * blockDim.x + threadIdx.x;
    if (row >= m.num_apc_calls) return;

    const Rule* rules     = d_rules_concat + m.rules_off;
    const DevInteractionDag* intrs = d_interactions_concat + m.interactions_off;
    const OutputDesc* descs = d_output_descs_concat + m.output_descs_off;
    Fp* inter = d_intermediates_concat + m.intermediates_off + thread_offset_in_chip;
    // ... rule walk + dispatch identical to single-chip kernel
}
```

**Host-side flow:**
1. New buffer accumulator collects per-chip rules/interactions/descs as each PowdrChipGpu calls `try_generate_witness`. Stash in a per-segment buffer.
2. Skip the per-chip bus_kernel launch.
3. After all PowdrChipGpus in a segment have run, but before periphery chips process histograms: fire the batched kernel.

**Cost:** ~250–400 LOC across kernel, FFI, host emitter, and the deferred-launch hook.

**Architectural challenge:** there's no natural "end-of-segment / before-periphery" hook in openvm's framework. `PowdrChipGpu::generate_proving_ctx` returns the trace; periphery chips' `generate_proving_ctx` runs later. To insert the batched kernel between them, we either:
- Trigger lazily on first periphery-chip access to the histograms (requires plumbing in periphery chips), OR
- Add a "before-segment-finalize" hook to openvm (upstream change), OR
- Convert powdr's per-chip generator into a once-per-segment generator that emits all PowdrAir traces at once (large refactor).

**Best case:** kernel grid = sum of all chips' blocks = ~700 blocks → SMs fully occupied → small-chip latency mostly disappears. Total bus_kernel time could drop 60-80%.
**Worst case:** atomic contention same as D1; deferred launch architecture is complex and error-prone.

## Comparison

| dimension | D1 (streams) | D2 (batched kernel) |
|---|---|---|
| LOC | ~50 | ~300+ |
| Architectural change | minimal (per-stream isolation) | large (deferred launch + cross-chip batching) |
| Likely concurrency gain | 4-8× concurrent kernels | up to 50× concurrent blocks (if SMs are the limit) |
| Atomic contention risk | linear in stream count | linear in block count |
| Reversibility if it doesn't win | easy: drop streams, restore default-stream code | hard: deferred-launch hook stays in the way |
| Measurement plan | A/B same workload with stream pool size 1 vs 4 vs 8 | A/B requires implementation; cost-prohibitive to validate before-building |

## Recommendation

**Do D1 first.** Smaller change, easily reversible, directly tests the hypothesis "more concurrent warps fix the launch-bound issue." If D1 wins, we know batching would win even more — but D1 captures most of the gain at 1/6 the cost. If D1 doesn't win (atomic contention dominates), D2 won't either, and we save the bigger rewrite.

## D1 implementation plan

1. **Confirm stream availability in openvm-cuda-common.** Look for `Stream` type, `cudaStreamCreate` wrapper, async kernel launch helpers.
2. **Add `stream: cudaStream_t` to `_apc_apply_bus_dag` extern signature** + corresponding Rust safe wrapper.
3. **Modify kernel launch** in `apc_apply_bus_dag.cu` to `apc_apply_bus_dag_kernel<<<grid, block, 0, stream>>>` and use `cudaMemcpyAsync(..., stream)` for any per-launch H2D (currently we use `to_device()` which probably defaults to NULL stream — verify).
4. **Stream pool in mod.rs:** introduce a small ring (8 streams). Each `try_generate_witness` call picks the next stream. Periphery chip's `generate_proving_ctx` synchronizes the pool first.
5. **Intermediates buffer per-launch:** confirm DeviceBuffer alloc doesn't implicitly synchronize the default stream (which would defeat the purpose).
6. **Bench gates:**
   - Bit-equal proof on pairing APC=10 and APC=500 (both must match VM).
   - nsys top-kernels comparison vs current DAG-global single-stream baseline.
   - Acceptance: ≥15% reduction in `apc_apply_bus_dag_kernel` total time on APC=500. <15% → revert.
7. **ncu re-measurement** on a small-chip launch under D1 to verify Active Warps Per Scheduler went up (≥4 expected) and No Eligible dropped (<70% expected).

## Bench protocol for D1

- Baseline: current `prototype/dag-bus` HEAD (single-stream global-mode DAG).
- Variant: D1 with stream pool sizes 1, 4, 8, 16.
- Workload: pairing APC=500, warm APC cache.
- Metric: nsys `cuda_gpu_kern_sum` for `apc_apply_bus_dag_kernel`.
- ncu: same launch position (skip=100) under each variant — Active Warps Per Scheduler, No Eligible, Mem Busy.
- Correctness: bit-equal `public_values_commit` against VM (canonical: `[1093111822, 928887173, ...]`).

If D1 wins, scope D2 only if we want to push further. If D1 loses (atomic contention), pivot away from concurrency-based fixes entirely.
