# Candidate #2: GLOBAL/local intermediate buffer split

## Goal
Mirror GKR's `BUFFER_THRESHOLD`-gated dispatch: small DAGs use a
stack-allocated `Fp local_buffer[THRESHOLD]` (nvcc spills into registers
or per-thread local memory but accesses are L1-resident), large DAGs
keep the existing slot-major global `d_intermediates`. Pick the kernel
variant per-chip at launch time based on the chip's `buffer_size`.

## Why this is the right next step
- GKR pattern: `zerocheck_round0.cu:23` `constexpr uint32_t BUFFER_THRESHOLD = 16;`
  → `local_buffer[GLOBAL ? 1 : BUFFER_THRESHOLD * NUM_COSETS]` at line
  214 → runtime branch `bool is_global = buffer_size > BUFFER_THRESHOLD;`
  at line 692 dispatches to one of the templated instantiations.
- Today our kernel unconditionally does
  `inter[slot * inter_stride] = val;` for every rule write and
  `inter[d.value * inter_stride]` for every interaction-output read.
  That's a global-memory round-trip per intermediate access on every
  chip, no matter how small the DAG.
- For chips with `buffer_size ≤ 16`, the entire intermediate working
  set fits in registers. Eliminates all `inter[]` global traffic on
  those chips. Most short chips also have small rule lists, so the
  ratio of intermediate-access cost to total kernel cost is high.

## Kernel design

### Template signature
```cpp
template <bool GLOBAL, uint32_t LOCAL_K>
__global__ void apc_apply_bus_dag_kernel(
    const Fp *__restrict__ d_output,
    int num_apc_calls,
    uint32_t apc_height,
    const Rule *__restrict__ d_rules,
    uint32_t n_rules,
    const DevInteractionDag *__restrict__ d_interactions,
    uint32_t n_interactions,
    const OutputDesc *__restrict__ d_output_descs,
    Fp *__restrict__ d_intermediates,   // ignored when !GLOBAL
    uint32_t var_range_bus_id,
    uint32_t *__restrict__ d_var_hist,
    uint32_t var_num_bins,
    uint32_t tuple2_bus_id,
    uint32_t *__restrict__ d_tuple2_hist,
    uint32_t tuple2_sz0,
    uint32_t tuple2_sz1,
    uint32_t bitwise_bus_id,
    uint32_t *__restrict__ d_bitwise_hist);
```

### Body — split branching
```cpp
const uint32_t total_threads = gridDim.x * blockDim.x;
const uint32_t thread_id = blockIdx.x * blockDim.x + threadIdx.x;
const uint32_t r = thread_id;
if (r >= (uint32_t)num_apc_calls) return;

Fp local_buf[GLOBAL ? 1 : LOCAL_K];
Fp *inter;
uint32_t inter_stride;
if constexpr (GLOBAL) {
    inter = d_intermediates + thread_id;
    inter_stride = total_threads;
} else {
    inter = local_buf;
    inter_stride = 1;
}
// rest of body identical, indexing inter[slot * inter_stride]
```

`if constexpr` forces both arms to be compile-time pruned. The
`local_buf[GLOBAL ? 1 : LOCAL_K]` declaration costs 4 B of stack in the
GLOBAL branch — negligible. Without `if constexpr` nvcc may keep the
local_buf live in the GLOBAL path; with it, the array is eliminated.

### Threshold value
GKR uses 16. The CUDA programming guide recommends ≤ 32-64 registers of
local state to stay in registers reliably on modern SMs. Each
`Fp local_buf[K]` slot is 4 bytes. K=16 → 64 B/thread → likely fits in
registers; K=32 → 128 B/thread → may spill but stays L1-hot.

Plan: **`THRESHOLD = 16`** as the first cut (match GKR), with a CLI/env
override (`POWDR_BUS_DAG_LOCAL_K`) so we can sweep 8/16/32 in
benchmarking without recompile loops. Default 16. If sweep shows 32
strictly dominates, raise it.

### Host dispatcher
```cpp
extern "C" int _apc_apply_bus_dag(... uint32_t buffer_size ...) {
    const bool is_global = buffer_size > LOCAL_THRESHOLD;
    const int block_x = 128;
    dim3 block(block_x), grid(div_ceil(num_apc_calls, block_x));
    if (is_global) {
        apc_apply_bus_dag_kernel<true, 1><<<grid, block>>>(...);
    } else {
        apc_apply_bus_dag_kernel<false, LOCAL_THRESHOLD><<<grid, block>>>(...);
    }
    return cudaGetLastError();
}
```
The `d_intermediates` pointer is `nullptr` (or skip-allocation) when
`!is_global`. The kernel reads it only in the GLOBAL arm thanks to
`if constexpr`.

## Host changes

### `cuda_abi.rs`
- Add `buffer_size: u32` parameter to `_apc_apply_bus_dag`.
  Used host-side to pick the kernel variant — still passed in for
  symmetry / future debug.

### `trace_generator/cuda/mod.rs:656-680`
- Move `buffer_size` out of the `bus_dag_inputs` tuple's anonymous slot
  into a named field (`Option<(rules, interactions, output_descs,
  Option<intermediates>, buffer_size)>`).
- When `buffer_size <= LOCAL_THRESHOLD`, **skip the
  `DeviceBuffer::with_capacity(inter_len)` allocation entirely**. Pass
  `std::ptr::null_mut()` for `d_intermediates`. This is the
  zero-allocation path for small chips.
- When `buffer_size > LOCAL_THRESHOLD`, behave exactly as today.

### Result
For typical RSP run with ~88 chip launches per segment, expect that the
majority of launches drop into the local-mode path. The `bus_compile_h2d`
substage shrinks proportionally (DeviceBuffer allocation has measurable
cost at high APC counts).

## Validation

### Correctness — bit-equal proof
Run `cli-openvm-riscv prove guest-keccak --autoprecompiles 10`
with `POWDR_BUS_KERNEL=dag`. Expected `public_values_commit`:
```
[1093111822, 928887173, 1821551885, 633297552,
 927835968, 221699809, 380879836, 1130691046]
```
(matches the option-2 winner baseline from memory.) Any drift = bug
in the local-mode branch.

Then sweep `POWDR_BUS_DAG_LOCAL_K=8` / `16` / `32` and confirm
identical commit at each — threshold should be a perf knob with no
correctness impact.

### Perf — nsys baseline
Baseline (option-2 winner, all global): 135.83 ms across 704 launches,
avg 192.9 µs. Measure with `POWDR_BUS_KERNEL=dag` after the change:
- Expect ~15–30 % bus_kernel reduction.
- Stretch: 40 % if small chips dominate the launch distribution.

### Stats run — confirm distribution before merging
Run with `POWDR_DUMP_BUS_DAG=1` (already wired at expr_dag.rs:323) and
collect `buffer_size` per chip. Expected: bimodal distribution with
many chips at `buffer_size ≤ 16` and a long tail up to ~1762 (the
max we previously observed). If buffer_size is mostly large, the
optimization is dead-on-arrival — fall back to plan #1
(`__launch_bounds__`).

## Risks

| # | Risk | Likelihood | Mitigation |
|---|---|---|---|
| R1 | Local array spills to per-thread local memory (lmem) — same trap as before | Medium | Cap THRESHOLD at 16 (64 B/thread); measure with `nvcc --resource-usage` and inspect `stack frame` of the local-mode instantiation. If stack frame > 0, drop K or revert. |
| R2 | The `local_buf[GLOBAL ? 1 : K]` declaration in the GLOBAL path forces nvcc to allocate the array anyway | Low | `if constexpr` guard around the assignment + declare local_buf inside the `else` branch. (See ”final body sketch” below.) |
| R3 | Indirect `inter[slot]` accesses with non-constant `slot` (slot index comes from decoded rule) force nvcc to fall back to local memory even for small K, because indirection prevents register-allocation | High — this is the core risk | Most non-constant indices through small arrays still land in registers via predicated moves on sm_80+; sm_120 (Blackwell) is more permissive. Need to verify by reading PTX of the resulting kernel. If indirection forces lmem, perf is no worse than today (still L1-hot vs DRAM). |
| R4 | Threshold sweep noise | Low | Median of 3 runs; report variance. |
| R5 | Kernel binary size doubles (2 instantiations) | Low | Acceptable; ~10 KB. |

## Final body sketch (correctness-critical)

```cpp
template <bool GLOBAL, uint32_t LOCAL_K>
__global__ void apc_apply_bus_dag_kernel(... Fp *__restrict__ d_intermediates ...) {
    const uint32_t total_threads = gridDim.x * blockDim.x;
    const uint32_t thread_id = blockIdx.x * blockDim.x + threadIdx.x;
    const uint32_t r = thread_id;
    if (r >= (uint32_t)num_apc_calls) return;

    // Two paths share the same inter[]/inter_stride abstraction.
    // `if constexpr` is critical: it physically removes the unused branch
    // including the local_buf declaration in the global path.
    Fp *inter;
    uint32_t inter_stride;
    if constexpr (GLOBAL) {
        inter = d_intermediates + thread_id;
        inter_stride = total_threads;
    } else {
        // Stack-allocated; nvcc may put in registers for small K.
        Fp local_buf[LOCAL_K];
        inter = local_buf;
        inter_stride = 1;
    }
    // ... rest identical to today, body unchanged.
}
```
**Problem with the sketch above**: `local_buf` falls out of scope at the
end of the `else`. Fix: hoist the declaration to function scope, gate
its size with the ternary, and assign `inter` per branch:

```cpp
template <bool GLOBAL, uint32_t LOCAL_K>
__global__ void apc_apply_bus_dag_kernel(...) {
    ...
    if (r >= (uint32_t)num_apc_calls) return;

    Fp local_buf[GLOBAL ? 1 : LOCAL_K];
    Fp *inter;
    uint32_t inter_stride;
    if constexpr (GLOBAL) {
        (void)local_buf;
        inter = d_intermediates + thread_id;
        inter_stride = total_threads;
    } else {
        inter = local_buf;
        inter_stride = 1;
    }
    // rest of body unchanged
}
```
With `local_buf[1]` and `(void)local_buf` in the GLOBAL path, nvcc
elides the array.

## Implementation order

1. **Kernel templating** — `openvm/cuda/src/apc_apply_bus_dag.cu`. Make
   the kernel a template, add `if constexpr` branching, leave body
   otherwise unchanged. Add explicit `template instantiation` lines so
   `_apc_apply_bus_dag` can refer to both variants from extern "C".
2. **Host dispatcher** — same file, `_apc_apply_bus_dag`: add
   `buffer_size` param, read `POWDR_BUS_DAG_LOCAL_K` once (cached in a
   static), branch on `buffer_size > k`.
3. **FFI** — `cuda_abi.rs`: add `buffer_size: u32` to extern signature.
4. **Host driver** — `trace_generator/cuda/mod.rs`: thread `buffer_size`
   through, skip `DeviceBuffer` allocation when local mode applies.
   `&intermediates` becomes `intermediates.as_ref().map(|b| b as *const _)
   .unwrap_or(std::ptr::null())`.
5. **Validate** — bit-equal proof on guest-keccak APC=10.
6. **Bench** — nsys on RSP/keccak with `POWDR_BUS_KERNEL=dag`.
7. **Sweep** — K ∈ {8, 16, 32}, pick best.

Estimated LOC: ~80 (40 kernel + 30 host + 10 FFI).

## Expected outcome

Best case (most chips have `buffer_size ≤ 16` and indirection doesn't
force lmem): 30 %+ reduction → bus_kernel ≈ 95 ms.
Conservative (50 % of chips qualify, indirection partial lmem): 15 %
reduction → bus_kernel ≈ 115 ms.
Failure mode (R3 hits universally): ~0 % gain, no regression — local
path falls back to L1-resident lmem which is still strictly better
than today's slot-major DRAM. Bail-out plan: keep templating
infrastructure, revert dispatcher to always pick GLOBAL.
