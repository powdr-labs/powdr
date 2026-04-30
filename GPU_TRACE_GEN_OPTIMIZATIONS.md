# GPU Trace Gen Optimizations

## Results

```
                                Baseline    +G4
trace_gen                       1,203ms    1,159ms
gpu_powdr_create_dummy_chips       32ms       ~0ms   ← cached
gpu_powdr_dummy_traces            770ms      779ms   ← unchanged, needs OpenVM changes
CUDA kernels                        2ms        2ms
```

G4 (cache dummy chip complex): 32ms saved. Committed.
G1-G3 require modifying OpenVM's GPU chip trace generation infrastructure.

### Detailed dummy_traces analysis

Only 3 invocations (one dominant APC per segment), not 90:
- Per invocation: ~260ms, ~2.3B cells
- Individual chips are <1ms each but produce large traces (up to 524K rows)
- The cost is real GPU compute + memory allocation for full trace matrices
- Each invocation generates full traces for ~4 instruction chips, then only
  a subset of columns are read by the APC substitution kernel

### Detailed dummy_traces profiling (per-chip)

The cost is concentrated in 1 dominant APC per segment (95%+ of time):

```
Segment 1 dominant APC (381ms):
  BaseAluCoreAir:     33,554,432 rows x 36 cols = 1.2B cells  (177ms)
  LoadStoreCoreAir:   16,777,216 rows x 41 cols = 688M cells  ( 46ms)
  ShiftCoreAir:       16,777,216 rows x 53 cols = 889M cells  ( 81ms)
  BranchEqualCoreAir:    131,072 rows x 26 cols = 3.4M cells  ( 12ms)
  Total: ~2.8B cells

All other APCs: <6ms each (tiny traces, 4K-32K rows)
```

The 33M rows are real: 98K APC calls × ~340 instructions per call.
The APC substitution only reads ~10-15 of 36-53 columns per chip.

### What's needed for G1-G3

These require changes INSIDE OpenVM's GPU chip infrastructure (~/openvm):
- Modify `Chip::generate_proving_ctx()` to accept a column mask
- Or create a new API for partial trace extraction
- Or fuse substitution into the trace generation kernel

Estimated savings from G1: 796ms -> ~318ms (2.5x on dummy_traces).
For reth (larger workload), proportionally larger absolute savings.

## Baseline (keccak 10K hashes, APC=30, mock prove, CUDA build)

```
trace_gen                          1,203ms  100%
  gpu_powdr_dummy_traces             770ms   64.0%  <<<< BOTTLENECK
  gpu_powdr_create_dummy_chips        32ms    2.7%
  CUDA kernels (tracegen+derived+bus)  2ms    0.2%
  system_trace_gen (memory/merkle)   ~400ms  33.1%

12 dummy chips generated, ~7B total cells in GPU matrices
Individual chips all <1ms — cost is aggregate GPU alloc + kernel launch overhead
```

GPU overhead: APC=0 800ms → APC=30 1,203ms (1.51x)

## Optimizations (descending expected benefit)

### G1: Selective column extraction from record arenas

- **Target**: gpu_powdr_dummy_traces (770ms)
- **Expected**: 3-5x reduction (770ms → 150-250ms)
- **What**: Currently generates full padded GPU trace matrices for each of 12
  original instruction chips, producing ~7B total cells. The APC substitution
  kernel then reads only a small subset of columns from these matrices. Most
  allocated data is never read.
- **Optimization**: Instead of calling `chip.generate_proving_ctx(record_arena)`
  which allocates a full DeviceMatrix with all columns padded to power-of-2
  height, extract only the column values needed by the APC's substitution map
  directly from the DenseRecordArena. This avoids:
  - GPU memory allocation for unused columns
  - Kernel launch overhead for full trace generation
  - Power-of-2 padding of unused rows
- **Risk**: Medium. Requires understanding how DenseRecordArena stores per-chip
  records and whether column values can be extracted without running the chip's
  full trace generation logic (some chips compute auxiliary columns).
- **Measurement**: Compare gpu_powdr_dummy_traces before/after. Track
  gpu_powdr_dummy_total_cells reduction.

### G2: Batch dummy chip trace generation

- **Target**: gpu_powdr_dummy_traces (770ms)
- **Expected**: 1.5-2x reduction
- **What**: The 12 dummy chips are generated serially via an iterator. Each
  chip.generate_proving_ctx() launches GPU kernels independently, causing 12
  separate kernel launch + sync cycles. Batching multiple chips into a single
  kernel launch (or at least overlapping transfers with computation via CUDA
  streams) would reduce launch overhead.
- **Optimization**: Use CUDA streams to overlap chip trace generation. Launch
  all 12 chips' kernels without synchronizing between them, then sync once at
  the end. Or batch small chips into a single kernel call.
- **Risk**: Low-medium. Requires changes to how ChipInventory generates
  proving contexts — currently each chip returns a separate DeviceMatrix.
- **Measurement**: Compare gpu_powdr_dummy_traces wall time before/after.
  The per-chip times are already <1ms, so the overhead is kernel launch
  serialization, not computation.

### G3: Fuse substitution into dummy trace generation

- **Target**: gpu_powdr_dummy_traces (770ms) + gpu_powdr_apc_tracegen (1ms)
- **Expected**: 1.3-2x reduction on dummy_traces
- **What**: Currently the pipeline is: (1) generate full dummy trace matrices,
  (2) run apc_tracegen kernel to copy selected columns to APC output. This
  means all dummy trace data must be materialized in GPU memory before
  substitution. If substitution were fused into the trace generation kernel
  itself, dummy traces would never need to be fully materialized — each row
  would write directly to the APC output columns.
- **Optimization**: Modify the GPU trace generation to accept the substitution
  map as input and write directly to the APC output matrix instead of to
  intermediate dummy matrices. This eliminates the intermediate DeviceMatrix
  allocations entirely.
- **Risk**: High. Requires modifying the GPU chip trace generation API in
  OpenVM, which is a cross-cutting change. Each chip's CUDA kernel would need
  to know about the APC column mapping.
- **Measurement**: Eliminate gpu_powdr_dummy_traces entirely. The apc_tracegen
  kernel becomes the trace generation kernel. Track total PowdrAir
  single_trace_gen time.

### G4: Reduce gpu_powdr_create_dummy_chips overhead

- **Target**: gpu_powdr_create_dummy_chips (32ms)
- **Expected**: 2-5x reduction (32ms → 6-16ms)
- **What**: Creates a fresh dummy chip complex for every APC chip, every
  segment. This involves constructing AIR objects, chip wrappers, and
  periphery instances. With 30 APCs × 3 segments = 90 constructions.
  Most of this setup is identical across invocations.
- **Optimization**: Cache the dummy chip complex across APC chips within the
  same segment (or even across segments if the VM config doesn't change).
  Only create it once per segment and share the chip inventory.
- **Risk**: Low. The dummy chip complex is read-only after construction.
  Need to verify no mutable state leaks between APC invocations.
- **Measurement**: Compare gpu_powdr_create_dummy_chips before/after.

### G5: Skip dummy traces for zero-row chips

- **Target**: gpu_powdr_dummy_traces (770ms)
- **Expected**: Minor (depends on how many chips have zero rows)
- **What**: The current code calls generate_proving_ctx for all chips that
  have a record arena, then filters out zero-height results. If we can
  determine the row count from the record arena before calling
  generate_proving_ctx, we avoid the GPU allocation + kernel launch for
  chips that would produce empty traces.
- **Optimization**: Check record arena size before generating the trace.
  Skip chips with empty arenas (already done via take_real_arena returning
  None) or arenas that would produce zero rows.
- **Risk**: Very low. Simple filter.
- **Measurement**: Track number of chips skipped vs generated.

## Measurement

All GPU runs use:
```bash
cargo build --bin powdr_openvm_riscv --release --features cuda,metrics
./target/release/powdr_openvm_riscv prove guest-keccak --input 10000 \
    --autoprecompiles 30 --mock \
    --compiled /tmp/tracegen-bench/keccak_gpu_apc030.cbor \
    --metrics /tmp/tracegen-bench/metrics.json
```

Key metrics:
- `gpu_powdr_dummy_traces_time_ms` (G1-G3, G5)
- `gpu_powdr_create_dummy_chips_time_ms` (G4)
- `gpu_powdr_dummy_total_cells` (G1 — track reduction)
- `gpu_powdr_dummy_chip_count` (G5)
- `single_trace_gen_time_ms` for PowdrAir (overall)
- `trace_gen_time_ms` (total)

## Context: Why SP1's GPU optimizations don't directly apply

SP1 keeps APC trace generation entirely on CPU (`supports_device_main_tracegen()`
returns false). Their optimizations (two-phase cache, interaction tiers,
LinearCombination) target CPU bus replay. Powdr's GPU path already runs bus
replay as a CUDA kernel (<1ms), making those optimizations irrelevant.

The shared bottleneck between CPU and GPU paths is dummy trace generation
(generating original instruction traces for APC's constituent instructions).
On CPU this is 8.0s (post-O1+O2+O5), on GPU it's 770ms. Both paths would
benefit from G1 (selective column extraction).
