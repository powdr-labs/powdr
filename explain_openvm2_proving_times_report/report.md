# OpenVM 2 STARK Proving Time Analysis

This report analyzes why the "STARK (excl. trace)" proving time doesn't scale linearly with trace cells, constraint instances, and bus interaction messages when using autoprecompiles (APCs). The analysis is based on benchmarks of the Pairing and Keccak guests with varying numbers of APCs, run on a GPU server using the `openvm-v2-integration` branch.

## 1. Confirming the Observations

The observations from the problem statement are confirmed by the data. See `analysis_output.txt` and `cost_model_output.txt` for full numbers.

### Pairing Guest: apc000 (baseline) vs apc500

| Metric | apc000 | apc500 | Ratio |
|--------|--------|--------|-------|
| Trace cells | 1.90B | 807M | **2.36x reduction** |
| Constraint instances | 1.13B | 491M | **2.29x reduction** |
| Bus interaction messages | 966M | 448M | **2.15x reduction** |
| **STARK (excl. trace)** | **2.38s** | **2.75s** | **0.87x (15% increase!)** |

### Keccak Guest: apc000 (baseline) vs apc003

| Metric | apc000 | apc003 | Ratio |
|--------|--------|--------|-------|
| Trace cells | 20.97B | 2.18B | **9.64x reduction** |
| Constraint instances | 17.38B | 703M | **24.71x reduction** |
| Bus interaction messages | 9.88B | 1.52B | **6.50x reduction** |
| **STARK (excl. trace)** | **21.25s** | **4.20s** | **5.06x reduction** |

### Keccak Guest: apc000 vs manual precompile

| Metric | apc000 | manual | Ratio |
|--------|--------|--------|-------|
| Trace cells | 20.97B | 2.37B | **8.85x reduction** |
| Constraint instances | 17.38B | 2.45B | **7.09x reduction** |
| Bus interaction messages | 9.88B | 315M | **31.41x reduction** |
| **STARK (excl. trace)** | **21.25s** | **2.17s** | **9.79x reduction** |

**Key observation confirmed**: Even though cells/constraints/interactions decrease with APCs, the STARK time does not decrease proportionally (and even increases in the Pairing case). The manual keccak precompile achieves much better time scaling than the APC-based approach.

## 2. What "STARK (excl. trace)" Actually Measures

The OpenVM 2 prover uses a GKR/LogUp-based proving system with WHIR polynomial commitments. "STARK (excl. trace)" (`stark_prove_excluding_trace_time_ms`) is the sum across all segments of the STARK prover time, excluding trace generation. It breaks down as:

```
STARK (excl. trace)
├── Constraints (prover.rap_constraints_time_ms)        ~50-65%
│   ├── LogUp GKR (logup_gkr_time_ms)                  ~25-40%
│   │   └── GKR input evaluation + fractional sumcheck
│   ├── Round 0 / PLE Round 0 (ple_round0_time_ms)     ~10-25%
│   │   └── Per-AIR univariate sumcheck polynomial
│   └── MLE Rounds (mle_rounds_time_ms)                ~5-8%
│       └── Multilinear sumcheck rounds
├── Openings (prover.openings_time_ms)                  ~20-30%
│   ├── WHIR (whir_time_ms)                             ~15-20%
│   │   └── Polynomial opening proofs via WHIR protocol
│   └── Stacked Reduction (stacked_reduction_time_ms)   ~5-15%
│       └── Batch opening reduction across all AIRs
├── Trace Commit (main_trace_commit_time_ms)            ~10-25%
│   └── Merkle tree commitment of all trace matrices
└── Other (residual)                                    ~2-5%
```

### 2.1 Round 0 (Univariate Sumcheck)

**What it does**: Evaluates the sumcheck polynomial for the first variable. For each AIR, it:
1. Transfers trace data to the GPU (H2D)
2. Launches a GPU kernel to evaluate constraint polynomials on a coset domain
3. Launches a GPU kernel to evaluate interaction (LogUp) expressions
4. Transfers results back (D2H)

**Key code location**: `cuda-backend/src/logup_zerocheck/mod.rs` lines 730-880

**Critical finding**: Round 0 is a **sequential per-AIR loop**. Each AIR requires:
- 1 H2D transfer (trace matrix pointers)
- 2 GPU kernel launches (zerocheck constraints + logup interactions)
- 2 D2H transfers (evaluation results)
- CPU-side DAG construction for interaction expressions

This is the most significant source of per-AIR overhead on the GPU.

### 2.2 MLE Rounds (Multilinear Sumcheck)

**What it does**: After the univariate Round 0, the prover runs `n_max` rounds of multilinear sumcheck, where `n_max = log2(max_trace_height) - l_skip` (typically 10-20).

Each round:
1. Partitions AIRs into "late" (already folded) and "early" (still active)
2. Evaluates sumcheck polynomials per-AIR, then batches them
3. Folds MLE evaluations

**Scaling**: O(n_max × num_AIRs × constraint_degree) for the batching step. But the GPU implementation batches AIRs together by type, so the per-AIR overhead is mostly in the CPU-side partitioning and coefficient accumulation.

### 2.3 LogUp GKR

**What it does**: Evaluates the GKR (Generalized Knowledge Reduction) input layer for the LogUp argument. For each AIR, it evaluates all interaction expressions at every row to produce the GKR input evaluations.

**Scaling**: O(Σ height_i × num_interactions_i) — proportional to bus interaction messages. The GPU version processes traces in parallel but has per-AIR setup costs.

### 2.4 WHIR Openings

**What it does**: Generates polynomial commitment opening proofs using the WHIR protocol. Operates on the "stacked" matrix — all AIR trace columns concatenated side-by-side.

**Scaling**: O(stacked_width × stacked_height), where:
- `stacked_width` = sum of all column widths across all AIR commitments
- `stacked_height` = 2^(l_skip + n_stack) = 2^(max log_height)

**Key insight**: WHIR does NOT have per-AIR loops. It operates on the stacked matrix as a whole. The cost scales with total columns, not number of AIRs.

### 2.5 Stacked Reduction

**What it does**: Reduces multiple polynomial commitment openings (one per AIR commitment) into a single opening via a batch sumcheck. This is done in `n_stack` rounds.

**Scaling**: O(n_stack × total_stacked_width) per round, but loops over "height windows" (groups of AIRs with the same trace height). More AIRs with different heights → more windows → more kernel launches.

### 2.6 Trace Commit

**What it does**: Commits all trace matrices into a single Merkle tree. This is a single operation on the stacked matrix.

**Scaling**: O(total_cells) — purely proportional to trace cells.

## 3. Where the Per-AIR Overhead Comes From

### 3.1 Quantifying the Per-AIR Overhead

From the Pairing guest data, comparing apc000 (20 AIRs, 100 AIR instances) to apc500 (520 AIRs, 745 AIR instances):

| Sub-component | apc000 | apc500 | Delta | Per extra AIR instance |
|---------------|--------|--------|-------|----------------------|
| Round 0 | 176ms | 710ms | **+534ms** | **~0.83ms** |
| MLE Rounds | 118ms | 197ms | **+79ms** | **~0.12ms** |
| Stacked Reduction | 116ms | 378ms | **+262ms** | **~0.41ms** |
| LogUp GKR | 977ms | 818ms | -159ms | (scales with BIM) |
| WHIR | 410ms | 193ms | -217ms | (scales with cells) |
| Trace Commit | 572ms | 452ms | -120ms | (scales with cells) |

**Total per-AIR-instance overhead: ~1.36ms**, dominated by:
1. **Round 0: ~0.83ms/AIR instance** — sequential GPU kernel launches + data transfer
2. **Stacked Reduction: ~0.41ms/AIR instance** — per-commitment work in the opening phase
3. **MLE Rounds: ~0.12ms/AIR instance** — CPU-side per-AIR batching

With 645 extra AIR instances (apc500 has 2 segments × 520 AIRs, minus 5 segments × 20 AIRs = 745 - 100 = 645), this amounts to **~875ms of per-AIR overhead**, explaining why the STARK time increases despite reducing cells by 2.4x.

### 3.2 Why APCs Create So Many AIRs

Each APC is a separate AIR. With 500 APCs across 2 segments, that's 1000 AIR instances. Additionally, each APC has:
- Average ~160 columns (vs ~40 for native OpenVM AIRs)
- Average ~75-105 constraints per AIR
- Average ~100-115 bus interactions per AIR

This is very different from OpenVM's typical usage pattern of ~20 AIRs with ~37 columns each.

### 3.3 Why the Keccak Manual Precompile Does Better

| Metric | apc003 | manual |
|--------|--------|--------|
| AIRs | 22 | 24 |
| AIR instances | 220 | 72 |
| Segments | 10 | 3 |
| Columns | 29K | 14.5K |
| BIM | 1.52B | 315M |
| STARK time | 4.20s | 2.17s |

The manual precompile achieves better STARK time because:
1. **Fewer segments** (3 vs 10) → less per-segment overhead
2. **Fewer AIR instances** (72 vs 220) → less per-AIR overhead
3. **Far fewer bus interaction messages** (315M vs 1.52B) → faster LogUp GKR
4. **Fewer columns** (14.5K vs 29K) → faster stacked operations
5. **Optimized bus interactions**: The manual precompile has only 3.2K bus interactions (vs 20.2K for apc003), meaning it communicates much more efficiently with the rest of the system

## 4. Scaling Analysis by Sub-Component

### 4.1 Cost Efficiency Degradation

The "cost per unit" increases dramatically with more APCs:

| Experiment | STARK/Mcell | STARK/M-constraint-inst | STARK/M-BIM |
|------------|-------------|------------------------|-------------|
| pairing_apc000 | 1.25ms | 2.12ms | 2.47ms |
| pairing_apc100 | 1.95ms | 3.12ms | 3.72ms |
| pairing_apc500 | 3.41ms | 5.61ms | 6.14ms |
| keccak_apc000 | 1.01ms | 1.22ms | 2.15ms |
| keccak_apc003 | 1.93ms | 5.97ms | 2.76ms |
| keccak_manual | 0.92ms | 0.88ms | 6.90ms |

The cost per unit work increases **2-3x** going from baseline to 500 APCs. This means the prover is increasingly dominated by overhead rather than useful computation.

### 4.2 Inherent vs Implementation Overhead

**Inherent to the proof system:**
- LogUp GKR scaling with interaction messages ✓ (expected)
- WHIR scaling with total cells ✓ (expected)
- Trace commit scaling with total cells ✓ (expected)
- MLE rounds scaling with `n_max × num_AIRs` (somewhat inherent — the sumcheck protocol must visit each AIR)

**Implementation overhead (can potentially be optimized):**
- **Round 0 sequential per-AIR GPU kernel launches**: This is the biggest bottleneck. Each AIR requires separate kernel launches because the constraint DAGs differ per AIR. In principle, AIRs with the same constraint structure could be batched.
- **Stacked Reduction per-commitment overhead**: Each AIR's preprocessed/cached trace creates a separate commitment that needs to be opened. This scales linearly with the number of commitments (≈ number of AIRs).
- **DAG construction per AIR in Round 0**: The interaction DAG (`SymbolicDagBuilder`) is rebuilt for each AIR in `evaluate_round0_interactions_gpu`. This involves CPU-side expression tree traversal and GPU memory allocation.

### 4.3 Per-Segment Breakdown

Looking at per-segment average STARK time reveals the overhead more clearly:

| Experiment | Segments | STARK/segment | Constraints/seg | Openings/seg |
|------------|----------|---------------|----------------|-------------|
| pairing_apc000 | 5 | 0.48s | 0.26s | 0.11s |
| pairing_apc100 | 3 | 0.77s | 0.46s | 0.16s |
| pairing_apc500 | 2 | 1.38s | 0.86s | 0.29s |
| keccak_apc000 | 48 | 0.44s | 0.22s | 0.11s |
| keccak_apc003 | 10 | 0.42s | 0.24s | 0.10s |
| keccak_manual | 3 | 0.72s | 0.36s | 0.13s |

For Pairing, the per-segment cost nearly **triples** from apc000 to apc500, even though cells per segment are much lower with APCs. This is because the per-AIR overhead is paid in every segment.

## 5. Cost Model

### 5.1 Simple Model: Cells Only

```
STARK_time = 0.000001 × cells + 1346ms
R² = 0.97
```

This model is reasonable for the keccak baseline but fails badly for APC experiments (errors up to 65%).

### 5.2 Best Fit Model

The best overall model (R² = 0.9995, all predictions within 12%):

```
STARK_time = 3.97e-6 × cells
           + 2.76e-2 × total_cols
           - 1.21 × n_air_instances
           - 2.56e-6 × constraint_instances
           - 1.68e-6 × bus_interaction_messages
           - 742ms
```

However, this model has some counter-intuitive negative coefficients for `n_air_instances` and `constraint_instances`, which indicates overfitting on 13 data points. The negative `n_air_instances` coefficient is likely compensating for the fact that more AIRs correlates with fewer segments, and fewer cells per AIR.

### 5.3 Practical Cost Model

Based on the analysis, a better conceptual model is:

```
STARK_time_per_segment ≈ base_cost
                        + α × cells_per_segment
                        + β × n_AIRs_per_segment
                        + γ × total_cols_per_segment

STARK_time = Σ_segments STARK_time_per_segment
```

Where:
- `base_cost` ≈ 50-100ms (per-segment fixed overhead)
- `α` ≈ 0.8-1.0 ms/Mcell (cell-proportional work: trace commit + WHIR + LogUp GKR)
- `β` ≈ 1.0-1.5 ms/AIR (per-AIR overhead: Round 0 kernel launches + stacked reduction)
- `γ` ≈ 0.002-0.005 ms/column (column-proportional: stacked matrix operations)

### 5.4 Sub-Component Models

Individual sub-components are better modeled:

| Component | Primary driver | Per-AIR cost | R² |
|-----------|---------------|-------------|-----|
| Round 0 | n_air_instances | ~2.5ms | 0.93 |
| Stacked Reduction | n_air_instances | ~1.6ms | 0.97 |
| MLE Rounds | n_air_instances | ~0.15ms | 0.96 |
| LogUp GKR | bus_interaction_messages | ~0.37ms | 0.97 |
| WHIR | total_cells + total_cols | (none) | 0.97 |
| Trace Commit | total_cells | (none) | 0.997 |

The per-AIR overhead is ~4.5ms per AIR instance in Round 0 + Stacked Reduction + MLE Rounds combined (from the sub-component model fit).

## 6. Recommendations

### 6.1 Optimizations in OpenVM's Implementation

1. **Batch Round 0 kernel launches**: The sequential per-AIR loop in Round 0 (logup_zerocheck/mod.rs:730-880) is the largest overhead source. AIRs with similar constraint structures could potentially be batched into a single GPU kernel launch. Even without structural batching, using CUDA streams to overlap AIR evaluations could help.

2. **Reduce per-AIR GPU memory allocations**: Each AIR in Round 0 allocates intermediate buffers (`DeviceBuffer`) and transfers data. Pre-allocating a pool and reusing buffers would reduce allocation overhead.

3. **Stacked Reduction optimization**: The stacked reduction creates per-AIR `StackedPcsData2` objects and loops over AIR commitments. With 500+ AIRs, this creates hundreds of small commitments. Merging AIR preprocessed traces into fewer commitments could reduce this overhead.

4. **Monomial kernel optimization for many small AIRs**: The code has a `DAG_FALLBACK_MONOMIAL_RATIO` heuristic that chooses between DAG and monomial evaluation. For many similar-sized APCs, a specialized kernel that handles multiple AIRs in one launch would be beneficial.

### 6.2 Optimizations in APC Design

1. **Reduce the number of APCs**: Each APC adds ~1-4.5ms of overhead per segment. With 500 APCs across 2 segments, that's 1-4.5 seconds of pure overhead. Fewer, larger APCs are preferable.

2. **Minimize bus interactions per APC**: APCs have ~100-115 bus interactions each. The LogUp GKR cost scales with total interaction messages. Reducing interactions (e.g., by merging memory reads) directly reduces proving time.

3. **Reduce APC column count**: APCs average ~160-180 columns. More columns increase the stacked matrix width, directly increasing WHIR and stacked reduction costs.

4. **Consider the number of segments**: More APCs → fewer segments (traces are smaller → fit in fewer segments). This is a beneficial effect that partially offsets the per-AIR overhead. The trade-off point depends on the workload.

### 6.3 Is This Inherent?

The per-AIR overhead is **partially inherent** to the proof system (each AIR needs its own constraint polynomial evaluation in the sumcheck protocol) but **significantly amplified by the GPU implementation** (sequential kernel launches, per-AIR memory transfers). A more optimized implementation could likely reduce the per-AIR overhead by 2-5x through batching and pipeline optimization, but it cannot be eliminated entirely.

The key insight is that OpenVM was designed for ~20 AIRs, not 500+. At that scale, the per-AIR overhead is negligible. As the number of AIRs grows, the overhead becomes the dominant cost, completely masking the cell/constraint reduction benefits.

## 7. Files in This Report

- `report.md` — This report
- `scripts/analyze_metrics.py` — Confirms observations, computes statistics from raw metrics
- `scripts/cost_model.py` — Fits cost models, analyzes per-AIR overhead
- `analysis_output.txt` — Full output of analyze_metrics.py
- `cost_model_output.txt` — Full output of cost_model.py
- `data/pairing_metrics.json` — Pairing benchmark metrics
- `data/keccak_metrics.json` — Keccak benchmark metrics
- `data/pairing_candidates.json` — Pairing APC candidates
- `data/keccak_candidates.json` — Keccak APC candidates

## Appendix A: OpenVM 2 Prover Pipeline Pseudo-Code

```python
def prove(multi_pk, proving_context):
    # 1. Main trace commitment (single Merkle tree for all AIRs)
    commitment, pcs_data = commit(all_traces)  # O(total_cells)

    # 2. Emit trace metrics
    for air_id, trace_ctx in per_trace:         # O(num_AIRs)
        emit_metrics(air_id, trace_ctx)

    # 3. Transcript observations
    for air_id, trace_ctx in per_trace:         # O(num_AIRs)
        transcript.observe(commitments, public_values)

    # 4. Constraint proving (largest step)
    proof, r = prove_rap_constraints(transcript, pk, ctx, pcs_data)

    # 5. Opening proofs
    opening = prove_openings(transcript, pk, ctx, pcs_data, r)

    return Proof(proof, opening)


def prove_rap_constraints(transcript, pk, ctx, pcs_data):
    # Setup
    alpha, beta = transcript.sample(2)
    prover = LogupZerocheckGpu(pk, ctx, alpha, beta)

    # GKR input layer: evaluate interactions for all AIRs
    for trace_idx, (air_idx, trace) in enumerate(ctx.per_trace):  # O(num_AIRs)
        evals = eval_interactions(trace, pk.per_air[air_idx])     # O(height × num_interactions)
        stack_evaluations(evals)

    # Fractional sumcheck (GKR)
    gkr_proof = fractional_sumcheck(transcript, stacked_evals)

    # Round 0: univariate sumcheck (PER-AIR SEQUENTIAL LOOP)
    lambda_ = transcript.sample()
    for trace_idx, (air_idx, air_ctx) in enumerate(ctx.per_trace):  # O(num_AIRs) SEQUENTIAL
        d_main = main_traces.to_device()                             # H2D transfer
        sp_zc = evaluate_round0_constraints_gpu(pk, ...)             # GPU kernel
        sp_logup = evaluate_round0_interactions_gpu(pk, ...)         # GPU kernel
        zc_results = sp_zc.to_host()                                 # D2H transfer
        logup_results = sp_logup.to_host()                           # D2H transfer

    # MLE rounds: multilinear sumcheck
    for round in 1..=n_max:                                          # O(n_max) rounds
        for trace_idx in 0..num_traces:                              # O(num_AIRs) per round
            if round > n_lift[trace_idx]:
                late_batch.add(trace_idx)
            else:
                early_batch.add(trace_idx)
        evaluate_batched(late_batch)                                 # 1-2 GPU kernels
        evaluate_batched(early_batch)                                # 1-2 GPU kernels

    return (gkr_proof, batch_proof, r)


def prove_openings(transcript, pk, ctx, pcs_data, r):
    # Collect per-AIR commitment data
    per_commit = [pcs_data]                                          # common main
    for air_idx, air_ctx in ctx.per_trace:                          # O(num_AIRs)
        if pk.per_air[air_idx].preprocessed:
            per_commit.append(preprocessed_data)                     # +1 per preprocessed AIR
        for cached in air_ctx.cached_mains:
            per_commit.append(cached_data)                           # +1 per cached main

    # Stacked reduction: n_stack rounds
    for round in 1..=n_stack:                                        # O(n_stack) rounds
        for height_window in windows:                                # O(num_distinct_heights)
            gpu_kernel(round, window)                                # kernel launch per window

    # WHIR opening proof
    whir_proof = prove_whir(stacked_matrices, u_cube)               # O(total_width × height)

    return (stacking_proof, whir_proof)
```

## Appendix B: Key Source Files

| Component | Repository | Path |
|-----------|-----------|------|
| GPU prover entry | stark-backend (v2-powdr) | `crates/cuda-backend/src/gpu_backend.rs` |
| GPU logup zerocheck | stark-backend (v2-powdr) | `crates/cuda-backend/src/logup_zerocheck/mod.rs` |
| GPU Round 0 kernels | stark-backend (v2-powdr) | `crates/cuda-backend/src/logup_zerocheck/round0.rs` |
| GPU stacked reduction | stark-backend (v2-powdr) | `crates/cuda-backend/src/stacked_reduction.rs` |
| CPU prover (reference) | stark-backend (v2-powdr) | `crates/stark-backend/src/prover/mod.rs` |
| CPU logup zerocheck | stark-backend (v2-powdr) | `crates/stark-backend/src/prover/logup_zerocheck/mod.rs` |
| Stacked PCS layout | stark-backend (v2-powdr) | `crates/stark-backend/src/prover/stacked_pcs.rs` |
| Metrics specification | powdr (main) | `openvm/metrics-viewer/spec.py` |
