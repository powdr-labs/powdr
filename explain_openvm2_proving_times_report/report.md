# OpenVM 2 STARK Proving Time Analysis

This report analyzes why the "STARK (excl. trace)" proving time doesn't scale linearly with trace cells, constraint instances, and bus interaction messages when using autoprecompiles (APCs). The analysis is based on benchmarks of the Pairing and Keccak guests with varying numbers of APCs, run on a GPU server using the `openvm-v2-integration` branch.

## 1. Confirming the Observations

The observations from the problem statement are confirmed by the data. See `analysis_output.txt` and `cost_model_output.txt` for full numbers.

### Pairing Guest: apc000 (baseline) vs apc500

| Metric                   | apc000    | apc500    | Ratio                     |
| ------------------------ | --------- | --------- | ------------------------- |
| Trace cells              | 1.90B     | 807M      | **2.36x reduction**       |
| Constraint instances     | 1.13B     | 491M      | **2.29x reduction**       |
| Bus interaction messages | 966M      | 448M      | **2.15x reduction**       |
| **STARK (excl. trace)**  | **2.38s** | **2.75s** | **0.87x (15% increase!)** |

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
├── Constraints (prover.rap_constraints_time_ms)        ~49-63%
│   ├── LogUp GKR (logup_gkr_time_ms)                  ~25-40%
│   │   └── GKR input evaluation + fractional sumcheck
│   ├── Round 0 / PLE Round 0 (ple_round0_time_ms)     ~6-26%
│   │   └── Per-AIR univariate sumcheck polynomial
│   └── MLE Rounds (mle_rounds_time_ms)                ~5-8%
│       └── Multilinear sumcheck rounds
├── Openings (prover.openings_time_ms)                  ~19-30%
│   ├── WHIR (whir_time_ms)                             ~7-19%
│   │   └── Polynomial opening proofs via WHIR protocol
│   └── Stacked Reduction (stacked_reduction_time_ms)   ~3-14%
│       └── Batch opening reduction across all AIRs
├── Trace Commit (main_trace_commit_time_ms)            ~10-27%
│   └── Merkle tree commitment of all trace matrices
└── Other (residual)                                    ~1-5%
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

**What it does**: After the univariate Round 0, the prover runs $n_{\max}$ rounds of multilinear sumcheck, where $n_{\max} = \log_2(\text{max\_trace\_height}) - l_{\text{skip}}$ (typically 10–20).

Each round:
1. Partitions AIRs into "late" (already folded) and "early" (still active)
2. Evaluates sumcheck polynomials per-AIR, then batches them
3. Folds MLE evaluations

**Scaling**: $O(n_{\max} \times n_{\text{AIRs}} \times d)$ for the batching step (where $d$ is constraint degree). But the GPU implementation batches AIRs together by type, so the per-AIR overhead is mostly in the CPU-side partitioning and coefficient accumulation.

### 2.3 LogUp GKR

**What it does**: Evaluates the GKR (Goldwasser-Kalai-Rothblum) input layer for the LogUp argument. For each AIR, it evaluates all interaction expressions at every row to produce the GKR input evaluations.

**Scaling**: $O\!\left(\sum_i h_i \cdot k_i\right)$ where $h_i$ is the trace height and $k_i$ the number of interactions for AIR $i$ — proportional to bus interaction messages. The GPU version processes traces in parallel but has per-AIR setup costs.

### 2.4 WHIR Openings

**What it does**: Generates polynomial commitment opening proofs using the WHIR protocol. Operates on the "stacked" matrix — all AIR trace columns concatenated side-by-side.

**Scaling**: $O(W \times H)$, where:
- $W = \sum_i w_i$ (sum of all column widths across all AIR commitments)
- $H = 2^{l_{\text{skip}} + n_{\text{stack}}} = 2^{\max(\log h_i)}$

**Key insight**: WHIR does NOT have per-AIR loops. It operates on the stacked matrix as a whole. The cost scales with total columns, not number of AIRs.

### 2.5 Stacked Reduction

**What it does**: Reduces multiple polynomial commitment openings (one per AIR commitment) into a single opening via a batch sumcheck. This is done in $n_{\text{stack}}$ rounds.

**Scaling**: $O(n_{\text{stack}} \times W)$ per round, but loops over "height windows" (groups of AIRs with the same trace height). More AIRs with different heights → more windows → more kernel launches.

### 2.6 Trace Commit

**What it does**: Commits all trace matrices into a single Merkle tree. This is a single operation on the stacked matrix.

**Scaling**: $O(\text{total\_cells})$ — purely proportional to trace cells.

## 3. Why STARK Time Doesn't Scale With Cells

### 3.1 Component Breakdown: What Actually Dominates

STARK time is a sum of sub-components with different scaling behaviors. The table below shows what fraction of STARK time each group accounts for:

| Experiment | R0+SR+MLE | GKR+WHIR+TC | STARK |
|---|---|---|---|
| pairing_apc000 | **17%** | **82%** | 2384ms |
| pairing_apc500 | **47%** | **53%** | 2753ms |
| keccak_apc000 | **22%** | **78%** | 21251ms |
| keccak_apc003 | **18%** | **82%** | 4197ms |
| keccak_manual | **39%** | **61%** | 2170ms |

For most experiments, **cell-proportional components (GKR, WHIR, Trace Commit) account for 53–82% of STARK time**. The components that process AIR instances sequentially (Round 0, Stacked Reduction, MLE Rounds) are 17–47%.

The exception is pairing_apc500, where the balance shifts toward per-AIR components due to 744 AIR instances.

### 3.2 The Confounding Problem

The metrics are highly correlated: more APCs means simultaneously more AIR instances, more columns, fewer segments, fewer cells, and fewer constraint instances. Within the Pairing series:

| Predictor pair | Pearson r |
|---|---|
| n_air_instances vs total_cols | 0.996 |
| n_air_instances vs cells | −0.975 |
| CI vs cells | 0.999 |

This makes it impossible to determine from the Pairing series alone whether Round 0 scales with AIR instances or with columns — they predict R0 equally well ($r^2 = 0.986$ vs $0.998$).

Cross-guest comparisons break this degeneracy. Univariate $r^2$ for Round 0 across all 13 experiments:

| Predictor | $r^2$ with R0 |
|---|---|
| CI (constraint instances) | **0.868** |
| cells | 0.843 |
| BIM | 0.799 |
| segments | 0.776 |
| n_air_instances | 0.643 |
| total_cols | 0.066 |

**Constraint instances are the best single predictor of Round 0 time**, not AIR instance count. This makes sense from the code: Round 0 evaluates constraint polynomials for each AIR instance, and the GPU kernel time scales with $h_i \times c_i$ (trace height × constraints) for each instance $i$.

### 3.3 Round 0: Both Fixed and Variable Cost Per AIR Instance

A two-feature regression across all experiments gives ($R^2 = 0.987$):

$$T_{\text{R0}} \approx 0.84 \;\text{ms} \times n_{\text{air\_inst}} + 0.08 \;\text{ms} \times \text{MCI} + 43 \;\text{ms}$$

This decomposes into:
- **Fixed per-instance overhead** (~0.84ms): kernel launch, DAG construction, memory transfer
- **Variable per-instance compute** (~0.08ms per million CI): GPU kernel time for constraint evaluation

The balance between these components shifts dramatically:

| Experiment | AI | Fixed (0.84 × AI) | Variable (0.08 × CI) | Fixed % of R0 |
|---|---|---|---|---|
| pairing_apc500 | 744 | 625ms | 39ms | **94%** |
| keccak_apc003 | 202 | 170ms | 56ms | **75%** |
| keccak_apc000 | 866 | 728ms | 1391ms | **34%** |
| keccak_manual | 66 | 55ms | 196ms | **22%** |

For many-small-APC experiments (pairing_apc500), fixed overhead dominates. For experiments with heavy AIR instances (keccak_manual: 37M CI per instance), variable GPU compute dominates.

### 3.4 Pairing: Per-AIR Overhead Negates Cell Reduction

From the Pairing guest data, comparing apc000 (99 AIR instances) to apc500 (744 AIR instances):

| Sub-component | apc000 | apc500 | Delta | Scaling driver |
|---|---|---|---|---|
| Round 0 | 178ms | 712ms | **+534ms** | per-AIR (sequential kernels) |
| Stacked Reduction | 115ms | 377ms | **+262ms** | per-AIR (per-commitment) |
| MLE Rounds | 118ms | 197ms | **+79ms** | per-AIR (CPU batching) |
| LogUp GKR | 981ms | 817ms | −164ms | cells & BIM |
| WHIR | 409ms | 192ms | −217ms | cells & columns |
| Trace Commit | 568ms | 452ms | −116ms | cells |

The +875ms from AIR-scaling components overwhelms the −497ms savings from cell-proportional components, producing a net **+369ms increase**. This is the only case in our data where STARK time actually *increases* despite reduced cells.

### 3.5 Keccak: Bus Interactions Are the Main Driver

The keccak_manual vs keccak_apc003 comparison is the most instructive:

| Metric | apc003 | manual | Ratio |
|---|---|---|---|
| AIRs | 22 | 24 | 0.9x |
| AIR instances | 202 | 66 | **3.1x fewer** |
| Segments | 10 | 3 | **3.3x fewer** |
| BIM | 1.52B | 315M | **4.8x fewer** |
| Bus interactions | 20.2K | 3.2K | **6.3x fewer** |
| Columns | 29K | 14.5K | **2.0x fewer** |
| CI | 703M | 2.45B | **3.5x more** |
| Cells | 2.18B | 2.37B | ~same |

Despite having 3x fewer AIR instances, the manual precompile's winning margin comes from sub-components:

| Sub-component | apc003 | manual | Ratio | Primary driver |
|---|---|---|---|---|
| **LogUp GKR** | **1797ms** | **381ms** | **4.7x** | **BIM (4.8x fewer)** |
| **WHIR** | **813ms** | **254ms** | **3.2x** | **cols × segments** |
| Trace Commit | 825ms | 690ms | 1.2x | cells (~same) |
| Round 0 | 263ms | 444ms | **0.6x (worse!)** | CI (3.5x more) |
| Stacked Reduction | 176ms | 126ms | 1.4x | fewer AIR instances |
| MLE Rounds | 296ms | 267ms | 1.1x | ~same |

The manual precompile is faster despite having **worse** Round 0 time (444ms vs 263ms) because its R0 AIR instances are much heavier (37M CI each vs 3.5M). The real wins are:

1. **LogUp GKR: −1416ms** — from 4.8x fewer bus interaction messages
2. **WHIR: −559ms** — from 2x fewer columns and 3.3x fewer segments
3. Round 0: +181ms (penalty from heavier per-instance work)

**Bus interaction messages, not AIR instance count, are the dominant driver of the keccak manual precompile's advantage.**

### 3.6 What APCs Change About the Workload

Each APC is a separate AIR with:
- Average ~159–182 columns (vs ~40 for native OpenVM AIRs)
- Average ~75–105 constraints per AIR
- Average ~102–114 bus interactions per AIR

This creates a workload profile very different from OpenVM's typical ~20 AIRs with ~40 columns:
- Many more AIR instances per segment → Round 0 and Stacked Reduction overhead
- Many more total columns → wider stacked matrix → WHIR and Stacked Reduction cost
- Many bus interactions per APC → high BIM → LogUp GKR cost

## 4. Cost Model

### 4.1 Simple Model: Cells Only

$$T_{\text{STARK}} = 0.001 \cdot \text{cells} + 1346 \;\text{ms} \quad (R^2 = 0.97)$$

This model is reasonable for the keccak baseline but fails badly for APC experiments (errors up to 65%).

### 4.2 Best Fit Model

The best overall model ($R^2 = 0.9995$, all predictions within 12%):

$$T_{\text{STARK}} = 3.97 \cdot \text{Mcells} + 27.6 \cdot \text{total\_cols} - 1.21 \cdot n_{\text{air\_inst}} - 2.56 \cdot \text{MCI} - 1.68 \cdot \text{MBIM} - 742 \;\text{ms}$$

where Mcells, MCI, MBIM are in millions. However, this model has counter-intuitive negative coefficients for $n_{\text{air\_inst}}$ and constraint instances, which indicates overfitting on 13 data points. The negative $n_{\text{air\_inst}}$ coefficient likely compensates for the fact that more AIRs correlates with fewer segments and fewer cells per AIR.

### 4.3 Sub-Component Models

Individual sub-components are better modeled:

| Component | Primary driver | R² | Notes |
|-----------|---------------|-----|-------|
| Round 0 | n_air_instances + CI | 0.987 | ~0.84ms fixed/AI + 0.08ms/M-CI |
| Stacked Reduction | n_air_instances | 0.97 | per-commitment work |
| MLE Rounds | n_air_instances | 0.96 | CPU-side batching |
| LogUp GKR | bus_interaction_messages | 0.97 | dominates STARK in most cases |
| WHIR | total_cells + total_cols | 0.97 | stacked matrix operations |
| Trace Commit | total_cells | 0.997 | nearly perfect linear scaling |

### 4.4 Per-Segment Breakdown

| Experiment | Segments | STARK/seg | Constraints/seg | Openings/seg |
|------------|----------|-----------|----------------|-------------|
| pairing_apc000 | 5 | 0.48s | 0.26s | 0.11s |
| pairing_apc100 | 3 | 0.77s | 0.46s | 0.16s |
| pairing_apc500 | 2 | 1.38s | 0.86s | 0.29s |
| keccak_apc000 | 48 | 0.44s | 0.22s | 0.11s |
| keccak_apc003 | 10 | 0.42s | 0.24s | 0.10s |
| keccak_manual | 3 | 0.72s | 0.36s | 0.13s |

For Pairing, the per-segment cost nearly **triples** from apc000 to apc500, even though cells per segment are much lower with APCs. The keccak manual precompile has a higher per-segment cost than apc003 (0.72s vs 0.42s) but wins on total because it needs only 3 segments vs 10.

### 4.5 Cost Efficiency

| Experiment | STARK/Mcell | STARK/M-CI | STARK/M-BIM |
|------------|-------------|------------|-------------|
| pairing_apc000 | 1.25ms | 2.12ms | 2.47ms |
| pairing_apc100 | 1.95ms | 3.12ms | 3.72ms |
| pairing_apc500 | 3.41ms | 5.61ms | 6.14ms |
| keccak_apc000 | 1.01ms | 1.22ms | 2.15ms |
| keccak_apc003 | 1.93ms | 5.97ms | 2.76ms |
| keccak_manual | 0.92ms | 0.88ms | 6.90ms |

Note the contrasting patterns: the manual precompile has the best STARK/Mcell and STARK/M-CI ratios, but the worst STARK/M-BIM — because it achieves low BIM at the cost of higher CI per cell.

## 5. Recommendations

### 5.1 Optimizations in APC Design (highest impact)

1. **Reduce bus interactions per APC**: This is the highest-impact optimization. APCs have ~100–115 bus interactions each. LogUp GKR — the largest STARK component at 30–40% of time — scales with total bus interaction messages ($\text{BIM} = \sum h_i \times k_i$). The keccak manual precompile achieves 6.3x fewer bus interactions than apc003, which translates to 4.7x less GKR time. Reducing per-APC bus interactions (e.g., by batching memory reads, merging interactions) is the single most effective lever.

2. **Reduce APC column count**: APCs average ~160–180 columns. WHIR operates on the stacked matrix (width = total columns, height = max trace height), and more columns directly increase WHIR and Stacked Reduction costs. Fewer columns per APC also reduce the stacked matrix width.

3. **Reduce the number of APCs (for the Pairing case)**: The pairing_apc500 case is the only experiment where per-AIR overhead actually *increases* total STARK time. The fixed per-AIR-instance overhead (~0.84ms for Round 0 kernel launch + DAG + transfer, plus Stacked Reduction) accumulates to ~36% of STARK with 744 instances. Fewer, larger APCs would help, but this is secondary to reducing BIM and columns.

4. **Consider the segment trade-off**: More APCs → fewer segments (smaller traces fit in fewer segments). The keccak manual precompile's advantage partly comes from needing only 3 segments vs 10 for apc003. Each segment pays per-AIR costs for all its active AIRs, so reducing segments has a multiplicative effect.

### 5.2 Optimizations in OpenVM's Implementation

1. **Batch Round 0 kernel launches**: The sequential per-AIR loop in Round 0 (logup_zerocheck/mod.rs:730-880) processes each AIR instance with separate GPU kernel launches. For many small AIR instances, the fixed overhead (~0.84ms) dominates over the actual GPU compute. Batching AIRs with similar structure into a single kernel, or overlapping launches with CUDA streams, would improve GPU utilization.

2. **Reduce per-AIR GPU memory allocations**: Each AIR in Round 0 allocates intermediate buffers (`DeviceBuffer`) and transfers data. Pre-allocating a pool and reusing buffers would reduce allocation overhead.

3. **Stacked Reduction optimization**: Each AIR's preprocessed/cached trace creates a separate commitment. With 500+ AIRs, this creates hundreds of small commitments. Merging preprocessed traces into fewer commitments could reduce this overhead.

### 5.3 What Is Inherent vs. Fixable?

**Inherent to the proof system:**
- LogUp GKR scaling with bus interaction messages (the dominant cost)
- WHIR scaling with stacked matrix dimensions
- Trace commit scaling with total cells
- Each AIR needing its own constraint evaluation in the sumcheck

**Implementation overhead (potentially fixable):**
- Round 0 sequential kernel launches (~0.84ms fixed cost per AIR instance)
- DAG reconstruction per AIR in `evaluate_round0_interactions_gpu`
- Per-AIR memory allocation/transfer in Round 0
- Per-commitment overhead in Stacked Reduction

The fixed per-AIR overhead is significant only when there are hundreds of lightweight AIR instances (pairing_apc500). For most workloads, the cell-proportional components dominate and the per-AIR overhead is 4–7% of STARK time. The most impactful improvements come from reducing the **inherent** costs: fewer bus interactions, fewer columns, and fewer segments.

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
