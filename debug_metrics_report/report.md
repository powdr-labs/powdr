# Metrics Discrepancy Report: Metrics Viewer vs APC Analyzer

## Executive Summary

The discrepancies between the OpenVM Metrics Viewer and the APC Analyzer numbers stem from **two root causes** and **two expected differences**:

| Finding | Type | Impact |
|---------|------|--------|
| `air_id` collision bug in metrics viewer | **Bug** | Inflates constraint instances by **5.45x**, bus messages by **1.50x** |
| Interaction columns counted in `cells` but not `main_cols` | Expected | Inflates trace cells by **3.07x** vs APC analyzer |
| Power-of-2 padding per segment | Expected | Inflates all metrics by **~1.20x** |
| Non-instruction AIRs | Expected | Adds **~1.3%** to trace cells |

### Numbers at a Glance

| Metric | APC Analyzer | Viewer (buggy) | Viewer (corrected) | Ratio (corrected/APC) |
|--------|-------------|----------------|---------------------|-----------------------|
| Trace cells | 1.54B | 5.77B | 5.77B (no bug here) | 3.74x |
| Constraint instances | 0.90B | **6.14B** | **1.13B** | 1.26x |
| Bus interaction msgs | 0.80B | **1.44B** | **0.97B** | 1.21x |

---

## Bug 1: `air_id` Collision in Metrics Viewer (constraint instances & bus messages)

### What Happens

The metrics viewer computes constraint instances as:

```python
constraint_instances = sum(constraints[air_id] * rows[air_id])  # for each air_id
```

The problem: **`air_id` is not globally unique**. It's just the index of the AIR within a proving phase's AIR set. Each proving phase (app, leaf, compression, internal) has its own set of AIRs numbered from 0. For example:

| `air_id` | App Phase AIR | Leaf Phase AIR |
|----------|--------------|----------------|
| 17 | LessThanCoreAir (constraints=28) | Poseidon2Air (constraints=282) |
| 18 | BaseAluCoreAir (constraints=22) | MerkleVerifyAir (constraints=23) |
| 13 | BranchEqualCoreAir (constraints=11) | InteractionsFoldingAir (constraints=73) |

The `constraints` and `interactions` metrics are emitted during keygen for **every proving phase**, but the viewer's weighted sum iterates over **all entries** while weighting by **app-only rows**:

```javascript
// index.html weighted sum — iterates all_entries, not just app entries
function weightedSum(metricName, weights) {
    return allEntries.filter(e => e.metric === metricName)
        .reduce((sum, e) => sum + parseFloat(e.value) * (weights[e.air_id] || 0), 0);
}
```

This means the leaf phase's Poseidon2Air constraints (282, at air_id=17) get multiplied by the app phase's LessThanCoreAir rows (9.4M, also at air_id=17), producing a massive inflation.

### Impact

Every single app AIR has a colliding `air_id` in the leaf/internal/compression phases:

| app AIR (air_id) | Colliding AIR | Constraint inflation |
|-------------------|---------------|---------------------|
| LessThanCoreAir (17) | Poseidon2Air | **11.07x** |
| MultiplicationCoreAir (6) | MultilinearSumcheckAir | **16.00x** |
| RangeTupleCheckerAir (7) | EqNsAir | **9.12x** |
| BaseAluCoreAir (18) | MerkleVerifyAir | 2.05x |
| BranchEqualCoreAir (13) | InteractionsFoldingAir | 7.64x |

Overall: constraint instances inflated by **5.45x**, bus interaction messages by **1.50x**.

### Root Cause in OpenVM

The `constraints` and `interactions` metrics are emitted during keygen in `stark-backend/src/keygen/mod.rs`:

```rust
let labels = [("air_name", pk.air_name.clone())];
metrics::counter!("constraints", &labels).absolute(...);
metrics::counter!("interactions", &labels).absolute(...);
```

The `air_id` is added via `metrics_tracing_context` from the enclosing tracing span. Since each proving phase does its own keygen, the same `air_id` values get reused.

### Fix

The viewer should join on `(air_id, air_name)` instead of just `air_id`:

```javascript
// Use composite key: air_id + air_name
const key = `${e.air_id}:${e.air_name}`;
```

The same fix applies to `spec.py`.

---

## Discrepancy 2: Interaction Columns in `cells` (trace cells)

### What Happens

In OpenVM V2, the `cells` metric includes **interaction columns** that are NOT reported in `main_cols`, `prep_cols`, or `perm_cols`:

```
cells = rows * (main_cols + 4 * num_interactions)
```

The factor of **4** is the **challenge extension degree** (`CHALLENGE_EXT_DEGREE = 4` for BabyBear's quartic extension field).

### Verification

This holds exactly for every single AIR in the data:

| AIR | main_cols | interactions | cells/rows | main + 4*inter |
|-----|-----------|-------------|------------|----------------|
| BaseAluCoreAir | 36 | 20 | 116 | 116 |
| LoadStoreCoreAir | 41 | 17 | 109 | 109 |
| LessThanCoreAir | 37 | 18 | 109 | 109 |
| MulHCoreAir | 39 | 24 | 135 | 135 |
| MultiplicationCoreAir | 31 | 19 | 107 | 107 |
| BranchEqualCoreAir | 26 | 11 | 70 | 70 |
| ProgramAir | 10 | 1 | 14 | 14 |
| Poseidon2PeripheryAir | 300 | 1 | 304 | 304 |

100% of AIRs match the formula `cells = rows * (main_cols + 4 * interactions)`.

### Source Code Confirmation

In `stark-backend/src/prover/metrics.rs` (lines 103-117):

```rust
let mut interaction_width = pk.vk.num_interactions();
let ext_degree = PB::CHALLENGE_EXT_DEGREE as usize;  // = 4
interaction_width *= ext_degree;

let cells = TraceCells {
    cached_mains: width.cached_mains.iter().map(|w| w * height).collect(),
    common_main: width.common_main * height,
    after_challenge: vec![interaction_width * height],  // <-- 4 * interactions * rows
};

let total_cells = cells.cached_mains.iter()
    .chain([&cells.common_main])
    .chain(cells.after_challenge.iter())
    .sum::<usize>();  // = main_cols*rows + 4*interactions*rows
```

Meanwhile, the emitted `perm_cols` metric uses `width.after_challenge` from the `TraceWidth` struct, which is **0 in V2** (the after-challenge trace width is handled differently in the GKR/LogUp protocol):

```rust
counter!("perm_cols", &labels)
    .absolute(self.width.after_challenge.iter().sum::<usize>() as u64);  // = 0 in V2
```

### Impact on APC Analyzer

The APC analyzer reports "software cost" as `main_columns * execution_frequency`. This counts only main trace columns and misses the 4 interaction columns per bus interaction. Since bus interactions are a significant component (avg ~18 per instruction), the actual trace cells are **3.07x** larger than `main_columns * frequency` would suggest.

### Possible Actions

1. **Metrics viewer**: The "Columns" metric (sum of main+prep+perm) underreports the actual column count. Consider showing the effective column count including interaction columns, or show them separately.

2. **APC analyzer**: Consider using `main_columns + 4 * bus_interactions` as the cost metric instead of just `main_columns`. This would more accurately reflect the actual trace area cost.

---

## Discrepancy 3: Power-of-2 Padding (~1.20x)

### What Happens

Each AIR's row count is padded to the next power of 2 per segment. With 5 segments in this benchmark:

| AIR | Actual instructions | Padded rows (5 segments) | Padding factor |
|-----|-------------------|--------------------------|----------------|
| BaseAluCoreAir | ~17.1M (ADD+SUB) | 18.9M | 1.10x |
| LoadStoreCoreAir | ~7.3M (LOADW+STOREW) | 9.4M | 1.29x |
| LessThanCoreAir | ~7.1M (SLTU) | 9.4M | 1.33x |
| MulHCoreAir | ~3.1M (MULHU) | 4.5M | 1.46x |
| MultiplicationCoreAir | ~3.2M (MUL) | 4.5M | 1.40x |

Overall: **1.20x** padding factor across all instruction AIRs.

This is expected and moderate. The padding overhead per segment depends on how evenly the instructions are distributed across segments.

---

## Discrepancy 4: Non-Instruction AIRs (~1.3%)

The APC analyzer only accounts for instruction AIRs. System AIRs (ProgramAir, RangeTupleCheckerAir, MemoryMerkleAir, etc.) contribute an additional 1.3% of total trace cells. This is negligible.

---

## Complete Decomposition

### Trace Cells: 3.74x ratio

```
APC software cells (main_cols only):           1.54B  (1.00x)
  + interaction columns (4 per bus interaction) → 4.73B  (3.07x)
  + power-of-2 padding per segment              → 5.69B  (3.70x)
  + non-instruction AIRs                        → 5.77B  (3.74x)
```

### Constraint Instances: 6.84x ratio (buggy) → 1.26x (corrected)

```
APC constraint instances:                      0.90B  (1.00x)
  + power-of-2 padding + minor differences     → 1.13B  (1.26x)
  + air_id collision BUG                        → 6.14B  (6.84x)
```

### Bus Interaction Messages: 1.81x ratio (buggy) → 1.21x (corrected)

```
APC bus interaction messages:                  0.80B  (1.00x)
  + power-of-2 padding + minor differences     → 0.97B  (1.21x)
  + air_id collision BUG                        → 1.44B  (1.81x)
```

---

## Reproducing These Results

All analysis scripts are in `debug_metrics_report/`:

```bash
cd debug_metrics_report

# Step 1: Validate observations (confirm viewer/analyzer numbers)
python3 step1_validate_observations.py

# Step 2: Deep analysis (cells/rows vs main_cols, padding)
python3 step2_deep_analysis.py

# Step 3: air_id collision bug analysis
python3 step3_airid_collision.py

# Step 4: Complete decomposition including interaction columns
python3 step4_cells_mystery.py
```

Data files:
- `metrics_v2_pairing_combined.json` — [Gist link](https://gist.githubusercontent.com/leonardoalt/3074cb729c03470b1116674618b97267/raw/eec5e5a086bf07a57e2215843f0a3f1ada9d0d5c/metrics_v2_pairing_combined.json)
- `apc_candidates_pairing_v2.json` — [Gist link](https://gist.githubusercontent.com/leonardoalt/4e7f5a1e81048df77d4a92cb01636a4a/raw/562784471ff6f11082af0f0e096fb9dd61920a1b/apc_candidates_pairing_v2.json)
