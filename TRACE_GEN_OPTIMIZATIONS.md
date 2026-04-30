# Trace Gen Bus Replay Optimizations

## Results Summary

```
                       Baseline    O1       O1+O2     Speedup
trace_gen              58,459ms   41,382ms  17,413ms   3.36x
powdr_bus_replay       36,676ms   27,000ms   3,255ms  11.27x
powdr_fill_and_replay  39,310ms   28,937ms   5,069ms   7.75x
powdr_dummy_traces      9,833ms    8,001ms   8,038ms   1.22x
```

O1 (BTreeMap→Vec): **1.41x** overall
O2 (LinearCombination): **3.36x** overall (cumulative)
O5 (Parallel replay): **4.45x** overall (cumulative)

Post-O1+O2+O5 breakdown:
```
trace_gen                        13,144ms  100%
  powdr_dummy_traces              8,043ms   61.2%  <<<< new bottleneck
  powdr_fill_rows_and_replay      1,931ms   14.7%
  powdr_generate_trace_data         ~500ms    3.8%
  memory_tracegen                   ~380ms    2.9%
  other                           ~2,290ms   17.4%
```

## Baseline (keccak 10K hashes, APC=30, mock prove)

```
trace_gen                                 58,459ms  100%
  powdr_dummy_traces                       9,833ms   16.8%
  powdr_generate_trace_data                  811ms    1.4%
  powdr_fill_rows_and_replay              39,310ms   67.2%
    powdr_copy_dummy_rows                  2,348ms    4.0%
    powdr_derived_columns                     91ms    0.2%
    powdr_bus_replay (eval+apply)         36,676ms   62.7%  <<<< BOTTLENECK
  system chips (memory/merkle)              ~400ms    0.7%

240K rows x 1,734 bus interactions = 416M expression evals
Each: recursive AST walk + BTreeMap lookup per variable
88ns per bus interaction evaluation
```

## Optimizations (descending expected impact)

### O1: BTreeMap -> Vec for variable lookup
- **Target sub-metric**: powdr_bus_replay_time_ms
- **Expected**: 10-20% reduction on bus_replay
- **What**: `MappingRowEvaluator` uses `BTreeMap<u64, usize>` for every `eval_var` call.
  ~2B lookups each doing ~11 comparisons. Replace with dense `Vec<usize>`.
- **Impl**: Build `Vec<usize>` indexed by poly_id at init. Replace BTreeMap get with Vec index.

### O2: Compile expressions to LinearCombination
- **Target sub-metric**: powdr_bus_replay_time_ms
- **Expected**: 2-3x reduction on bus_replay
- **What**: Replace recursive AST walk (`AlgebraicExpression::to_expression`) with pre-compiled
  evaluation. At build time, walk each expression once and classify as:
  - `Constant(F)` — no eval needed
  - `DirectLoad(usize)` — single `row[idx]`
  - `LinearCombination(F, Vec<(usize, F)>)` — direct multiply-accumulate
  - `General` — fallback to AST walk
- **Impl**: Add `CompiledExpr` enum. Compile at generate_trace time. Eval via match dispatch.

### O3: is_valid constant folding
- **Target sub-metric**: powdr_bus_replay_time_ms (enables better O2 classification)
- **Expected**: minor direct savings, multiplicative with O2
- **What**: During replay `is_valid=1` always. Substitute into expressions at build time,
  then simplify. `is_valid * flag` -> `flag` (DirectLoad). Enables more LinearCombination.
- **Impl**: Walk expressions, substitute is_valid column -> 1, constant-fold.

### O4: Tier classification (precompute constant interactions)
- **Target sub-metric**: powdr_bus_replay_time_ms
- **Expected**: 5-15% reduction
- **What**: Some interactions have constant mult and all-constant args. Precompute once per APC.
  Others have constant mult with DirectLoad args — skip expression eval.
- **Impl**: At build time classify interactions. Constant -> accumulate once. DirectLoad -> col read.

### O5: Parallel chunked replay
- **Target sub-metric**: powdr_bus_replay_time_ms
- **Expected**: 4-8x with multi-core
- **What**: The replay loop is single-threaded. Split rows into chunks, thread-local periphery
  accumulators, merge after.
- **Impl**: rayon par_chunks_mut, thread-local range checker / bitwise lookup, merge.

### O6: Reduce powdr_dummy_traces cost
- **Target sub-metric**: powdr_dummy_traces_time_ms
- **Expected**: save ~5-8s
- **What**: Currently generates full trace matrices with padding for each original chip,
  then extracts only the needed columns. Could extract values directly from record arenas
  without full trace generation.

## Measurement

All runs use:
```bash
./target/release/powdr_openvm_riscv prove guest-keccak --input 10000 \
    --autoprecompiles 30 --mock \
    --compiled /tmp/tracegen-bench/keccak_apc030.cbor \
    --metrics /tmp/tracegen-bench/metrics.json
```

Key metrics to check per optimization:
- `powdr_bus_replay_time_ms` (O1-O5)
- `powdr_dummy_traces_time_ms` (O6)
- `powdr_fill_rows_and_replay_time_ms` (overall fill loop)
- `trace_gen_time_ms` (total)
