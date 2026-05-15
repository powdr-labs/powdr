# Plan v2: GKR-style DAG eval for `apc_apply_bus_kernel`

Priorities (strict order):
1. **Runtime speed** of bus_kernel on pairing APC=500.
2. **Reuse** stark-backend's GKR/Rule/SymbolicRulesBuilder infrastructure.
3. **Minimize diff** in powdr and avoid upstream stark-backend changes.

## Non-negotiable: CSE *must* be the GKR Rule/DAG mechanism

The entire point of this rewrite is to emulate the GKR DAG approach to CSE. Any path that re-invents a CSE scheme (custom intermediates buffer, ad-hoc opcodes added to the stack VM, host-side substitution before bytecode emit) is **out of scope** even if it benchmarks faster. Concretely this means:

- Bytecode format: the upstream `Rule { low, high }` 128-bit struct from `codec.cuh`, **not** an extension of the current `OpCode` enum.
- Slot allocation: `SymbolicRulesBuilder` from `cuda-backend/src/logup_zerocheck/rules/mod.rs`, **not** a powdr-side hand-rolled priority queue or use-count heuristic.
- Intermediates buffer: the `Fp inter[K]` register / strided-global `DeviceBuffer` pattern from `gkr_input.cu`, **not** a per-thread stack VM extended with `LOAD_INTER`/`STORE_INTER` opcodes.
- Output identification: the existing `buffer_vars=true` + `constraint_idx` mechanism for forcing buffered slots, **not** a parallel "output table" defined outside the rule scheduler.

Anything we add to powdr is plumbing (algebraic→symbolic conversion, FFI signature, runtime flag, Fp scalar-eval helper) — the CSE algorithm itself is upstream's, unchanged.

## Current baseline (HEAD = `7b64604fa`)

The other agent already shipped two non-DAG bus optimisations:
- `4d5f13c6b` — peephole folds in `emit_expr`: `x*1 → x`, `x*0 → 0`, `x*(-1) → -x`, `x±0 → x`, `Neg(Number c) → Number(-c)`.
- `7b64604fa` — `INTR_FLAG_STATIC_MULT_1`: 81% of pairing-APC interactions have `mult = is_valid * 1`; the kernel now skips that bytecode walk and m==0 short-circuit.

CSE potential measured **on the raw `AlgebraicExpression` AST** (host-side `analyze_bus_cse`, pairing APC=500, first ≥150-interaction chip):
- 196 interactions, 970 arg expressions, **676 distinct subexpressions**, 3663 raw ops, **944 ops removable by perfect CSE (25.8%)**.
- BUT **81% of those 944 ops are trivial peephole cases** (`is_valid * 1`, `is_valid * -1`, `15360 * timestamp`, `-1`) that the peephole at `4d5f13c6b` already eliminates at bytecode-emit time. After peephole, those become 2-op leaves whose CSE payoff is zero (single-eval cost = `PushIntermediate` cost = 2 ops).
- Residual *structural* CSE potential (e.g. `mem_ptr_limbs__0_X + 65536 * mem_ptr_limbs__1_X` doubletons, ~11 patterns at 6 saved ops each) ≈ **70–100 ops out of ~2700 post-peephole bytecode**, i.e. **~3% on top of the peephole**.

That's the ceiling for the DAG rewrite on this workload. Expected wall-time win on bus_kernel: ≤10 ms out of ~213 ms (≤0.4% of total prove time). Going ahead per user instruction because every other lever has been pulled.

## Reused infrastructure (no upstream patch)

| component | source | role |
|---|---|---|
| `Rule` (16 B), `RuleHeader`, `SourceInfo`, `OperationType`, `decode_rule_header`, `decode_y`, `decode_z_index` | `stark-backend/.../cuda-backend/cuda/include/codec.cuh` | device-side rule decode |
| `SymbolicRulesBuilder`, `SymbolicRulesGpu`, `Rule<F>`, `Source<F>`, `RuleWithFlag<F>` | `stark-backend/.../cuda-backend/src/logup_zerocheck/rules/{mod,codec}.rs` | host: hash-cons, lifetime analysis, slot-priority-queue, 128-bit encoding |
| `SymbolicExpressionDag<F>`, `SymbolicExpressionNode<F>`, `SymbolicVariable<F>::{Main}` | `stark-backend/.../stark-backend/src/air_builders/symbolic/dag.rs` + `symbolic_variable.rs` | input to `SymbolicRulesBuilder::new` |
| Kernel pattern (per-row DAG walk with intermediates buffer, local-vs-global mode) | `stark-backend/.../cuda-backend/cuda/src/logup_zerocheck/gkr_input.cu` | template for our Fp-flavour kernel |

All public in our existing dep graph — no fork. **`codec.cuh` is vendored** (header-only, ~157 LOC) because `cuda-backend` doesn't expose `DEP_CUDA_BACKEND_CUDA_INCLUDE` (no `links` + no `cargo:include=` emit in its build.rs).

## Reviewer-flagged fixes folded in

Independent review of v2 found four correctness gotchas, one reuse improvement, and one bench-fairness gap. Folded into the file plan below; flagged here so the implementer doesn't regress them.

1. **Slot lookup is not a single `decode_z_index` call.** `SymbolicRulesGpu::dag_idx_to_rule_idx` gives `rule_idx` (`rules/mod.rs:408-412`), and only `accumulate=true` dag_idxs get entries (`rules/mod.rs:400`). `buffer_vars=true` only forces a slot for **shared** Variables — `use_count > 1` (`rules/mod.rs:281`). For each interaction output we must inspect the resulting rule and case-split:
   - `Rule::Add/Sub/Mul/Neg` (always `intermediate=true`, slot present): read `inter[decode_z_index]`.
   - `Rule::BufferVar` (shared Variable hoisted to a slot): read `inter[decode_z_index]`.
   - `Rule::Variable(Source::Constant(c))` (constant outputs — e.g. `Number(1)` mult collapsed by peephole): no slot, **embed the constant in the per-output dispatch table** so the kernel uses it directly.
   - `Rule::Variable(Source::Var(v))` with `use_count == 1` (bare-column output that nobody else uses): no slot, **embed the column-base in the per-output dispatch table** so the kernel reads `d_output[base + r]` directly.
   - The dispatch table emitted by the host becomes: `enum { Const(Fp), Col(u32 base), Inter(u32 slot) }` per output (flat, 8 bytes/output max).

2. **`d_main` is an indirection table, not the trace.** `evaluate_dag_entry_gkr` in `gkr_input.cu:33` reads `(Fp*)d_main[src.part]` — i.e. `d_main` is `uint64_t*` pointing at an array of column-set pointers. To reuse upstream's helper signature, the launcher allocates a one-element `uint64_t[1] = {(uint64_t)d_output}` device array and passes its address. ~10 LOC at host launch time. Alternative (slightly smaller): skip the helper's `ENTRY_MAIN` arm entirely and have our `evaluate_dag_entry_bus` take a plain `const Fp* d_main` directly. Choose this — see (3).

3. **Reuse upstream helper via templatisation, not rewrite.** A `template<typename T>` over the return type lets us reuse `evaluate_dag_entry_gkr`'s body verbatim — `T(...)` constructors work for both `Fp` and `FpExt`. **But** the helper reads `d_preprocessed`/`d_main[part]`/`d_public_values`/`d_challenges`/`d_intermediates` which we don't all have or need. Cheaper path: a small `evaluate_dag_entry_bus` that takes only `(SourceInfo, row, d_main: const Fp*, d_inter: const Fp*, stride, H)` — 4 EntryType arms instead of 11 (`ENTRY_MAIN`, `SOURCE_CONSTANT`, `SRC_INTERMEDIATE`, default → assert). This is ~25 LOC and avoids the `d_main[part]` indirection entirely. The cost of "not reusing" the helper here is small relative to the cost of building the indirection table.

4. **`SymbolicRulesBuilder::new` debug-asserts `dag.constraint_idx.iter().is_sorted()`** (`rules/mod.rs:134`). After collecting per-interaction output dag_idxs in `[i0_mult, i0_arg0, …, i1_mult, …]` order, sort the union for `constraint_idx`. Keep the original per-output ordering on the side (for emit) in `Vec<Vec<usize>>` indexed by interaction.

5. **Bracket the global-mode intermediates allocation.** When `buffer_size > LOCAL_K`: hoist the `DeviceBuffer<Fp>` of size `TASK_SIZE × buffer_size` to a per-chip cache (allocated once on the first call, kept alive in `PowdrTraceGeneratorGpu`). The per-launch cost becomes zero. Without this, alloc cost would leak out of `bus_kernel_us` and bias the A/B.

## Diff (estimated +~600 / -180)

### New
1. `openvm/cuda/include/codec.cuh` — vendored verbatim from stark-backend SHA `8d36ad26`, header comment cites source. ~157 LOC.
2. `openvm/cuda/src/apc_apply_bus_dag.cu` — new kernel + host launcher. ~250 LOC.
   - `evaluate_dag_entry_bus(SourceInfo, row, d_main, d_inter, stride, H) -> Fp` (~30 LOC) — mirror of `evaluate_dag_entry_gkr` minus FpExt and minus the preprocessed/public/challenge/is_first/is_last/is_transition arms (bus exprs only use `ENTRY_MAIN`, `SOURCE_CONSTANT`, `SRC_INTERMEDIATE`).
   - `apc_apply_bus_dag_kernel<bool GLOBAL>` — one thread per row; walks `d_rules` to fill `inter_ptr[buffer_slot * stride]`; after all rules done, reads each interaction's `mult` + args from `inter_ptr` and runs the existing range/tuple2/bitwise histogram dispatch verbatim.
   - Local mode: `Fp inter[K]` with `K=24` compile-time. Global mode if `buffer_size > K`: thread-strided `DeviceBuffer<Fp>` of size `TASK_SIZE × buffer_size`.
3. `openvm/src/powdr_extension/trace_generator/cuda/expr_dag.rs` — converter. ~120 LOC.
   - `algebraic_to_symbolic_dag(bus_interactions, id_to_apc_index, apc_height) -> (SymbolicExpressionDag<BabyBear>, Vec<InteractionOutputDagIdxs>)` where `InteractionOutputDagIdxs = { bus_id, mult_dag_idx, arg_dag_idxs: Vec<usize>, num_args }`.
   - Hash-cons over `AlgebraicExpression` using `(op, left_dag_idx, right_dag_idx)` as key (refs + numbers as leaves).
   - Mapping: `Number(c) → Constant(c)`; `Reference(r) → Variable(SymbolicVariable{ entry: Entry::Main{ part_index: 0, offset: 0 }, index: id_to_apc_index[&r.id] })`; `UnaryOp(Minus, e) → Neg{ idx }`; `BinaryOp(Add|Sub|Mul, l, r) → Add|Sub|Mul{ left_idx, right_idx }`.
   - Populate `dag.constraint_idx` with the dag_idx of every interaction output (mult + each arg).
   - The `degree_multiple` field on `SymbolicExpressionNode` is required (the type carries it) — compute conservatively as `left.deg + right.deg` for Mul, `max(left.deg, right.deg)` for Add/Sub, equal-to-input for Neg, 0 for leaves. (Used only for `max_rotation()` queries on the host; the kernel doesn't read it.)

### Modified
4. `openvm/build.rs` — no change. `codec.cuh` is already under `cuda/include` (vendored), and the existing `.include("cuda/include")` picks it up.
5. `openvm/src/cuda_abi.rs` — add new extern `_apc_apply_bus_dag(...)` and safe wrapper `apc_apply_bus_dag(...)`. New args: `d_rules: *const Rule` (16 B/elt), `n_rules: usize`, `d_intermediates: *mut Fp` (global-mode buffer; pass null/empty when local mode), `buffer_size: u32`, `d_interactions_dag: *const DevInteractionDag` (length n_interactions), `d_output_dag_idxs: *const u32` (flat, one entry per interaction-output, length = sum_i (1+num_args[i])). Existing histogram args unchanged. **Do not delete `_apc_apply_bus`** — both paths coexist behind a runtime flag.
6. `openvm/src/powdr_extension/trace_generator/cuda/mod.rs` — replace `compile_bus_to_gpu` with a thin selector:
   - Read `POWDR_BUS_KERNEL` once at process start (`OnceLock`). Values: `"vm"` (default, current stack-VM) or `"dag"` (new path).
   - If `"vm"`: existing path, unchanged.
   - If `"dag"`: call `algebraic_to_symbolic_dag` → `dag`. `SymbolicRulesGpu::new(&dag, /*buffer_vars=*/true)` → `(rules: Vec<Rule>, buffer_size: usize, dag_idx_to_rule_idx)`. For each interaction-output `dag_idx`, decode the corresponding rule's `z_index` (or, for Variable outputs which the builder now buffers thanks to `buffer_vars=true`, the same decode applies) to get its buffer slot. Emit `DevInteractionDag { bus_id, num_args, output_idxs_off }` and a flat `output_dag_idxs: Vec<u32>` ordered `[i0_mult, i0_arg0, …, i1_mult, …]`.
   - Host-allocate the intermediates `DeviceBuffer<Fp>` only if `buffer_size > LOCAL_K`; otherwise pass an empty `DeviceBuffer` and the kernel uses its register array.
   - Same `bus_compile_h2d` / `bus_kernel` `timed_substage!` envelopes.

### Deleted
None initially. After the perf bench confirms a winner, a follow-up commit deletes the loser.

## Why this fix-set addresses the reviewer's flagged risks

| reviewer flag | resolution in plan v2 |
|---|---|
| **Variable outputs not buffered with `accumulate=true` alone** | Use `SymbolicRulesGpu::new(&dag, true)` — `buffer_vars=true` forces a slot for shared Variable nodes (`mod.rs:178-184`). Confirmed via `mod.rs:281` condition: `intermediate \|\| (buffer_var && use_count > 1)`. |
| **`DEP_CUDA_BACKEND_CUDA_INCLUDE` doesn't exist** | Vendor `codec.cuh` from day 1. Comment cites source SHA so future updates are traceable. |
| **Global-mode buffer bandwidth risk with ~120 slots** | Threshold check at host: `if buffer_size > LOCAL_K { go_global = true; emit warning to tracing; }`. After build runs once, we know the actual `buffer_size` — if global, we measure A/B and revert if regress >5%. |
| **CSE-ratio test doesn't predict speed** | Already measured the ratio (25.8% raw, ~3% post-peephole). Per user's call we proceed anyway. The bench (step 6 below) is the real gate. |
| **No Fp `evaluate_dag_entry`** | We write our own (~30 LOC). Plan accounts. |
| **App-replay non-determinism could break A/B in ncu** | We're not relying on ncu for the bench. Use `POWDR_TRACE_PROFILE=1` + `bus_kernel_us` counter, which is summed across all 704 launches per prove and doesn't require kernel replay. |

## Sequence of work

1. **Vendor `codec.cuh`** — copy + provenance comment. 5 min.
2. **Write `algebraic_to_symbolic_dag`** + unit test (round-trip a small expression). 30 min.
3. **One-off host-side measurement** — call `SymbolicRulesGpu::new` on a real pairing APC=500 chip via a `#[ignore]` test or a `POWDR_DUMP_BUS_DAG=1` debug path; log `rules_len`, `buffer_size`, `accumulate_count`. **Gating decision**: if `buffer_size ≤ 24` → proceed local-mode (high confidence speedup); if 24 < `buffer_size` ≤ 64 → proceed cautiously (uncertain); if > 64 → reconsider (global-mode likely regresses given the small structural CSE pool). 15 min.
4. **Write `apc_apply_bus_dag.cu`** + Fp `evaluate_dag_entry_bus` helper. 60–90 min.
5. **Wire FFI + Rust selector** behind `POWDR_BUS_KERNEL=dag`. 30 min.
6. **Bit-equal smoke test** — pairing APC=10 with `POWDR_BUS_KERNEL=dag`. Compare `public_values_commit` against the same prove with default `vm`. 10 min.
7. **Perf bench** — pairing APC=500 with `POWDR_TRACE_PROFILE=1`, both kernels (set the env var to switch between runs). Compare:
   - `bus_kernel_us` total (host-side timer, microsecond resolution)
   - `bus_compile_h2d_us` total (DAG-build adds host work; expected ~+30%)
   - nsys top-kernel slice `apc_apply_bus_dag_kernel` vs `apc_apply_bus_kernel` (% GPU, total ns, avg per launch)
   - Emit a `combined_metrics.json` snapshot keyed `{vm_baseline, dag_v1}`.
8. **Ship/kill** — given the ~3% theoretical ceiling, the ship bar drops to **any net win** on the combined `bus_kernel_us + bus_compile_h2d_us`. A regression on either ⇒ revert. The point of running the experiment isn't a guaranteed win; it's to establish empirically whether the GKR DAG approach can beat a heavily-optimised stack VM on a workload with structurally low CSE potential. Either result is publishable.

## Estimated bound on the prize

Conservative: peephole already captured ~50% of redundant bytecode. Residual structural CSE is ~70-100 ops out of ~2700 per-row bytecode = ~3% reduction. If that translates linearly to bus_kernel time (it won't — global-mode buffer overhead could eat it), upper bound is ~6 ms saved on 213 ms = **~2.8% bus_kernel speedup**. Below the 10% ship criterion. Plan ships only if reality beats theory.

## Rollout

- One commit adds the entire DAG path behind `POWDR_BUS_KERNEL=dag` (default off). Mergeable to the parent branch independent of the bench outcome.
- A follow-up commit either flips the default to `dag` and deletes the VM path, or reverts the DAG-path commit entirely.
