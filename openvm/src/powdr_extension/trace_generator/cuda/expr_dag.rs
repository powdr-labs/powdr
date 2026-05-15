//! Convert powdr `AlgebraicExpression` bus interactions into stark-backend's
//! `SymbolicExpressionDag` so we can reuse the upstream `SymbolicRulesBuilder`
//! for CSE / slot allocation / 128-bit `Rule` encoding.
//!
//! Spike entry point: `dump_bus_dag_stats` — when env `POWDR_DUMP_BUS_DAG=1` is
//! set, runs once on the first chip with >= 150 bus interactions and prints
//! `rules_len`, `buffer_size`, `accumulate_count`. Gates whether the full DAG
//! kernel rewrite is worth doing.
#![allow(dead_code)]

use std::{collections::BTreeMap, sync::Arc};

use openvm_cuda_backend::logup_zerocheck::rules::{
    codec::Codec, Rule, Source, SymbolicRulesGpu,
};
use openvm_stark_backend::{
    air_builders::symbolic::{
        symbolic_expression::SymbolicExpression,
        symbolic_variable::{Entry, SymbolicVariable},
        SymbolicDagBuilder, SymbolicExpressionDag, SymbolicExpressionNode,
    },
    p3_field::PrimeField32,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{expression::AlgebraicExpression, symbolic_machine::SymbolicBusInteraction};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use crate::cuda_abi::{
    DevInteractionDag, DevRule, OutputDesc, OUTPUT_KIND_COL, OUTPUT_KIND_CONST, OUTPUT_KIND_INTER,
};

/// dag_idxs for one interaction's outputs, in fixed order `[mult, arg0, arg1, ...]`.
pub struct InteractionOutputs {
    pub bus_id: u32,
    pub num_args: u32,
    pub mult_dag_idx: usize,
    pub arg_dag_idxs: Vec<usize>,
}

/// Convert one expression to an Arc-wrapped `SymbolicExpression`. Each
/// `AlgebraicExpression::Reference` becomes a `SymbolicVariable::Main` whose
/// `index` is the column's index in the APC trace; `offset=0`, `part_index=0`
/// (we have a single main partition).
fn to_symbolic(
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
) -> Arc<SymbolicExpression<BabyBear>> {
    match expr {
        AlgebraicExpression::Number(c) => Arc::new(SymbolicExpression::Constant(*c)),
        AlgebraicExpression::Reference(r) => {
            let var = SymbolicVariable::new(
                Entry::Main {
                    part_index: 0,
                    offset: 0,
                },
                id_to_apc_index[&r.id],
            );
            Arc::new(SymbolicExpression::Variable(var))
        }
        AlgebraicExpression::UnaryOperation(u) => {
            let child = to_symbolic(&u.expr, id_to_apc_index);
            match u.op {
                AlgebraicUnaryOperator::Minus => {
                    let deg = child.degree_multiple();
                    Arc::new(SymbolicExpression::Neg {
                        x: child,
                        degree_multiple: deg,
                    })
                }
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            let l = to_symbolic(&b.left, id_to_apc_index);
            let r = to_symbolic(&b.right, id_to_apc_index);
            let l_deg = l.degree_multiple();
            let r_deg = r.degree_multiple();
            let node = match b.op {
                AlgebraicBinaryOperator::Add => SymbolicExpression::Add {
                    x: l,
                    y: r,
                    degree_multiple: l_deg.max(r_deg),
                },
                AlgebraicBinaryOperator::Sub => SymbolicExpression::Sub {
                    x: l,
                    y: r,
                    degree_multiple: l_deg.max(r_deg),
                },
                AlgebraicBinaryOperator::Mul => SymbolicExpression::Mul {
                    x: l,
                    y: r,
                    degree_multiple: l_deg + r_deg,
                },
            };
            Arc::new(node)
        }
    }
}

/// Build a `SymbolicExpressionDag` over all bus interactions and return it
/// along with the per-interaction output dag_idxs (in `[mult, arg0, ...]`
/// order). `SymbolicDagBuilder` already performs structural dedup +
/// constant-folding peephole (see its docs at `dag.rs:128-141`), so we get the
/// upstream-equivalent of the host-side peephole "for free" inside DAG build.
pub fn algebraic_to_symbolic_dag(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    id_to_apc_index: &BTreeMap<u64, usize>,
) -> (SymbolicExpressionDag<BabyBear>, Vec<InteractionOutputs>) {
    let mut builder = SymbolicDagBuilder::<BabyBear>::new();
    let mut per_interaction = Vec::with_capacity(bus_interactions.len());

    // CRITICAL: `SymbolicDagBuilder.expr_to_idx` caches by raw `*const
    // SymbolicExpression<F>` Arc pointer (see its docstring — assumes the
    // input Arcs stay live for the duration of the build). If we drop an Arc
    // between two `add_expr` calls, the next `Arc::new` may reuse the freed
    // address, and the pointer cache returns the wrong idx. To keep the
    // invariant intact, retain every Arc we hand to `add_expr` in `keepalive`
    // until the whole DAG has been built.
    let mut keepalive: Vec<Arc<SymbolicExpression<BabyBear>>> = Vec::new();
    let trace_args = std::env::var("POWDR_DUMP_BUS_ARGS").is_ok();
    for (intr_i, bi) in bus_interactions.iter().enumerate() {
        let mult_expr = to_symbolic(&bi.mult, id_to_apc_index);
        let mult_dag_idx = builder.add_expr(&mult_expr);
        keepalive.push(mult_expr);

        let mut arg_dag_idxs = Vec::with_capacity(bi.args.len());
        for arg in &bi.args {
            let a = to_symbolic(arg, id_to_apc_index);
            arg_dag_idxs.push(builder.add_expr(&a));
            keepalive.push(a);
        }

        // Spot-print: for the failing bitwise interaction (intr_i == 14, 4
        // args), dump the AST of each arg so we can see whether they're
        // genuinely structurally equal (legitimate dedup) or whether our
        // converter is collapsing distinct exprs.
        if trace_args && intr_i == 14 && bi.args.len() == 4 {
            eprintln!(
                "[bus_args] intr=14 dag_idxs=({}, [{}]) mult={:?}",
                mult_dag_idx,
                arg_dag_idxs
                    .iter()
                    .map(usize::to_string)
                    .collect::<Vec<_>>()
                    .join(","),
                bi.mult
            );
            for (ai, (a, d)) in bi.args.iter().zip(arg_dag_idxs.iter()).enumerate() {
                eprintln!("[bus_args]   arg{} dag_idx={} expr={:?}", ai, d, a);
            }
        }

        per_interaction.push(InteractionOutputs {
            bus_id: bi.id as u32,
            num_args: bi.args.len() as u32,
            mult_dag_idx,
            arg_dag_idxs,
        });
    }

    // `SymbolicRulesBuilder::new` debug-asserts `constraint_idx.is_sorted()`.
    // Collect all output dag_idxs, sort, dedup.
    let mut output_idxs: Vec<usize> = per_interaction
        .iter()
        .flat_map(|i| std::iter::once(i.mult_dag_idx).chain(i.arg_dag_idxs.iter().copied()))
        .collect();
    output_idxs.sort_unstable();
    output_idxs.dedup();

    let dag = SymbolicExpressionDag {
        nodes: builder.nodes,
        constraint_idx: output_idxs,
    };
    (dag, per_interaction)
}

/// Compile bus interactions for the DAG kernel. Returns three device-ready
/// vectors:
/// - `rules`: 128-bit encoded `DevRule { low, high }` array (CSE-deduped by
///   `SymbolicRulesBuilder` with `buffer_vars=true`).
/// - `interactions`: `DevInteractionDag` per bus interaction (bus_id, num_args,
///   outputs_off).
/// - `output_descs`: flat dispatch table — one `OutputDesc` per (interaction,
///   slot) in order `[i0_mult, i0_arg0, ..., i1_mult, ...]`. Each descriptor
///   tells the kernel where to read the value: `inter[slot]`, `d_output[col +
///   r]`, or inline `Fp(const)`.
pub fn compile_bus_to_gpu_dag(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    id_to_apc_index: &std::collections::BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevRule>, Vec<DevInteractionDag>, Vec<OutputDesc>, usize) {
    let (dag, outputs) = algebraic_to_symbolic_dag(bus_interactions, id_to_apc_index);
    let rules_gpu: SymbolicRulesGpu<BabyBear> = SymbolicRulesGpu::new(&dag, /*buffer_vars=*/ true);

    // Validate that every source we'll send to the kernel is one of the three
    // types our kernel handles: Source::Intermediate, Source::Var, Source::Constant.
    // Anything else (IsFirst/IsLast/IsTransition or any Entry::{Preprocessed,Permutation,Public,Challenge,Exposed})
    // would trip the device-side assert.
    if std::env::var("POWDR_VALIDATE_BUS_DAG").is_ok() {
        for (i, r) in rules_gpu.rules.iter().enumerate() {
            let sources: Vec<&Source<BabyBear>> = match &r.inner {
                Rule::Add(x, y, z) | Rule::Sub(x, y, z) | Rule::Mul(x, y, z) => vec![x, y, z],
                Rule::Neg(x, z) => vec![x, z],
                Rule::Variable(x) => vec![x],
                Rule::BufferVar(x, z) => vec![x, z],
            };
            for s in sources {
                match s {
                    Source::Intermediate(_)
                    | Source::TerminalIntermediate
                    | Source::Constant(_) => {}
                    Source::Var(v) => match v.entry {
                        Entry::Main { part_index, offset } => {
                            if part_index != 0 || offset != 0 {
                                eprintln!("[bus_dag_check] rule {}: Var with non-default Main entry: part={} offset={}", i, part_index, offset);
                            }
                        }
                        other => eprintln!("[bus_dag_check] rule {}: Var with non-Main entry: {:?}", i, other),
                    },
                    other => eprintln!(
                        "[bus_dag_check] rule {}: unsupported source {:?}",
                        i, other
                    ),
                }
            }
        }
    }

    // Encode rules to device format.
    let dev_rules: Vec<DevRule> = rules_gpu
        .rules
        .iter()
        .map(|r| {
            let encoded: u128 = r.encode();
            DevRule {
                low: encoded as u64,
                high: (encoded >> 64) as u64,
            }
        })
        .collect();

    // For each output (mult + each arg) we need the rule for the corresponding
    // dag_idx, then case-split on the rule shape to emit an `OutputDesc`.
    //
    // dag_idx_to_rule_idx only contains accumulate=true entries — which we
    // marked via `constraint_idx`. So every output dag_idx is present.
    let mut output_descs: Vec<OutputDesc> = Vec::new();
    let mut dev_interactions: Vec<DevInteractionDag> = Vec::with_capacity(outputs.len());

    for out in &outputs {
        let outputs_off = output_descs.len() as u32;

        // Push mult dispatch desc first, then each arg's desc.
        let all_outs = std::iter::once(out.mult_dag_idx).chain(out.arg_dag_idxs.iter().copied());
        for dag_idx in all_outs {
            let rule_idx = rules_gpu
                .dag_idx_to_rule_idx
                .get(&dag_idx)
                .copied()
                .unwrap_or_else(|| panic!("output dag_idx={} has no rule_idx (not accumulated?)", dag_idx));
            let rule = &rules_gpu.rules[rule_idx].inner;
            output_descs.push(rule_to_output_desc(rule, apc_height));
        }

        dev_interactions.push(DevInteractionDag {
            bus_id: out.bus_id,
            num_args: out.num_args,
            outputs_off,
            flags: 0,
        });
    }

    (dev_rules, dev_interactions, output_descs, rules_gpu.buffer_size)
}

fn rule_to_output_desc(rule: &Rule<BabyBear>, apc_height: usize) -> OutputDesc {
    // Helper that converts a `Source` to a `(kind, value)` pair for outputs.
    // For Source::Intermediate(slot) → (Inter, slot)
    // For Source::Var(v) → (Col, v.index * apc_height) — single-use bare col
    // For Source::Constant(c) → (Const, c.as_canonical_u32())
    // Other sources are unreachable for bus outputs.
    let source_to_desc = |src: &Source<BabyBear>| -> OutputDesc {
        match src {
            Source::Intermediate(slot) => OutputDesc {
                kind: OUTPUT_KIND_INTER,
                value: *slot as u32,
            },
            Source::Var(v) => OutputDesc {
                kind: OUTPUT_KIND_COL,
                value: (v.index * apc_height) as u32,
            },
            Source::Constant(c) => OutputDesc {
                kind: OUTPUT_KIND_CONST,
                value: c.as_canonical_u32(),
            },
            Source::TerminalIntermediate => {
                panic!("output rule has Source::TerminalIntermediate — accumulate=true should have allocated a slot")
            }
            Source::IsFirst | Source::IsLast | Source::IsTransition => {
                panic!("bus outputs should never reference IsFirst/IsLast/IsTransition")
            }
        }
    };

    // For a rule with a `z` operand (Add/Sub/Mul/Neg/BufferVar), z is the
    // result location — that's what we want for downstream lookup.
    // For `Rule::Variable(x)`, the rule produces the value of `x` inline (no
    // slot), so we case on `x` directly.
    match rule {
        Rule::Add(_, _, z)
        | Rule::Sub(_, _, z)
        | Rule::Mul(_, _, z)
        | Rule::Neg(_, z)
        | Rule::BufferVar(_, z) => source_to_desc(z),
        Rule::Variable(x) => source_to_desc(x),
    }
}

/// Run the full host-side DAG build + GKR rule scheduler on every chip whose
/// `bus_interactions.len()` clears the threshold, and accumulate stats.
/// Reports one summary line per chip, then a final "max across chips" line so
/// we can decide local vs global buffer mode.
/// Gated by env `POWDR_DUMP_BUS_DAG=1`. Runs once per chip per process.
pub fn dump_bus_dag_stats(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    id_to_apc_index: &BTreeMap<u64, usize>,
) {
    if std::env::var("POWDR_DUMP_BUS_DAG").is_err() {
        return;
    }
    let threshold: usize = std::env::var("POWDR_DUMP_BUS_DAG_MIN")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(50);
    if bus_interactions.len() < threshold {
        return;
    }

    use openvm_cuda_backend::logup_zerocheck::rules::{SymbolicRulesBuilder, SymbolicRulesGpu};

    let (dag, outputs) = algebraic_to_symbolic_dag(bus_interactions, id_to_apc_index);

    let n_nodes = dag.nodes.len();
    let n_outputs: usize = outputs.iter().map(|i| 1 + i.arg_dag_idxs.len()).sum();
    let n_distinct_outputs = dag.constraint_idx.len();

    let (mut n_const, mut n_var, mut n_add, mut n_sub, mut n_mul, mut n_neg) =
        (0usize, 0, 0, 0, 0, 0);
    for node in &dag.nodes {
        match node {
            SymbolicExpressionNode::Constant(_) => n_const += 1,
            SymbolicExpressionNode::Variable(_) => n_var += 1,
            SymbolicExpressionNode::Add { .. } => n_add += 1,
            SymbolicExpressionNode::Sub { .. } => n_sub += 1,
            SymbolicExpressionNode::Mul { .. } => n_mul += 1,
            SymbolicExpressionNode::Neg { .. } => n_neg += 1,
            _ => {}
        }
    }

    let mut rb = SymbolicRulesBuilder::new(&dag, /*buffer_vars=*/ true);
    rb.set_range(0, dag.nodes.len());
    let rules: SymbolicRulesGpu<BabyBear> = rb.to_rules();

    // Maintain a global max across chips so we can spot the worst-case buffer_size.
    use std::sync::atomic::{AtomicUsize, Ordering};
    static MAX_BUF: AtomicUsize = AtomicUsize::new(0);
    static MAX_RULES: AtomicUsize = AtomicUsize::new(0);
    static MAX_NODES: AtomicUsize = AtomicUsize::new(0);
    let prev_buf = MAX_BUF.fetch_max(rules.buffer_size, Ordering::Relaxed);
    let prev_rules = MAX_RULES.fetch_max(rules.rules.len(), Ordering::Relaxed);
    let prev_nodes = MAX_NODES.fetch_max(n_nodes, Ordering::Relaxed);

    eprintln!(
        "[bus_dag] chip: n_intr={:>4} outs={:>4} dag_nodes={:>4} (C{:>3} V{:>3} A{:>3} S{:>2} M{:>3} N{:>2}) rules={:>4} buffer_size={:>3}",
        bus_interactions.len(),
        n_outputs,
        n_nodes,
        n_const, n_var, n_add, n_sub, n_mul, n_neg,
        rules.rules.len(),
        rules.buffer_size,
    );
    // Note when this chip raised any of the running maxima (cheap signal of the
    // worst-case chip for global-vs-local decision).
    let new_max =
        prev_buf < rules.buffer_size || prev_rules < rules.rules.len() || prev_nodes < n_nodes;
    if new_max {
        eprintln!(
            "[bus_dag]   ^ new max: max_buffer_size={} max_rules={} max_nodes={}",
            MAX_BUF.load(Ordering::Relaxed),
            MAX_RULES.load(Ordering::Relaxed),
            MAX_NODES.load(Ordering::Relaxed),
        );
    }
    // Silence the unused-`n_distinct_outputs` warning while keeping it computed
    // (handy for debugging if needed).
    let _ = n_distinct_outputs;
}
