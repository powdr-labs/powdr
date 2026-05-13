use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use openvm_circuit::{
    arch::{ChipInventory, DenseRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_circuit_primitives::Chip;
use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::copy::MemCopyH2D;
use openvm_stark_backend::{
    p3_field::{PrimeCharacteristicRing, PrimeField32},
    prover::{AirProvingContext, ProverBackend},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    symbolic_machine::SymbolicBusInteraction,
};
use powdr_constraint_solver::constraint_system::{ComputationMethod, DerivedVariable};
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use crate::{
    cuda_abi::{self, DerivedExprSpec, DevInteraction, ExprSpan, OpCode, OriginalAir, Subst},
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::{IsaApc, OpenVmISA},
    powdr_extension::{chip::PowdrChipGpu, executor::OriginalArenas},
    BabyBearSC, GpuBackend,
};

mod inventory;
mod periphery;

pub use inventory::GpuDummyChipComplex;
pub use periphery::{
    PowdrPeripheryInstancesGpu, SharedPeripheryChipsGpu, SharedPeripheryChipsGpuProverExt,
};

/// Encodes an algebraic expression into GPU stack-machine bytecode.
///
/// Appends instructions to `bc` representing `expr` using the opcodes in `OpCode`.
/// References are encoded as `PushApc` with a column-major offset computed from
/// `id_to_apc_index` and `apc_height` (offset = apc_col_index * apc_height).
/// Constants are encoded as `PushConst` followed by the field element as `u32`.
/// Unary minus and binary operations map to `Neg`, `Add`, `Sub`, and `Mul`.
///
/// Peephole folds applied here (host-side): `x*0→0`, `x*1→x`, `x*(-1)→-x`,
/// `x±0→x`, `Neg(Number c)→Number(-c)`. The powdr-level optimizer doesn't
/// fold these inside bus expressions, but on pairing-style APCs they account
/// for ~50% of the redundant bytecode the GPU VM would otherwise evaluate
/// once per row (see `analyze_bus_cse`).
///
/// Note: This function does not track or enforce the evaluation stack depth,
/// which is done in device code.
fn emit_expr(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match expr {
        AlgebraicExpression::Number(c) => {
            bc.push(OpCode::PushConst as u32);
            bc.push(c.as_canonical_u32());
        }
        AlgebraicExpression::Reference(r) => {
            let idx = (id_to_apc_index[&r.id] * apc_height) as u32;
            bc.push(OpCode::PushApc as u32);
            bc.push(idx);
        }
        AlgebraicExpression::UnaryOperation(u) => {
            // Fold Neg(Number(c)) → Number(-c).
            if let (AlgebraicUnaryOperator::Minus, AlgebraicExpression::Number(c)) =
                (u.op, &*u.expr)
            {
                bc.push(OpCode::PushConst as u32);
                bc.push((-*c).as_canonical_u32());
                return;
            }
            emit_expr(bc, &u.expr, id_to_apc_index, apc_height);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            let zero = BabyBear::ZERO;
            let one = BabyBear::ONE;
            let neg_one = -BabyBear::ONE;
            let as_const = |e: &AlgebraicExpression<BabyBear>| match e {
                AlgebraicExpression::Number(c) => Some(*c),
                _ => None,
            };
            let l_const = as_const(&b.left);
            let r_const = as_const(&b.right);
            match b.op {
                AlgebraicBinaryOperator::Mul => {
                    // x*0 → 0, 0*x → 0 (kills the other side too)
                    if l_const == Some(zero) || r_const == Some(zero) {
                        bc.push(OpCode::PushConst as u32);
                        bc.push(0);
                        return;
                    }
                    // 1*x → x, x*1 → x
                    if l_const == Some(one) {
                        emit_expr(bc, &b.right, id_to_apc_index, apc_height);
                        return;
                    }
                    if r_const == Some(one) {
                        emit_expr(bc, &b.left, id_to_apc_index, apc_height);
                        return;
                    }
                    // (-1)*x → -x, x*(-1) → -x
                    if l_const == Some(neg_one) {
                        emit_expr(bc, &b.right, id_to_apc_index, apc_height);
                        bc.push(OpCode::Neg as u32);
                        return;
                    }
                    if r_const == Some(neg_one) {
                        emit_expr(bc, &b.left, id_to_apc_index, apc_height);
                        bc.push(OpCode::Neg as u32);
                        return;
                    }
                }
                AlgebraicBinaryOperator::Add => {
                    // 0+x → x, x+0 → x
                    if l_const == Some(zero) {
                        emit_expr(bc, &b.right, id_to_apc_index, apc_height);
                        return;
                    }
                    if r_const == Some(zero) {
                        emit_expr(bc, &b.left, id_to_apc_index, apc_height);
                        return;
                    }
                }
                AlgebraicBinaryOperator::Sub => {
                    // x-0 → x. 0-x is left as Sub (cheaper than emitting Neg).
                    if r_const == Some(zero) {
                        emit_expr(bc, &b.left, id_to_apc_index, apc_height);
                        return;
                    }
                }
            }
            emit_expr(bc, &b.left, id_to_apc_index, apc_height);
            emit_expr(bc, &b.right, id_to_apc_index, apc_height);
            match b.op {
                AlgebraicBinaryOperator::Add => bc.push(OpCode::Add as u32),
                AlgebraicBinaryOperator::Sub => bc.push(OpCode::Sub as u32),
                AlgebraicBinaryOperator::Mul => bc.push(OpCode::Mul as u32),
            }
        }
    }
}

/// Given the current bytecode, appends bytecode for the expression `expr` and returns the associated span
fn emit_expr_span(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> ExprSpan {
    // The span starts where the bytecode currently ends
    let off = bc.len() as u32;
    // Append the bytecode for `expr`
    emit_expr(bc, expr, id_to_apc_index, apc_height);
    // Calculate the length of the span
    let len = (bc.len() as u32) - off;
    ExprSpan { off, len }
}

/// Compile derived columns to GPU bytecode according to input order.
fn compile_derived_to_gpu(
    derived_columns: &[DerivedVariable<
        BabyBear,
        AlgebraicReference,
        AlgebraicExpression<BabyBear>,
    >],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DerivedExprSpec>, Vec<u32>) {
    let mut specs = Vec::with_capacity(derived_columns.len());
    let mut bytecode = Vec::new();

    for DerivedVariable {
        variable,
        computation_method,
    } in derived_columns
    {
        let apc_col_index = apc_poly_id_to_index[&variable.id];
        let off = bytecode.len() as u32;
        match computation_method {
            ComputationMethod::Constant(c) => {
                // Encode constant as an expression
                bytecode.push(OpCode::PushConst as u32);
                bytecode.push(c.as_canonical_u32());
            }
            ComputationMethod::QuotientOrZero(e1, e2) => {
                // Invert denominator (or use zero), then multiply with numerator.
                emit_expr(&mut bytecode, e2, apc_poly_id_to_index, apc_height);
                bytecode.push(OpCode::InvOrZero as u32);
                emit_expr(&mut bytecode, e1, apc_poly_id_to_index, apc_height);
                bytecode.push(OpCode::Mul as u32);
            }
        }
        let len = (bytecode.len() as u32) - off;
        specs.push(DerivedExprSpec {
            col_base: (apc_col_index * apc_height) as u64,
            span: ExprSpan { off, len },
        });
    }

    (specs, bytecode)
}

/// Count, for the first PowdrAir whose bus_interactions exceed a threshold, how
/// much redundancy exists across the AlgebraicExpression trees of all (mult,
/// args). Useful to decide whether a CSE-aware emitter would pay off vs the
/// current direct-emit walk. Gated by `POWDR_DUMP_BUS_CSE=1` and only runs once
/// per process.
fn analyze_bus_cse(bus_interactions: &[SymbolicBusInteraction<BabyBear>]) {
    if std::env::var("POWDR_DUMP_BUS_CSE").is_err() {
        return;
    }
    let threshold: usize = std::env::var("POWDR_DUMP_BUS_CSE_MIN")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(150);
    if bus_interactions.len() < threshold {
        return; // wait for a big chip
    }
    static DONE: std::sync::Once = std::sync::Once::new();
    DONE.call_once(|| {
        // Walk every subtree, counting occurrences. AlgebraicExpression derives
        // Hash/Eq; hashing/clone cost is O(subtree-size) per insert. Tolerable
        // because this runs once.
        fn walk(
            e: &AlgebraicExpression<BabyBear>,
            counts: &mut HashMap<AlgebraicExpression<BabyBear>, usize>,
        ) {
            *counts.entry(e.clone()).or_insert(0) += 1;
            match e {
                AlgebraicExpression::Number(_) | AlgebraicExpression::Reference(_) => {}
                AlgebraicExpression::UnaryOperation(u) => walk(&u.expr, counts),
                AlgebraicExpression::BinaryOperation(b) => {
                    walk(&b.left, counts);
                    walk(&b.right, counts);
                }
            }
        }
        // emit cost in the current direct-emit scheme.
        fn ops(e: &AlgebraicExpression<BabyBear>) -> usize {
            match e {
                AlgebraicExpression::Number(_) | AlgebraicExpression::Reference(_) => 2,
                AlgebraicExpression::UnaryOperation(u) => 1 + ops(&u.expr),
                AlgebraicExpression::BinaryOperation(b) => 1 + ops(&b.left) + ops(&b.right),
            }
        }

        let mut counts: HashMap<AlgebraicExpression<BabyBear>, usize> = HashMap::new();
        for bi in bus_interactions {
            walk(&bi.mult, &mut counts);
            for a in &bi.args {
                walk(a, &mut counts);
            }
        }

        let mut total_ops_no_cse = 0usize;
        let mut total_args = 0usize;
        for bi in bus_interactions {
            total_ops_no_cse += ops(&bi.mult);
            total_args += 1;
            for a in &bi.args {
                total_ops_no_cse += ops(a);
                total_args += 1;
            }
        }

        // Perfect CSE: each unique subtree is evaluated once and stored to a slot
        // (no extra cost; the result lives on the stack). Each *additional*
        // occurrence is replaced by a `PushIntermediate <slot>` = 2 ops. Savings
        // per duplicate = max(0, ops_of_subtree - 2).
        let mut savings = 0usize;
        let mut savings_only_multi_op = 0usize;
        for (subtree, &count) in &counts {
            if count < 2 {
                continue;
            }
            let single_eval_ops = ops(subtree);
            let per_dup = single_eval_ops.saturating_sub(2);
            savings += (count - 1) * per_dup;
            if single_eval_ops > 4 {
                savings_only_multi_op += (count - 1) * per_dup;
            }
        }

        eprintln!("[bus_cse] -------- first chip with >=50 interactions --------");
        eprintln!("[bus_cse] interactions               : {}", bus_interactions.len());
        eprintln!("[bus_cse] total arg expressions      : {}", total_args);
        eprintln!("[bus_cse] distinct subexpressions    : {}", counts.len());
        eprintln!("[bus_cse] total ops without CSE      : {}", total_ops_no_cse);
        eprintln!(
            "[bus_cse] total ops with perfect CSE : {}",
            total_ops_no_cse - savings
        );
        eprintln!(
            "[bus_cse] reduction (perfect)        : {:.1}%   ({} ops saved)",
            (savings as f64) / (total_ops_no_cse as f64) * 100.0,
            savings
        );
        eprintln!(
            "[bus_cse] reduction (only ops>4)     : {:.1}%   ({} ops saved)",
            (savings_only_multi_op as f64) / (total_ops_no_cse as f64) * 100.0,
            savings_only_multi_op
        );

        let mut top: Vec<_> = counts
            .iter()
            .filter(|(_, &c)| c >= 2)
            .map(|(e, &c)| (c, ops(e), e))
            .collect();
        top.sort_by(|a, b| {
            (b.0 * b.1.saturating_sub(2))
                .cmp(&(a.0 * a.1.saturating_sub(2)))
                .then_with(|| b.0.cmp(&a.0))
        });
        eprintln!("[bus_cse] top-15 reusable subtrees by (count-1)*(ops-2):");
        for (c, o, e) in top.iter().take(15) {
            eprintln!(
                "[bus_cse]   count={:>4} ops={:>3}  saves={:>5}   expr={}",
                c,
                o,
                (c - 1) * o.saturating_sub(2),
                e
            );
        }
    });
}

pub fn compile_bus_to_gpu(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevInteraction>, Vec<ExprSpan>, Vec<u32>) {
    analyze_bus_cse(bus_interactions);

    let mut interactions = Vec::with_capacity(bus_interactions.len());
    let mut arg_spans = Vec::new();
    let mut bytecode = Vec::new();

    for bus_interaction in bus_interactions {
        // multiplicity as first arg span
        let args_index_off = arg_spans.len() as u32;
        let mult_span = emit_expr_span(
            &mut bytecode,
            &bus_interaction.mult,
            apc_poly_id_to_index,
            apc_height,
        );
        arg_spans.push(mult_span);

        // args
        for arg in &bus_interaction.args {
            let span = emit_expr_span(&mut bytecode, arg, apc_poly_id_to_index, apc_height);
            arg_spans.push(span);
        }

        interactions.push(DevInteraction {
            bus_id: (bus_interaction.id as u32),
            num_args: bus_interaction.args.len() as u32,
            args_index_off,
        });
    }

    (interactions, arg_spans, bytecode)
}

pub struct PowdrTraceGeneratorGpu<ISA: OpenVmISA> {
    pub apc: IsaApc<BabyBear, ISA>,
    pub original_airs: OriginalAirs<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesGpu<ISA>,
}

/// Cached `POWDR_TRACE_PROFILE` env var read. When set, `timed_substage!`
/// synchronizes the GPU stream before each span closes so per-stage timings
/// reflect device-side execution. Off by default to avoid serializing host
/// work with GPU work — pure-launch stages (`tracegen_kernel`, `bus_kernel`)
/// then show enqueue time only.
fn trace_profile_sync_enabled() -> bool {
    static ENABLED: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var("POWDR_TRACE_PROFILE").is_ok())
}

/// Wrap a substage in an INFO `tracing` span so openvm's `TimingMetricsLayer`
/// auto-emits `<name>_time_ms`, with the `group` / `air` labels from the
/// surrounding `app_prove` / `single_trace_gen` spans propagated by
/// `TracingContextLayer`.
macro_rules! timed_substage {
    ($name:literal, $body:expr) => {{
        // Microsecond counter alongside the info_span!. The span goes
        // through TimingMetricsLayer which truncates per-call elapsed via
        // `as_millis() as f64`, so hundreds of <1ms calls (typical at high
        // APC counts where each PowdrAir is small) all round to 0 and the
        // total disappears. The counter accumulates without truncation; the
        // viewer can sum `<name>_us` for an accurate per-substage total.
        let _t0 = ::std::time::Instant::now();
        let r = {
            let _span = ::tracing::info_span!($name).entered();
            let inner = $body;
            if crate::powdr_extension::trace_generator::cuda::trace_profile_sync_enabled() {
                let _ = ::openvm_cuda_common::stream::current_stream_sync();
            }
            inner
        };
        ::metrics::counter!(concat!($name, "_us"))
            .increment(_t0.elapsed().as_micros() as u64);
        r
    }};
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorGpu<ISA> {
    pub fn new(
        apc: IsaApc<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesGpu<ISA>,
    ) -> Self {
        Self {
            apc,
            original_airs,
            config,
            periphery,
        }
    }

    fn try_generate_witness(
        &self,
        original_arenas: OriginalArenas<DenseRecordArena>,
    ) -> Option<DeviceMatrix<BabyBear>> {
        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                // if the arenas are uninitialized, the apc was not called, so we return early
                return None;
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        let chip_inventory: ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend> =
            timed_substage!("dummy_chip_inventory", {
                let airs =
                    ISA::create_dummy_airs(self.config.config(), self.periphery.dummy.clone())
                        .expect("Failed to create dummy airs");

                ISA::create_dummy_chip_complex_gpu(
                    self.config.config(),
                    airs,
                    self.periphery.dummy.clone(),
                )
                .expect("Failed to create chip complex")
                .inventory
            });

        let dummy_trace_by_air_name: HashMap<String, DeviceMatrix<BabyBear>> =
            timed_substage!("dummy_trace_gen", {
                chip_inventory
                    .chips()
                    .iter()
                    .enumerate()
                    .rev()
                    .filter_map(|(insertion_idx, chip)| {
                        let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                        let record_arena = {
                            match original_arenas.take_real_arena(&air_name) {
                                Some(ra) => ra,
                                None => return None, // skip this iteration, because we only have record arena for chips that are used
                            }
                        };

                        // We might have initialized an arena for an AIR which ends up having no real records. It gets filtered out here.
                        let ctx = chip.generate_proving_ctx(record_arena);
                        let m = ctx.common_main;
                        use openvm_stark_backend::prover::MatrixDimensions;
                        if m.height() > 0 {
                            Some((air_name, m))
                        } else {
                            None
                        }
                    })
                    .collect()
            });

        // Map from apc poly id to its index in the final apc trace
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // allocate for apc trace (zero-initialized so columns not covered
        // by substitutions or derived expressions default to zero, matching the CPU path)
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, width);
        output.buffer().fill_zero().unwrap();

        // Prepare `OriginalAir` and `Subst` arrays
        let (airs, substitutions) = {
            self.apc
                // go through original instructions
                .instructions()
                // along with their substitutions
                .zip_eq(self.apc.subs())
                // map to `(air_name, substitutions)`
                .filter_map(|(instr, subs)| {
                    if subs.is_empty() {
                        None
                    } else {
                        Some((&self.original_airs.opcode_to_air[&instr.inner.opcode], subs))
                    }
                })
                // group by air name. This results in `HashMap<air_name, Vec<subs>>` where the length of the vector is the number of rows which are created in this air, per apc call
                .into_group_map()
                // go through each air and its substitutions
                .iter()
                .enumerate()
                .fold(
                    (Vec::new(), Vec::new()),
                    |(mut airs, mut substitutions), (air_index, (air_name, subs_by_row))| {
                        // Find the substitutions that map to an apc column
                        let new_substitutions: Vec<Subst> = subs_by_row
                            .iter()
                            // enumerate over them to get the row index inside the air block
                            .enumerate()
                            .flat_map(|(row, subs)| {
                                // for each substitution, map to `Subst` struct
                                subs.iter()
                                    .map(move |sub| (row, sub))
                                    .map(|(row, sub)| Subst {
                                        air_index: air_index as i32,
                                        col: sub.original_poly_index as i32,
                                        row: row as i32,
                                        apc_col: apc_poly_id_to_index[&sub.apc_poly_id] as i32,
                                    })
                            })
                            .collect();

                        // get the device dummy trace for this air
                        let dummy_trace = &dummy_trace_by_air_name[*air_name];

                        use openvm_stark_backend::prover::MatrixDimensions;
                        airs.push(OriginalAir {
                            width: dummy_trace.width() as i32,
                            height: dummy_trace.height() as i32,
                            buffer: dummy_trace.buffer().as_ptr(),
                            row_block_size: subs_by_row.len() as i32,
                        });

                        substitutions.extend(new_substitutions);

                        (airs, substitutions)
                    },
                )
        };

        // Send the airs and substitutions to device
        let (airs, substitutions) = timed_substage!("tracegen_h2d", {
            (
                airs.to_device().unwrap(),
                substitutions.to_device().unwrap(),
            )
        });

        timed_substage!("tracegen_kernel", {
            cuda_abi::apc_tracegen(&mut output, airs, substitutions, num_apc_calls).unwrap();
        });

        // Apply derived columns using the GPU expression evaluator
        timed_substage!("derived", {
            let (derived_specs, derived_bc) = compile_derived_to_gpu(
                &self.apc.machine.derived_columns,
                &apc_poly_id_to_index,
                height,
            );
            // In practice `d_specs` is never empty, because we will always have `is_valid`
            let d_specs = derived_specs.to_device().unwrap();
            let d_bc = derived_bc.to_device().unwrap();
            cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, num_apc_calls).unwrap();
        });

        // Encode bus interactions for GPU consumption
        let (bus_interactions, arg_spans, bytecode) = timed_substage!("bus_compile_h2d", {
            let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
                &self.apc.machine.bus_interactions,
                &apc_poly_id_to_index,
                height,
            );
            (
                bus_interactions.to_device().unwrap(),
                arg_spans.to_device().unwrap(),
                bytecode.to_device().unwrap(),
            )
        });

        // Gather GPU inputs for periphery (bus ids, count device buffers)
        let periphery = &self.periphery.real;

        // Range checker
        let var_range_bus_id = self.periphery.bus_ids.range_checker as u32;
        let var_range_count = &periphery.range_checker.count;

        // Tuple checker
        let tuple_range_checker_chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = self.periphery.bus_ids.tuple_range_checker.unwrap() as u32;
        let tuple2_sizes = tuple_range_checker_chip.sizes;
        let tuple2_count_u32 = tuple_range_checker_chip.count.as_ref();

        // Bitwise lookup; NUM_BITS is fixed at 8 in CUDA
        let bitwise_bus_id = self.periphery.bus_ids.bitwise_lookup.unwrap() as u32;
        let bitwise_count_u32 = periphery.bitwise_lookup_8.as_ref().unwrap().count.as_ref();

        // Launch GPU apply-bus to update periphery histograms on device
        // Note that this is implicitly serialized after `apc_tracegen`,
        // because we use the default host to device stream, which only launches
        // the next kernel function after the prior (`apc_tracegen`) returns.
        // This is important because bus evaluation depends on trace results.
        timed_substage!("bus_kernel", {
            cuda_abi::apc_apply_bus(
                // APC related
                &output,
                num_apc_calls,
                // Interaction related
                bytecode,
                bus_interactions,
                arg_spans,
                // Variable range checker related
                var_range_bus_id,
                var_range_count,
                // Tuple range checker related
                tuple2_bus_id,
                tuple2_count_u32,
                tuple2_sizes,
                // Bitwise related
                bitwise_bus_id,
                bitwise_count_u32,
            )
            .unwrap();
        });

        Some(output)
    }
}

impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipGpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self
            .trace_generator
            .try_generate_witness(self.record_arena_by_air_name.take())
            .unwrap_or_else(DeviceMatrix::dummy);

        AirProvingContext {
            cached_mains: vec![],
            common_main: trace,
            public_values: vec![],
        }
    }
}
