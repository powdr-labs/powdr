use std::collections::{BTreeMap, HashMap};

use itertools::Itertools;
use openvm_circuit::{
    arch::{ChipInventory, DenseRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_circuit_primitives::Chip;
use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::{copy::cuda_memcpy, copy::MemCopyH2D};
use openvm_stark_backend::{
    p3_field::PrimeField32,
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
            emit_expr(bc, &u.expr, id_to_apc_index, apc_height);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
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

/// Compile one `ComputationMethod` into stack-machine bytecode appended to `bytecode`, recursing
/// through nested `IfEqZero` methods. `off` is the absolute start of the enclosing derived column's
/// span; jump targets are emitted relative to it (the VM indexes each expression from its span
/// start). `IfEqZero(cond, then, else)` lowers to short-circuit control flow so that, per row, only
/// the taken branch executes:
///   «cond»  JMP_IF_NONZERO else  «then»  JMP end  else: «else»  end:
/// (The trees are deep — up to ~16 nested in practice — so a branchless `cond*then + (1-cond)*else`
/// select would evaluate the whole tree every row and overflow the evaluator's fixed stack.)
fn emit_method(
    bytecode: &mut Vec<u32>,
    off: usize,
    method: &ComputationMethod<BabyBear, AlgebraicExpression<BabyBear>>,
    id_to_slot: &BTreeMap<u64, usize>,
    apc_height: usize,
) {
    match method {
        ComputationMethod::Constant(c) => {
            bytecode.push(OpCode::PushConst as u32);
            bytecode.push(c.as_canonical_u32());
        }
        ComputationMethod::QuotientOrZero(e1, e2) => {
            // Invert denominator (or use zero), then multiply with numerator.
            emit_expr(bytecode, e2, id_to_slot, apc_height);
            bytecode.push(OpCode::InvOrZero as u32);
            emit_expr(bytecode, e1, id_to_slot, apc_height);
            bytecode.push(OpCode::Mul as u32);
        }
        ComputationMethod::IfEqZero(cond, then_method, else_method) => {
            emit_expr(bytecode, cond, id_to_slot, apc_height);
            bytecode.push(OpCode::JmpIfNonzero as u32);
            let else_target_operand = bytecode.len();
            bytecode.push(0); // patched below to the `else`-block offset
            emit_method(bytecode, off, then_method, id_to_slot, apc_height);
            bytecode.push(OpCode::Jmp as u32);
            let end_target_operand = bytecode.len();
            bytecode.push(0); // patched below to the `end` offset
                              // `else` block begins here (after the unconditional-jump operand).
            bytecode[else_target_operand] = (bytecode.len() - off) as u32;
            emit_method(bytecode, off, else_method, id_to_slot, apc_height);
            bytecode[end_target_operand] = (bytecode.len() - off) as u32;
        }
    }
}

/// Compile derived columns to GPU bytecode according to input order. `id_to_slot` maps every poly
/// id a derived method may reference — both surviving APC columns and the "removed" columns the
/// optimizer substituted out (materialized into scratch slots of the trace buffer) — to its column
/// index, so `emit_expr`'s `PushApc` reads either uniformly.
fn compile_derived_to_gpu(
    derived_columns: &[DerivedVariable<
        BabyBear,
        AlgebraicReference,
        AlgebraicExpression<BabyBear>,
    >],
    id_to_slot: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DerivedExprSpec>, Vec<u32>) {
    let mut specs = Vec::with_capacity(derived_columns.len());
    let mut bytecode = Vec::new();

    for DerivedVariable {
        is_new,
        variable,
        computation_method,
    } in derived_columns
    {
        if !is_new {
            continue;
        }
        let apc_col_index = id_to_slot[&variable.id];
        let off = bytecode.len();
        emit_method(
            &mut bytecode,
            off,
            computation_method,
            id_to_slot,
            apc_height,
        );
        let len = (bytecode.len() - off) as u32;
        specs.push(DerivedExprSpec {
            col_base: (apc_col_index * apc_height) as u64,
            span: ExprSpan {
                off: off as u32,
                len,
            },
        });
    }

    (specs, bytecode)
}

pub fn compile_bus_to_gpu(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevInteraction>, Vec<ExprSpan>, Vec<u32>) {
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

        let chip_inventory: ChipInventory<BabyBearSC, DenseRecordArena, GpuBackend> = {
            let airs = ISA::create_dummy_airs(self.config.config(), self.periphery.dummy.clone())
                .expect("Failed to create dummy airs");

            ISA::create_dummy_chip_complex_gpu(
                self.config.config(),
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let dummy_trace_by_air_name: HashMap<String, DeviceMatrix<BabyBear>> = chip_inventory
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
            .collect();

        // Map from apc poly id to its index in the final apc trace
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // A derived column's computation method may reference a column the optimizer substituted
        // out of the circuit (absent from `main_columns()`), whose value still lives in the
        // original instruction trace. We give each such "removed" column a scratch slot appended
        // after the committed columns and fill it exactly like a surviving column (via a `Subst`);
        // the derived-expression VM then reads it through the ordinary `PushApc`. The scratch slots
        // are dropped before the trace is committed (see the end of this function). In the native
        // optimizer path there are none, so `id_to_slot == apc_poly_id_to_index` and nothing below
        // changes. (Any retained sub whose target is absent from `main_columns()` is, by
        // construction of `Apc::new`, exactly such a removed-but-referenced column.)
        let committed_width = apc_poly_id_to_index.len();
        let mut id_to_slot = apc_poly_id_to_index.clone();
        for subs in self.apc.subs() {
            for sub in subs {
                let next_slot = id_to_slot.len();
                id_to_slot.entry(sub.apc_poly_id).or_insert(next_slot);
            }
        }
        let total_width = id_to_slot.len();

        // allocate for apc trace (zero-initialized so columns not covered
        // by substitutions or derived expressions default to zero, matching the CPU path).
        // Width includes the removed-column scratch slots; only the first `committed_width`
        // columns are committed.
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, total_width);
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
                                        // `id_to_slot` covers both surviving columns and removed
                                        // columns (the latter mapped to scratch slots); a plain
                                        // `apc_poly_id_to_index` lookup would panic on a removed
                                        // sub target.
                                        apc_col: id_to_slot[&sub.apc_poly_id] as i32,
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
        let airs = airs.to_device().unwrap();
        let substitutions = substitutions.to_device().unwrap();

        cuda_abi::apc_tracegen(&mut output, airs, substitutions, num_apc_calls).unwrap();

        // Apply derived columns using the GPU expression evaluator
        let (derived_specs, derived_bc) =
            compile_derived_to_gpu(&self.apc.machine.derived_columns, &id_to_slot, height);
        // In practice `d_specs` is never empty, because we will always have `is_valid`
        let d_specs = derived_specs.to_device().unwrap();
        let d_bc = derived_bc.to_device().unwrap();
        cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, num_apc_calls).unwrap();

        // Encode bus interactions for GPU consumption
        let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
            &self.apc.machine.bus_interactions,
            &apc_poly_id_to_index,
            height,
        );
        let bus_interactions = bus_interactions.to_device().unwrap();
        let arg_spans = arg_spans.to_device().unwrap();
        let bytecode = bytecode.to_device().unwrap();

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

        // Commit only the contiguous column-major prefix [0, committed_width) — the surviving +
        // derived columns. The removed-column scratch slots (if any) live in the tail and are
        // dropped here so the committed trace width matches the APC AIR. `DeviceMatrix` requires
        // its buffer length to equal `height * width`, so we cannot narrow the reported width in
        // place; we copy the prefix into a right-sized matrix. The kernels above and this copy all
        // run on the per-thread default stream (the CUDA build uses `--default-stream=per-thread`),
        // so the copy is serialized after the kernels and before the prover's reads with no
        // explicit synchronization — exactly as the no-scratch fast path returns `output` directly.
        if total_width == committed_width {
            return Some(output);
        }
        let committed = DeviceMatrix::<BabyBear>::with_capacity(height, committed_width);
        unsafe {
            cuda_memcpy::<true, true>(
                committed.buffer().as_mut_raw_ptr(),
                output.buffer().as_raw_ptr(),
                committed_width * height * std::mem::size_of::<BabyBear>(),
            )
        }
        .unwrap();
        Some(committed)
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
