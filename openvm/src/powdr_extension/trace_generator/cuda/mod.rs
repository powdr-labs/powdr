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
/// Column references are delegated to `emit_ref`, which encodes them for the target kernel:
/// the bus evaluator reads committed APC columns (`PushApc`), while the derived-column evaluator
/// reads inputs straight from the original (dummy) AIR traces (`PushDummy`).
/// Constants are encoded as `PushConst` followed by the field element as `u32`.
/// Unary minus and binary operations map to `Neg`, `Add`, `Sub`, and `Mul`.
///
/// Note: This function does not track or enforce the evaluation stack depth,
/// which is done in device code.
fn emit_expr(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    emit_ref: &dyn Fn(&mut Vec<u32>, u64),
) {
    match expr {
        AlgebraicExpression::Number(c) => {
            bc.push(OpCode::PushConst as u32);
            bc.push(c.as_canonical_u32());
        }
        AlgebraicExpression::Reference(r) => emit_ref(bc, r.id),
        AlgebraicExpression::UnaryOperation(u) => {
            emit_expr(bc, &u.expr, emit_ref);
            match u.op {
                AlgebraicUnaryOperator::Minus => bc.push(OpCode::Neg as u32),
            }
        }
        AlgebraicExpression::BinaryOperation(b) => {
            emit_expr(bc, &b.left, emit_ref);
            emit_expr(bc, &b.right, emit_ref);
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
    emit_ref: &dyn Fn(&mut Vec<u32>, u64),
) -> ExprSpan {
    // The span starts where the bytecode currently ends
    let off = bc.len() as u32;
    // Append the bytecode for `expr`
    emit_expr(bc, expr, emit_ref);
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
    emit_ref: &dyn Fn(&mut Vec<u32>, u64),
) {
    match method {
        ComputationMethod::Constant(c) => {
            bytecode.push(OpCode::PushConst as u32);
            bytecode.push(c.as_canonical_u32());
        }
        ComputationMethod::QuotientOrZero(e1, e2) => {
            // Invert denominator (or use zero), then multiply with numerator.
            emit_expr(bytecode, e2, emit_ref);
            bytecode.push(OpCode::InvOrZero as u32);
            emit_expr(bytecode, e1, emit_ref);
            bytecode.push(OpCode::Mul as u32);
        }
        ComputationMethod::IfEqZero(cond, then_method, else_method) => {
            emit_expr(bytecode, cond, emit_ref);
            bytecode.push(OpCode::JmpIfNonzero as u32);
            let else_target_operand = bytecode.len();
            bytecode.push(0); // patched below to the `else`-block offset
            emit_method(bytecode, off, then_method, emit_ref);
            bytecode.push(OpCode::Jmp as u32);
            let end_target_operand = bytecode.len();
            bytecode.push(0); // patched below to the `end` offset
                              // `else` block begins here (after the unconditional-jump operand).
            bytecode[else_target_operand] = (bytecode.len() - off) as u32;
            emit_method(bytecode, off, else_method, emit_ref);
            bytecode[end_target_operand] = (bytecode.len() - off) as u32;
        }
    }
}

/// Compile derived columns to GPU bytecode according to input order.
///
/// A derived column's method may reference both surviving APC columns and the "removed" columns the
/// optimizer substituted out of the circuit. Both are read straight from the original (dummy) AIR
/// traces via `PushDummy`, using the `(air_index, col, row)` location recorded in `subs_by_id` for
/// every substituted column, so no referenced column needs to be staged in the committed APC buffer.
/// The write target of each new column is a committed column, looked up in `apc_poly_id_to_index`.
fn compile_derived_to_gpu(
    derived_columns: &[DerivedVariable<
        BabyBear,
        AlgebraicReference,
        AlgebraicExpression<BabyBear>,
    >],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    subs_by_id: &BTreeMap<u64, Subst>,
    apc_height: usize,
) -> (Vec<DerivedExprSpec>, Vec<u32>) {
    let emit_ref = |bc: &mut Vec<u32>, id: u64| {
        let sub = &subs_by_id[&id];
        bc.push(OpCode::PushDummy as u32);
        bc.push(sub.air_index as u32);
        bc.push(sub.col as u32);
        bc.push(sub.row as u32);
    };

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
        let apc_col_index = apc_poly_id_to_index[&variable.id];
        let off = bytecode.len();
        emit_method(&mut bytecode, off, computation_method, &emit_ref);
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

    // Bus interactions only reference surviving (committed) APC columns, read from the APC buffer
    // via `PushApc` at column-major offset `apc_col_index * apc_height`.
    let emit_ref = |bc: &mut Vec<u32>, id: u64| {
        let idx = (apc_poly_id_to_index[&id] * apc_height) as u32;
        bc.push(OpCode::PushApc as u32);
        bc.push(idx);
    };

    for bus_interaction in bus_interactions {
        // multiplicity as first arg span
        let args_index_off = arg_spans.len() as u32;
        let mult_span = emit_expr_span(&mut bytecode, &bus_interaction.mult, &emit_ref);
        arg_spans.push(mult_span);

        // args
        for arg in &bus_interaction.args {
            let span = emit_expr_span(&mut bytecode, arg, &emit_ref);
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

        // allocate for apc trace (zero-initialized so columns not covered
        // by substitutions or derived expressions default to zero, matching the CPU path).
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut output =
            DeviceMatrix::<BabyBear>::with_capacity(height, apc_poly_id_to_index.len());
        output.buffer().fill_zero().unwrap();

        // Prepare `OriginalAir` metadata and, per `apc_poly_id`, the dummy-trace location of every
        // substituted column as a `Subst` (`air_index`, `col`, `row`) plus its destination committed
        // column `apc_col` — or `apc_col = -1` for a "removed" column the optimizer substituted out
        // of the circuit (absent from `main_columns()`), which has no committed slot. A derived
        // column's method may reference either kind; the derived-expression evaluator reads them
        // straight from the dummy traces via `PushDummy`, so nothing is staged into the committed
        // buffer. The surviving subset (`apc_col >= 0`) is copied into the trace by `apc_tracegen`.
        // (Any sub whose target is absent from `main_columns()` is, by construction of `Apc::new`,
        // exactly such a removed-but-referenced column.)
        let mut subs_by_id: BTreeMap<u64, Subst> = BTreeMap::new();
        let airs = self
            .apc
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
                Vec::new(),
                |mut airs, (air_index, (air_name, subs_by_row))| {
                    subs_by_row
                        .iter()
                        // enumerate over them to get the row index inside the air block
                        .enumerate()
                        .for_each(|(row, subs)| {
                            for sub in subs.iter() {
                                let apc_col = apc_poly_id_to_index
                                    .get(&sub.apc_poly_id)
                                    .map_or(-1, |&i| i as i32);
                                subs_by_id.insert(
                                    sub.apc_poly_id,
                                    Subst {
                                        air_index: air_index as i32,
                                        col: sub.original_poly_index as i32,
                                        row: row as i32,
                                        apc_col,
                                    },
                                );
                            }
                        });

                    // get the device dummy trace for this air
                    let dummy_trace = &dummy_trace_by_air_name[*air_name];

                    use openvm_stark_backend::prover::MatrixDimensions;
                    airs.push(OriginalAir {
                        width: dummy_trace.width() as i32,
                        height: dummy_trace.height() as i32,
                        buffer: dummy_trace.buffer().as_ptr(),
                        row_block_size: subs_by_row.len() as i32,
                    });

                    airs
                },
            );

        // Only surviving columns (those with a committed slot) are copied into the trace.
        let substitutions: Vec<Subst> = subs_by_id
            .values()
            .filter(|s| s.apc_col >= 0)
            .copied()
            .collect();

        // Send the airs and substitutions to device. `airs` is shared with the derived-expression
        // kernel below (which reads removed/surviving columns from the dummy traces), so the backing
        // `dummy_trace_by_air_name` buffers must outlive both launches — they do (owned locally).
        let airs = airs.to_device().unwrap();
        let substitutions = substitutions.to_device().unwrap();

        cuda_abi::apc_tracegen(&mut output, &airs, substitutions, num_apc_calls).unwrap();

        // Apply derived columns using the GPU expression evaluator, reading inputs directly from the
        // dummy traces (`airs`) so no removed column is staged in the committed buffer.
        let (derived_specs, derived_bc) = compile_derived_to_gpu(
            &self.apc.machine.derived_columns,
            &apc_poly_id_to_index,
            &subs_by_id,
            height,
        );
        // In practice `d_specs` is never empty, because we will always have `is_valid`
        let d_specs = derived_specs.to_device().unwrap();
        let d_bc = derived_bc.to_device().unwrap();
        cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, &airs, num_apc_calls).unwrap();

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

        // `output` is exactly the committed trace: surviving columns filled by `apc_tracegen`,
        // derived columns filled by `apc_apply_derived_expr`. Removed columns are never staged here
        // (the derived evaluator reads them from the dummy traces), so no trailing scratch to drop
        // and no extra copy.
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
