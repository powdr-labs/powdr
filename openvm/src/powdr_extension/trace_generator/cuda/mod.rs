use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use itertools::Itertools;
use openvm_circuit::{
    arch::{AirInventory, DenseRecordArena},
    utils::next_power_of_two_or_zero,
};
use openvm_cuda_backend::base::DeviceMatrix;
use openvm_cuda_common::copy::MemCopyH2D;
use openvm_stark_backend::{
    p3_field::PrimeField32,
    prover::{hal::ProverBackend, types::AirProvingContext},
    Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    Apc, SymbolicBusInteraction,
};
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use crate::{
    bus_map::DEFAULT_TUPLE_RANGE_CHECKER,
    cuda_abi::{self, DerivedExprSpec, DevArgSpan, DevInteraction, OpCode, OriginalAir, Subst},
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    powdr_extension::{
        chip::PowdrChipGpu,
        executor::OriginalArenas,
        trace_generator::{common::create_dummy_airs, cuda::inventory::create_dummy_chip_complex},
    },
    BabyBearSC, Instr,
};

mod inventory;
mod periphery;

pub use periphery::PowdrPeripheryInstancesGpu;

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

fn emit_dev_arg_span(
    bc: &mut Vec<u32>,
    expr: &AlgebraicExpression<BabyBear>,
    id_to_apc_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> DevArgSpan {
    let off = bc.len() as u32;
    emit_expr(bc, expr, id_to_apc_index, apc_height);
    let len = (bc.len() as u32) - off;
    DevArgSpan { off, len }
}

/// Compile derived columns to GPU bytecode according to input order.
fn compile_derived_to_gpu(
    derived_columns: &[(
        AlgebraicReference,
        ComputationMethod<BabyBear, AlgebraicExpression<BabyBear>>,
    )],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DerivedExprSpec>, Vec<u32>) {
    let mut specs = Vec::with_capacity(derived_columns.len());
    let mut bytecode = Vec::new();

    for (col, computation_method) in derived_columns {
        let apc_col_index = apc_poly_id_to_index[&col.id];
        let off = bytecode.len() as u32;
        match computation_method {
            ComputationMethod::Constant(c) => {
                // Encode constant as an expression
                bytecode.push(OpCode::PushConst as u32);
                bytecode.push(c.as_canonical_u32());
            }
            ComputationMethod::InverseOrZero(expr) => {
                // Encode inner expression, then apply InvOrZero
                emit_expr(&mut bytecode, expr, apc_poly_id_to_index, apc_height);
                bytecode.push(OpCode::InvOrZero as u32);
            }
        }
        let len = (bytecode.len() as u32) - off;
        specs.push(DerivedExprSpec {
            col_base: (apc_col_index * apc_height) as u64,
            span: DevArgSpan { off, len },
        });
    }

    (specs, bytecode)
}

pub fn compile_bus_to_gpu(
    bus_interactions: &[SymbolicBusInteraction<BabyBear>],
    apc_poly_id_to_index: &BTreeMap<u64, usize>,
    apc_height: usize,
) -> (Vec<DevInteraction>, Vec<DevArgSpan>, Vec<u32>) {
    let mut interactions = Vec::with_capacity(bus_interactions.len());
    let mut arg_spans = Vec::new();
    let mut bytecode = Vec::new();

    for bus_interaction in bus_interactions {
        // multiplicity as first arg span
        let args_index_off = arg_spans.len() as u32;
        let mult_span = emit_dev_arg_span(
            &mut bytecode,
            &bus_interaction.mult,
            apc_poly_id_to_index,
            apc_height,
        );
        arg_spans.push(mult_span);

        // args
        for arg in &bus_interaction.args {
            let span = emit_dev_arg_span(&mut bytecode, arg, apc_poly_id_to_index, apc_height);
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

pub struct PowdrTraceGeneratorGpu {
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    pub original_airs: OriginalAirs<BabyBear>,
    pub config: OriginalVmConfig,
    pub periphery: PowdrPeripheryInstancesGpu,
}

impl PowdrTraceGeneratorGpu {
    pub fn new(
        apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
        original_airs: OriginalAirs<BabyBear>,
        config: OriginalVmConfig,
        periphery: PowdrPeripheryInstancesGpu,
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
        mut original_arenas: OriginalArenas<DenseRecordArena>,
    ) -> Option<DeviceMatrix<BabyBear>> {
        let num_apc_calls = original_arenas.number_of_calls();

        if num_apc_calls == 0 {
            // If the APC isn't called, early return with an empty trace.
            return None;
        }

        let chip_inventory = {
            let airs: AirInventory<BabyBearSC> =
                create_dummy_airs(&self.config.sdk_config.sdk, self.periphery.dummy.clone())
                    .expect("Failed to create dummy airs");

            create_dummy_chip_complex(
                &self.config.sdk_config.sdk,
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let arenas = original_arenas.arenas_mut();

        let dummy_trace_by_air_name: HashMap<String, DeviceMatrix<BabyBear>> = chip_inventory
            .chips()
            .iter()
            .enumerate()
            .rev()
            .filter_map(|(insertion_idx, chip)| {
                let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

                let record_arena = {
                    match arenas.remove(&air_name) {
                        Some(ra) => ra,
                        None => return None, // skip this iteration, because we only have record arena for chips that are used
                    }
                };

                let shared_trace = chip.generate_proving_ctx(record_arena).common_main.unwrap();

                Some((air_name, shared_trace))
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

        // allocate for apc trace
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, width);

        let derived_column_poly_ids = self
            .apc
            .machine
            .derived_columns
            .iter()
            .map(|(column, _)| column.id)
            .collect::<Vec<_>>();

        // Prepare `OriginalAir` and `Subst` arrays
        let (airs, substitutions) = {
            self.apc
                // go through original instructions
                .instructions()
                .iter()
                // along with their substitutions
                .zip_eq(self.apc.subs())
                // map to `(air_name, substitutions)`
                .map(|(instr, subs)| (&self.original_airs.opcode_to_air[&instr.0.opcode], subs))
                // group by air name. This results in `HashMap<air_name, Vec<subs>>` where the length of the vector is the number of rows which are created in this air, per apc call
                .into_group_map()
                // go through each air and its substitutions
                .iter()
                .fold(
                    (Vec::new(), Vec::new()),
                    |(mut airs, mut substitutions), (air_name, subs_by_row)| {
                        // Find the substitutions that map to an apc column
                        let filtered_substitutions: Vec<Subst> = subs_by_row
                            .iter()
                            // enumerate over them to get the row index inside the air block
                            .enumerate()
                            .flat_map(|(row, subs)| {
                                // for each substitution, map to `Subst` struct if it exists in apc
                                subs.iter()
                                    .enumerate()
                                    .filter_map(|(dummy_index, poly_id)| {
                                        // Filter out poly_id of derived columns, because they will be set separately
                                        if derived_column_poly_ids.contains(poly_id) {
                                            return None;
                                        }
                                        // Check if this dummy column is present in the final apc row
                                        apc_poly_id_to_index
                                            .get(poly_id)
                                            // If it is, map the dummy index to the apc index
                                            .map(|apc_index| Subst {
                                                col: dummy_index as i32,
                                                row: row as i32,
                                                apc_col: *apc_index as i32,
                                            })
                                    })
                                    .collect_vec()
                            })
                            // sort by column so that reads to the same column are coalesced, as the table is column major
                            .sorted_by(|left, right| left.col.cmp(&right.col))
                            .collect();

                        // get the device dummy trace for this air
                        let dummy_trace = &dummy_trace_by_air_name[*air_name];

                        use openvm_stark_backend::prover::hal::MatrixDimensions;
                        airs.push(OriginalAir {
                            width: dummy_trace.width() as i32,
                            height: dummy_trace.height() as i32,
                            buffer: dummy_trace.buffer().as_ptr(),
                            row_block_size: subs_by_row.len() as i32,
                            substitutions_offset: substitutions.len() as i32,
                            substitutions_length: filtered_substitutions.len() as i32,
                        });

                        substitutions.extend(filtered_substitutions);

                        (airs, substitutions)
                    },
                )
        };

        // Send the airs and substitutions to device
        let airs = airs.to_device().unwrap();
        let substitutions = substitutions.to_device().unwrap();

        cuda_abi::apc_tracegen(&mut output, airs, substitutions, num_apc_calls).unwrap();

        // Apply derived columns using the GPU expression evaluator
        let (derived_specs, derived_bc) = compile_derived_to_gpu(
            &self.apc.machine.derived_columns,
            &apc_poly_id_to_index,
            height,
        );
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
        let var_range_bus_id = periphery
            .range_checker
            .cpu_chip
            .as_ref()
            .unwrap()
            .bus()
            .index() as u32;
        let var_range_count = &periphery.range_checker.count;

        // Tuple checker
        let chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = DEFAULT_TUPLE_RANGE_CHECKER as u32;
        let tuple2_sizes = chip.sizes;
        let tuple2_count_u32 = chip.count.as_ref();

        // Bitwise lookup; NUM_BITS is fixed at 8 in CUDA
        let chip = periphery.bitwise_lookup_8.as_ref().unwrap();
        let bitwise_bus_id = chip.cpu_chip.as_ref().unwrap().bus().inner.index as u32;
        let bitwise_count_u32 = chip.count.as_ref();

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

        Some(output)
    }
}

impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>> Chip<R, PB> for PowdrChipGpu {
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let trace = self
            .trace_generator
            .try_generate_witness(self.record_arena_by_air_name.take());

        AirProvingContext::new(vec![], trace, vec![])
    }
}
