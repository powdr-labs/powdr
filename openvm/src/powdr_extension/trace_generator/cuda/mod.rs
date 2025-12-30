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
    ApcTracingContext, Chip,
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicExpression, AlgebraicReference},
    Apc, Substitution, SymbolicBusInteraction,
};
use powdr_constraint_solver::constraint_system::ComputationMethod;
use powdr_expression::{AlgebraicBinaryOperator, AlgebraicUnaryOperator};

use crate::{
    cuda_abi::{self, DerivedExprSpec, DevInteraction, ExprSpan, OpCode},
    customize_exe::OpenVmRegisterAddress,
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

/// Data for direct-to-APC trace generation for a single AIR.
///
/// This struct holds the substitution mappings and layout information needed
/// to copy trace values from original AIR columns to APC columns on the GPU.
struct DirectAirData<'a> {
    /// Substitution mappings for each instruction in this AIR.
    /// Each inner `Vec<Substitution>` maps original AIR column indices to APC column IDs
    /// for one instruction.
    subs: Vec<&'a Vec<Substitution>>,
    /// Number of substitutions (post-optimization columns) for each original instruction.
    /// Used to compute the destination offset in the APC trace.
    opt_widths: Vec<u32>,
    /// Cumulative offset into the post-optimization column layout for each instruction.
    /// Used to compute the destination offset in the APC trace.
    post_opt_offsets: Vec<u32>,
}

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

pub struct PowdrTraceGeneratorGpu {
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
    pub original_airs: OriginalAirs<BabyBear>,
    pub config: OriginalVmConfig,
    pub periphery: PowdrPeripheryInstancesGpu,
}

impl PowdrTraceGeneratorGpu {
    pub fn new(
        apc: Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
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

        // Get unique AIR names with widths for instructions that have substitutions
        // (only for AIRs in air_name_to_machine). BTreeMap naturally deduplicates by key.
        let active_direct_airs: std::collections::BTreeMap<&str, usize> = self
            .apc
            .instructions()
            .iter()
            .zip_eq(self.apc.subs())
            .filter_map(|(instr, subs)| {
                if subs.is_empty() {
                    None
                } else {
                    let air_name = &self.original_airs.opcode_to_air[&instr.0.opcode];
                    self.original_airs
                        .air_name_to_machine
                        .get(air_name)
                        .map(|machine| (air_name.as_str(), machine.1.widths.main))
                }
            })
            .collect();

        // Generate substitution data for each active direct-to-APC AIR
        let direct_air_data: HashMap<&str, DirectAirData<'_>> = active_direct_airs
            .iter()
            .map(|(&target_air_name, &air_width)| {
                let result = self
                    .apc
                    .instructions()
                    .iter()
                    .zip_eq(self.apc.subs())
                    .filter_map(|(instr, subs)| {
                        if subs.is_empty() {
                            None
                        } else {
                            Some((instr, subs))
                        }
                    })
                    .fold(
                        (Vec::new(), Vec::new(), Vec::new(), 0u32, 0u32),
                        |(
                            mut subs,
                            mut opt_widths,
                            mut post_opt_widths,
                            mut opt_width_acc,
                            mut post_opt_width_acc,
                        ),
                         (instr, sub)| {
                            let air_name = &self.original_airs.opcode_to_air[&instr.0.opcode];
                            if air_name == target_air_name {
                                subs.push(sub);
                                opt_widths.push(sub.len() as u32);
                                post_opt_widths.push(post_opt_width_acc);
                                opt_width_acc += air_width as u32;
                            }
                            post_opt_width_acc += sub.len() as u32;
                            (
                                subs,
                                opt_widths,
                                post_opt_widths,
                                opt_width_acc,
                                post_opt_width_acc,
                            )
                        },
                    );
                (
                    target_air_name,
                    DirectAirData {
                        subs: result.0,
                        opt_widths: result.1,
                        post_opt_offsets: result.2,
                    },
                )
            })
            .collect();

        // Extract calls_per_apc_row for each AIR (HashMap)
        let direct_calls_per_apc_row: HashMap<&str, u32> = direct_air_data
            .iter()
            .map(|(&air_name, data)| (air_name, data.subs.len() as u32))
            .collect();

        // Pad substitutions for each AIR and copy to device (HashMap)
        let direct_d_subs: HashMap<&str, _> = direct_air_data
            .iter()
            .map(|(&air_name, data)| {
                let air_width = active_direct_airs[air_name];
                let padded_subs: Vec<u32> = data
                    .subs
                    .iter()
                    .flat_map(|subs_for_row| {
                        let mut row = vec![u32::MAX; air_width];
                        for sub in subs_for_row.iter() {
                            assert!(
                                sub.original_poly_index < air_width,
                                "substitution index {} exceeds {} width ({})",
                                sub.original_poly_index,
                                air_name,
                                air_width
                            );
                            let apc_col = apc_poly_id_to_index[&sub.apc_poly_id];
                            row[sub.original_poly_index] =
                                u32::try_from(apc_col).expect("APC column index fits in u32");
                        }
                        row.into_iter()
                    })
                    .collect();
                let d_subs = padded_subs.to_device().unwrap_or_else(|_| {
                    panic!("Failed to copy {} substitutions to device", air_name)
                });
                (air_name, d_subs)
            })
            .collect();

        let direct_d_opt_widths: HashMap<&str, _> = direct_air_data
            .iter()
            .map(|(&air_name, data)| {
                let d_opt_widths =
                    data.opt_widths.clone().to_device().unwrap_or_else(|_| {
                        panic!("Failed to copy {} opt widths to device", air_name)
                    });
                (air_name, d_opt_widths)
            })
            .collect();

        let direct_d_post_opt_offsets: HashMap<&str, _> = direct_air_data
            .iter()
            .map(|(&air_name, data)| {
                let d_post_opt_offsets =
                    data.post_opt_offsets
                        .clone()
                        .to_device()
                        .unwrap_or_else(|_| {
                            panic!("Failed to copy {} post opt offsets to device", air_name)
                        });
                (air_name, d_post_opt_offsets)
            })
            .collect();

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

        // Generate traces for all chips using direct-to-APC path
        for (insertion_idx, chip) in chip_inventory.chips().iter().enumerate().rev() {
            let air_name = chip_inventory.airs().ext_airs()[insertion_idx].name();

            let record_arena = match original_arenas.take_real_arena(&air_name) {
                Some(ra) => ra,
                None => continue, // skip chips without records
            };

            // Check if AIR is in the active APC
            let d_subs = match direct_d_subs.get(air_name.as_str()) {
                Some(subs) => subs,
                None => {
                    // AIR has records but is not in the active APC - skip if it's a known AIR
                    assert!(
                        self.original_airs.air_name_to_machine.contains_key(&air_name),
                        "AIR '{}' is not in air_name_to_machine. All chips must implement generate_proving_ctx_direct.",
                        air_name
                    );
                    continue;
                }
            };

            let ctx = ApcTracingContext::new(
                output.buffer(),
                d_subs,
                &direct_d_opt_widths[air_name.as_str()],
                &direct_d_post_opt_offsets[air_name.as_str()],
                direct_calls_per_apc_row[air_name.as_str()],
                height,
                width,
            );
            chip.generate_proving_ctx_direct(record_arena, Some(&ctx));
        }

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
        // Note: kernels are serialized via the default stream, so this runs after
        // all chip trace generation and derived expression kernels complete.
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
