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
pub mod nvrtc_cache;
pub mod nvrtc_emit;
mod periphery;

/// Selects the GPU trace-generation backend for `PowdrChipGpu`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum JitBackend {
    /// Skip JIT entirely; use the original `apc_tracegen` substitution path.
    Off,
    /// Data-driven `apc_jit_tracegen` interpreter (current default JIT).
    Descriptor,
    /// NVRTC-compiled per-APC kernel, with per-APC fall-back to `Descriptor`
    /// when the emitter does not yet support every column computation.
    Nvrtc,
}

/// Resolve the active JIT backend from the environment.
///
/// `POWDR_JIT_BACKEND` takes precedence; values are case-insensitive
/// `off` / `descriptor` / `nvrtc`. For backwards compatibility, when
/// `POWDR_JIT_BACKEND` is unset the legacy `POWDR_JIT_TRACEGEN` flag enables
/// the `Descriptor` backend.
pub fn pick_jit_backend() -> JitBackend {
    if let Ok(v) = std::env::var("POWDR_JIT_BACKEND") {
        return match v.to_ascii_lowercase().as_str() {
            "off" => JitBackend::Off,
            "descriptor" => JitBackend::Descriptor,
            "nvrtc" => JitBackend::Nvrtc,
            other => {
                tracing::warn!(
                    "Unknown POWDR_JIT_BACKEND='{}', defaulting to Descriptor",
                    other
                );
                JitBackend::Descriptor
            }
        };
    }
    if std::env::var("POWDR_JIT_TRACEGEN").is_ok() {
        JitBackend::Descriptor
    } else {
        JitBackend::Off
    }
}

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

        Some(output)
    }

    /// JIT trace generation for GPU: reads DenseRecordArena bytes directly,
    /// computes only surviving APC columns, bypassing full original trace generation.
    fn try_generate_witness_jit(
        &self,
        original_arenas: OriginalArenas<DenseRecordArena>,
    ) -> Result<DeviceMatrix<BabyBear>, OriginalArenas<DenseRecordArena>> {
        use super::jit_mapping::{self, ArenaType, ColumnComputation};
        use crate::cuda_abi::{self, JitColumnDesc, JitInstructionDesc};
        use std::collections::BTreeMap;

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                return Ok(DeviceMatrix::dummy());
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        // Build instruction metadata
        let instructions_with_subs: Vec<_> = self
            .apc
            .instructions()
            .zip_eq(self.apc.subs())
            .filter(|(_, subs)| !subs.is_empty())
            .collect();

        // Get AIR names
        let air_names: Vec<String> = instructions_with_subs
            .iter()
            .map(|(instr, _)| self.original_airs.opcode_to_air[&instr.inner.opcode].clone())
            .collect();

        // Build mapping tables for Dense arena type
        let mappings_by_air: std::collections::HashMap<&str, jit_mapping::AirColumnMapping> = {
            let mut m = std::collections::HashMap::new();
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, BaseAluCoreAir<4, 8>",
                jit_mapping::base_alu_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32LoadStoreAdapterAir, LoadStoreCoreAir<4>",
                jit_mapping::loadstore_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, ShiftCoreAir<4, 8>",
                jit_mapping::shift_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32BranchAdapterAir, BranchEqualCoreAir<4>",
                jit_mapping::branch_equal_mapping_for(ArenaType::Dense),
            );
            m
        };

        // Pre-check: all AIR types must have mappings
        for air_name in air_names.iter() {
            if !mappings_by_air.contains_key(air_name.as_str()) {
                tracing::warn!("GPU JIT: no mapping for AIR '{}', falling back", air_name);
                return Err(OriginalArenas::Initialized(original_arenas));
            }
        }

        // Count occurrences per AIR type
        let air_id_occurrences: std::collections::HashMap<&str, usize> =
            air_names.iter().map(|s| s.as_str()).counts();

        // Compute per-instruction offset within its AIR type
        let instruction_offsets: Vec<usize> = air_names
            .iter()
            .scan(
                std::collections::HashMap::<&str, usize>::default(),
                |counts, air_name| {
                    let count = counts.entry(air_name.as_str()).or_default();
                    let current = *count;
                    *count += 1;
                    Some(current)
                },
            )
            .collect();

        // Build APC poly_id to index mapping
        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);

        // Collect arena bytes: take each arena, record its byte offset in the concatenated buffer
        let mut concat_arena_bytes: Vec<u8> = Vec::new();
        let mut arena_offsets: std::collections::HashMap<String, u32> =
            std::collections::HashMap::new();

        for air_name in air_names.iter().unique() {
            if let Some(arena) = original_arenas.take_real_arena(air_name) {
                let bytes = arena.allocated();
                if bytes.is_empty() {
                    continue;
                }
                let offset = concat_arena_bytes.len() as u32;
                concat_arena_bytes.extend_from_slice(bytes);
                arena_offsets.insert(air_name.clone(), offset);
            }
        }

        // Transfer concatenated arena to GPU
        let d_arena = concat_arena_bytes.to_device().unwrap();

        // Build JIT descriptor arrays
        let mut jit_instructions: Vec<JitInstructionDesc> = Vec::new();
        let mut jit_col_descs: Vec<JitColumnDesc> = Vec::new();

        // Derive range_max_bits from range checker count buffer size:
        // num_rows = 1 << (range_max_bits + 1), so range_max_bits = log2(num_rows) - 1
        let range_max_bits = {
            let num_bins = self.periphery.real.range_checker.count.len();
            if num_bins > 1 { (num_bins as f64).log2() as u32 - 1 } else { 29 }
        };

        for (idx, ((air_name, offset), (_, subs))) in air_names
            .iter()
            .zip(instruction_offsets.iter())
            .zip(instructions_with_subs.iter())
            .enumerate()
        {
            let mapping = &mappings_by_air[air_name.as_str()];
            let occurrences = *air_id_occurrences.get(air_name.as_str()).unwrap();

            let arena_offset = *arena_offsets.get(air_name).unwrap_or(&0);
            let record_stride_bytes = jit_mapping::dense_record_stride(mapping.air_name);

            let col_desc_start = jit_col_descs.len() as u32;

            // For each surviving substitution, create a column descriptor
            for sub in subs.iter() {
                let apc_col_index = apc_poly_id_to_index[&sub.apc_poly_id];
                let col_mapping = &mapping.columns[sub.original_poly_index];

                let desc = column_comp_to_jit_desc(
                    &col_mapping.computation,
                    apc_col_index as u16,
                );
                jit_col_descs.push(desc);
            }

            let col_desc_count = jit_col_descs.len() as u32 - col_desc_start;

            jit_instructions.push(JitInstructionDesc {
                arena_offset,
                record_stride: (record_stride_bytes * occurrences) as u32,
                record_offset: (record_stride_bytes * offset) as u32,
                col_desc_start,
                col_desc_count,
            });
        }

        // Upload descriptors to GPU
        let d_instructions = jit_instructions.to_device().unwrap();
        let d_col_descs = jit_col_descs.to_device().unwrap();

        // Allocate output (pre-zeroed)
        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, width);
        output.buffer().fill_zero().unwrap();

        // Launch JIT kernel (replaces Stage 1 + Stage 2)
        cuda_abi::apc_jit_tracegen(
            &mut output,
            &d_arena,
            &d_instructions,
            &d_col_descs,
            num_apc_calls,
            range_max_bits,
        )
        .unwrap();

        // Stage 3: derived expressions + bus interactions (unchanged)
        let (derived_specs, derived_bc) = compile_derived_to_gpu(
            &self.apc.machine.derived_columns,
            &apc_poly_id_to_index,
            height,
        );
        let d_specs = derived_specs.to_device().unwrap();
        let d_bc = derived_bc.to_device().unwrap();
        cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, num_apc_calls).unwrap();

        let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
            &self.apc.machine.bus_interactions,
            &apc_poly_id_to_index,
            height,
        );
        let bus_interactions = bus_interactions.to_device().unwrap();
        let arg_spans = arg_spans.to_device().unwrap();
        let bytecode = bytecode.to_device().unwrap();

        let periphery = &self.periphery.real;
        let var_range_bus_id = self.periphery.bus_ids.range_checker as u32;
        let var_range_count = &periphery.range_checker.count;
        let tuple_range_checker_chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = self.periphery.bus_ids.tuple_range_checker.unwrap() as u32;
        let tuple2_sizes = tuple_range_checker_chip.sizes;
        let tuple2_count_u32 = tuple_range_checker_chip.count.as_ref();
        let bitwise_bus_id = self.periphery.bus_ids.bitwise_lookup.unwrap() as u32;
        let bitwise_count_u32 = periphery.bitwise_lookup_8.as_ref().unwrap().count.as_ref();

        cuda_abi::apc_apply_bus(
            &output,
            num_apc_calls,
            bytecode,
            bus_interactions,
            arg_spans,
            var_range_bus_id,
            var_range_count,
            tuple2_bus_id,
            tuple2_count_u32,
            tuple2_sizes,
            bitwise_bus_id,
            bitwise_count_u32,
        )
        .unwrap();

        Ok(output)
    }

    /// NVRTC trace generation for GPU. Per-APC fallback: returns `Err` with the
    /// arenas if the emitter cannot yet handle every surviving column (Phase 1
    /// supports `DirectU32` only). On success the output trace contains all
    /// surviving columns plus the unchanged derived-expr / bus passes.
    fn try_generate_witness_nvrtc(
        &self,
        original_arenas: OriginalArenas<DenseRecordArena>,
    ) -> Result<DeviceMatrix<BabyBear>, OriginalArenas<DenseRecordArena>> {
        use crate::powdr_extension::trace_generator::cuda::nvrtc_cache::NvrtcKernelCache;
        use crate::powdr_extension::trace_generator::cuda::nvrtc_emit::{
            emit_jit_kernel_source, EmitterColumn, EmitterInput, EmitterInstruction,
        };
        use crate::powdr_extension::trace_generator::jit_mapping::{
            self as jit_mapping, ArenaType, ColumnComputation,
        };

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                return Ok(DeviceMatrix::dummy());
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        let instructions_with_subs: Vec<_> = self
            .apc
            .instructions()
            .zip_eq(self.apc.subs())
            .filter(|(_, subs)| !subs.is_empty())
            .collect();

        let air_names: Vec<String> = instructions_with_subs
            .iter()
            .map(|(instr, _)| self.original_airs.opcode_to_air[&instr.inner.opcode].clone())
            .collect();

        let mappings_by_air: std::collections::HashMap<&str, jit_mapping::AirColumnMapping> = {
            let mut m = std::collections::HashMap::new();
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, BaseAluCoreAir<4, 8>",
                jit_mapping::base_alu_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32LoadStoreAdapterAir, LoadStoreCoreAir<4>",
                jit_mapping::loadstore_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, ShiftCoreAir<4, 8>",
                jit_mapping::shift_mapping_for(ArenaType::Dense),
            );
            m.insert(
                "VmAirWrapper<Rv32BranchAdapterAir, BranchEqualCoreAir<4>",
                jit_mapping::branch_equal_mapping_for(ArenaType::Dense),
            );
            m
        };

        // Pre-check 1: every AIR has a Dense mapping (same as descriptor path).
        for air_name in air_names.iter() {
            if !mappings_by_air.contains_key(air_name.as_str()) {
                tracing::warn!(
                    "GPU NVRTC: no AIR mapping for '{}', falling back",
                    air_name
                );
                return Err(OriginalArenas::Initialized(original_arenas));
            }
        }

        let air_id_occurrences: std::collections::HashMap<&str, usize> =
            air_names.iter().map(|s| s.as_str()).counts();

        let instruction_offsets: Vec<usize> = air_names
            .iter()
            .scan(
                std::collections::HashMap::<&str, usize>::default(),
                |counts, air_name| {
                    let count = counts.entry(air_name.as_str()).or_default();
                    let current = *count;
                    *count += 1;
                    Some(current)
                },
            )
            .collect();

        let apc_poly_id_to_index: BTreeMap<u64, usize> = self
            .apc
            .machine
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(num_apc_calls);

        // Pre-check 2 (Phase 1 emitter is DirectU32-only). Walk surviving subs
        // first to fail fast before any device work; build EmitterInstruction
        // list as we go.
        let mut emitter_instructions: Vec<EmitterInstruction> =
            Vec::with_capacity(instructions_with_subs.len());

        for ((air_name, offset), (_, subs)) in air_names
            .iter()
            .zip(instruction_offsets.iter())
            .zip(instructions_with_subs.iter())
        {
            let mapping = &mappings_by_air[air_name.as_str()];
            let occurrences = *air_id_occurrences.get(air_name.as_str()).unwrap();
            let record_stride_bytes = jit_mapping::dense_record_stride(mapping.air_name);

            let mut columns: Vec<EmitterColumn> = Vec::with_capacity(subs.len());
            for sub in subs.iter() {
                let col_mapping = &mapping.columns[sub.original_poly_index];
                let apc_col = apc_poly_id_to_index[&sub.apc_poly_id] as u16;
                match column_comp_to_emitter(&col_mapping.computation, apc_col) {
                    Some(c) => columns.push(c),
                    None => {
                        tracing::debug!(
                            "GPU NVRTC: unsupported ColumnComputation on AIR '{}', falling back",
                            air_name
                        );
                        return Err(OriginalArenas::Initialized(original_arenas));
                    }
                }
            }

            // arena_offset is filled in below after we know the layout.
            emitter_instructions.push(EmitterInstruction {
                arena_offset: 0,
                record_stride: (record_stride_bytes * occurrences) as u32,
                record_offset: (record_stride_bytes * offset) as u32,
                columns,
            });
        }

        // Concat arena bytes (same layout as descriptor path).
        let mut concat_arena_bytes: Vec<u8> = Vec::new();
        let mut arena_offsets: std::collections::HashMap<String, u32> =
            std::collections::HashMap::new();
        for air_name in air_names.iter().unique() {
            if let Some(arena) = original_arenas.take_real_arena(air_name) {
                let bytes = arena.allocated();
                if bytes.is_empty() {
                    continue;
                }
                let offset = concat_arena_bytes.len() as u32;
                concat_arena_bytes.extend_from_slice(bytes);
                arena_offsets.insert(air_name.clone(), offset);
            }
        }

        // Backfill per-instruction arena_offset.
        for (i, air_name) in air_names.iter().enumerate() {
            emitter_instructions[i].arena_offset =
                *arena_offsets.get(air_name).unwrap_or(&0);
        }

        let d_arena = concat_arena_bytes.to_device().unwrap();

        let range_max_bits = {
            let num_bins = self.periphery.real.range_checker.count.len();
            if num_bins > 1 {
                (num_bins as f64).log2() as u32 - 1
            } else {
                29
            }
        };

        let mut output = DeviceMatrix::<BabyBear>::with_capacity(height, width);
        output.buffer().fill_zero().unwrap();

        // Emit, compile (cached), launch.
        let kernel = emit_jit_kernel_source(&EmitterInput {
            instructions: emitter_instructions,
        });
        let compiled = NvrtcKernelCache::global()
            .get_or_compile(&kernel)
            .expect("NVRTC compile failed");

        let block_x: u32 = 256;
        let grid_x: u32 = ((num_apc_calls as u32) + block_x - 1) / block_x.max(1);
        let rc = unsafe {
            cuda_abi::powdr_nvrtc_launch_jit_v1(
                compiled.function(),
                output.buffer().as_mut_ptr() as *mut u32,
                height,
                num_apc_calls as i32,
                d_arena.as_ptr(),
                range_max_bits,
                grid_x.max(1),
                block_x,
            )
        };
        assert_eq!(rc, 0, "NVRTC kernel launch failed: {}", rc);

        // Optional A/B verifier: launch the descriptor kernel into a parallel
        // buffer with the same arena and assert host-side equality. Fires only
        // when POWDR_JIT_BACKEND_VERIFY is set; intended as a development
        // guardrail while emitter coverage broadens.
        if std::env::var("POWDR_JIT_BACKEND_VERIFY").is_ok() {
            use crate::cuda_abi::{JitColumnDesc, JitInstructionDesc};
            use openvm_cuda_common::copy::MemCopyD2H;

            let mut jit_instructions: Vec<JitInstructionDesc> = Vec::new();
            let mut jit_col_descs: Vec<JitColumnDesc> = Vec::new();
            for ((air_name, offset), (_, subs)) in air_names
                .iter()
                .zip(instruction_offsets.iter())
                .zip(instructions_with_subs.iter())
            {
                let mapping = &mappings_by_air[air_name.as_str()];
                let occurrences = *air_id_occurrences.get(air_name.as_str()).unwrap();
                let arena_offset_inst = *arena_offsets.get(air_name).unwrap_or(&0);
                let record_stride_bytes = jit_mapping::dense_record_stride(mapping.air_name);
                let col_desc_start = jit_col_descs.len() as u32;
                for sub in subs.iter() {
                    let apc_col_index = apc_poly_id_to_index[&sub.apc_poly_id];
                    let col_mapping = &mapping.columns[sub.original_poly_index];
                    jit_col_descs.push(column_comp_to_jit_desc(
                        &col_mapping.computation,
                        apc_col_index as u16,
                    ));
                }
                let col_desc_count = jit_col_descs.len() as u32 - col_desc_start;
                jit_instructions.push(JitInstructionDesc {
                    arena_offset: arena_offset_inst,
                    record_stride: (record_stride_bytes * occurrences) as u32,
                    record_offset: (record_stride_bytes * offset) as u32,
                    col_desc_start,
                    col_desc_count,
                });
            }

            let d_instructions = jit_instructions.to_device().unwrap();
            let d_col_descs = jit_col_descs.to_device().unwrap();

            let mut output_desc = DeviceMatrix::<BabyBear>::with_capacity(height, width);
            output_desc.buffer().fill_zero().unwrap();
            cuda_abi::apc_jit_tracegen(
                &mut output_desc,
                &d_arena,
                &d_instructions,
                &d_col_descs,
                num_apc_calls,
                range_max_bits,
            )
            .unwrap();

            let host_nvrtc: Vec<BabyBear> = output.buffer().to_host().unwrap();
            let host_desc: Vec<BabyBear> = output_desc.buffer().to_host().unwrap();
            assert_eq!(host_nvrtc.len(), host_desc.len());

            let mut mismatches = 0usize;
            let mut first: Option<(usize, usize, u32, u32)> = None;
            for col in 0..width {
                for r in 0..num_apc_calls {
                    let off = col * height + r;
                    let n = host_nvrtc[off].as_canonical_u32();
                    let d = host_desc[off].as_canonical_u32();
                    if n != d {
                        mismatches += 1;
                        if first.is_none() {
                            first = Some((col, r, n, d));
                        }
                    }
                }
            }
            if mismatches > 0 {
                let (c, r, n, d) = first.unwrap();
                panic!(
                    "NVRTC verifier mismatch ({}): {} cells differ. First at apc_col={} row={}: nvrtc={} descriptor={}",
                    kernel.name, mismatches, c, r, n, d
                );
            }
            tracing::info!(
                "NVRTC verifier PASS: {} cols x {} rows match descriptor backend ({})",
                width, num_apc_calls, kernel.name
            );
        }

        // Stage 3 (unchanged): derived expressions + bus interactions.
        let (derived_specs, derived_bc) = compile_derived_to_gpu(
            &self.apc.machine.derived_columns,
            &apc_poly_id_to_index,
            height,
        );
        let d_specs = derived_specs.to_device().unwrap();
        let d_bc = derived_bc.to_device().unwrap();
        cuda_abi::apc_apply_derived_expr(&mut output, d_specs, d_bc, num_apc_calls).unwrap();

        let (bus_interactions, arg_spans, bytecode) = compile_bus_to_gpu(
            &self.apc.machine.bus_interactions,
            &apc_poly_id_to_index,
            height,
        );
        let bus_interactions = bus_interactions.to_device().unwrap();
        let arg_spans = arg_spans.to_device().unwrap();
        let bytecode = bytecode.to_device().unwrap();

        let periphery = &self.periphery.real;
        let var_range_bus_id = self.periphery.bus_ids.range_checker as u32;
        let var_range_count = &periphery.range_checker.count;
        let tuple_range_checker_chip = periphery.tuple_range_checker.as_ref().unwrap();
        let tuple2_bus_id = self.periphery.bus_ids.tuple_range_checker.unwrap() as u32;
        let tuple2_sizes = tuple_range_checker_chip.sizes;
        let tuple2_count_u32 = tuple_range_checker_chip.count.as_ref();
        let bitwise_bus_id = self.periphery.bus_ids.bitwise_lookup.unwrap() as u32;
        let bitwise_count_u32 = periphery.bitwise_lookup_8.as_ref().unwrap().count.as_ref();

        cuda_abi::apc_apply_bus(
            &output,
            num_apc_calls,
            bytecode,
            bus_interactions,
            arg_spans,
            var_range_bus_id,
            var_range_count,
            tuple2_bus_id,
            tuple2_count_u32,
            tuple2_sizes,
            bitwise_bus_id,
            bitwise_count_u32,
        )
        .unwrap();

        Ok(output)
    }
}

/// Convert a Rust ColumnComputation into the NVRTC emitter's column variant.
/// Returns `None` for arms the emitter does not yet support, in which case
/// the caller falls back to the descriptor backend.
fn column_comp_to_emitter(
    comp: &super::jit_mapping::ColumnComputation,
    apc_col: u16,
) -> Option<crate::powdr_extension::trace_generator::cuda::nvrtc_emit::EmitterColumn> {
    use crate::powdr_extension::trace_generator::cuda::nvrtc_emit::EmitterColumn;
    use super::jit_mapping::ColumnComputation as CC;
    match comp {
        CC::DirectU32 { record_byte_offset } => Some(EmitterColumn::DirectU32 {
            apc_col,
            off: *record_byte_offset as u16,
        }),
        CC::DirectU8 { record_byte_offset } => Some(EmitterColumn::DirectU8 {
            apc_col,
            off: *record_byte_offset as u16,
        }),
        CC::DirectU16 { record_byte_offset } => Some(EmitterColumn::DirectU16 {
            apc_col,
            off: *record_byte_offset as u16,
        }),
        CC::Constant(v) => Some(EmitterColumn::Constant {
            apc_col,
            value: *v,
        }),
        // Without opcode folding we'd need to read the opcode byte at runtime;
        // the host doesn't yet thread per-instruction opcode through, so
        // BoolFromOpcode without folding is unsupported.
        CC::BoolFromOpcode { .. } => None,
        CC::Conditional { condition_byte_offset, then_comp } => {
            let inner = column_comp_to_emitter(then_comp, apc_col)?;
            Some(EmitterColumn::Conditional {
                cond_off: *condition_byte_offset as u16,
                inner: Box::new(inner),
            })
        }
        CC::TimestampDecomp {
            curr_ts_byte_offset,
            curr_ts_delta,
            prev_ts_byte_offset,
            limb_index,
        } => Some(EmitterColumn::TimestampDecomp {
            apc_col,
            curr_off: *curr_ts_byte_offset as u16,
            prev_off: *prev_ts_byte_offset as u16,
            delta: *curr_ts_delta as u16,
            limb_index: *limb_index as u16,
        }),
        CC::PointerLimb {
            val_byte_offset,
            imm_byte_offset,
            imm_sign_byte_offset,
            limb_index,
        } => Some(EmitterColumn::PointerLimb {
            apc_col,
            val_off: *val_byte_offset as u16,
            imm_off: *imm_byte_offset as u16,
            imm_sign_off: *imm_sign_byte_offset as u16,
            limb_index: *limb_index as u16,
        }),
        CC::AluResult {
            opcode_byte_offset,
            b_byte_offset,
            c_byte_offset,
            limb_index,
        } => Some(EmitterColumn::AluResult {
            apc_col,
            opcode_off: *opcode_byte_offset as u16,
            b_off: *b_byte_offset as u16,
            c_off: *c_byte_offset as u16,
            limb_index: *limb_index as u16,
        }),
        CC::ShiftResult {
            opcode_byte_offset,
            b_byte_offset,
            c_byte_offset,
            limb_index,
        } => Some(EmitterColumn::ShiftResult {
            apc_col,
            opcode_off: *opcode_byte_offset as u16,
            b_off: *b_byte_offset as u16,
            c_off: *c_byte_offset as u16,
            limb_index: *limb_index as u16,
        }),
        CC::ShiftBitMulLeft { opcode_byte_offset, c_byte_offset } => {
            Some(EmitterColumn::ShiftBitMulLeft {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                c_off: *c_byte_offset as u16,
            })
        }
        CC::ShiftBitMulRight { opcode_byte_offset, c_byte_offset } => {
            Some(EmitterColumn::ShiftBitMulRight {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                c_off: *c_byte_offset as u16,
            })
        }
        CC::ShiftBSign { opcode_byte_offset, b_byte_offset } => {
            Some(EmitterColumn::ShiftBSign {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                b_off: *b_byte_offset as u16,
            })
        }
        CC::ShiftBitMarker { c_byte_offset, marker_index } => {
            Some(EmitterColumn::ShiftBitMarker {
                apc_col,
                c_off: *c_byte_offset as u16,
                marker_index: *marker_index as u16,
            })
        }
        CC::ShiftLimbMarker { c_byte_offset, marker_index } => {
            Some(EmitterColumn::ShiftLimbMarker {
                apc_col,
                c_off: *c_byte_offset as u16,
                marker_index: *marker_index as u16,
            })
        }
        CC::ShiftBitCarry { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            Some(EmitterColumn::ShiftBitCarry {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                b_off: *b_byte_offset as u16,
                c_off: *c_byte_offset as u16,
                limb_index: *limb_index as u16,
            })
        }
        CC::BranchEqualCmpResult { a_byte_offset, b_byte_offset, opcode_byte_offset } => {
            Some(EmitterColumn::BranchEqualCmpResult {
                apc_col,
                a_off: *a_byte_offset as u16,
                b_off: *b_byte_offset as u16,
                opcode_off: *opcode_byte_offset as u16,
            })
        }
        CC::BranchEqualDiffInvMarker { a_byte_offset, b_byte_offset, opcode_byte_offset: _, marker_index } => {
            Some(EmitterColumn::BranchEqualDiffInvMarker {
                apc_col,
                a_off: *a_byte_offset as u16,
                b_off: *b_byte_offset as u16,
                marker_index: *marker_index as u16,
            })
        }
        CC::LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset } => {
            Some(EmitterColumn::LoadStoreRdRs2Ptr {
                apc_col,
                off: *rd_rs2_ptr_byte_offset as u16,
            })
        }
        CC::LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset } => {
            Some(EmitterColumn::LoadStoreNeedsWrite {
                apc_col,
                off: *rd_rs2_ptr_byte_offset as u16,
            })
        }
        CC::LoadStoreWriteAuxPrevTs { write_prev_ts_byte_offset, rd_rs2_ptr_byte_offset } => {
            Some(EmitterColumn::LoadStoreWriteAuxPrevTs {
                apc_col,
                write_prev_ts_off: *write_prev_ts_byte_offset as u16,
                rd_rs2_ptr_off: *rd_rs2_ptr_byte_offset as u16,
            })
        }
        CC::LoadStoreWriteAuxDecomp { from_ts_byte_offset, write_prev_ts_byte_offset, rd_rs2_ptr_byte_offset, limb_index } => {
            Some(EmitterColumn::LoadStoreWriteAuxDecomp {
                apc_col,
                from_ts_off: *from_ts_byte_offset as u16,
                write_prev_ts_off: *write_prev_ts_byte_offset as u16,
                rd_rs2_ptr_off: *rd_rs2_ptr_byte_offset as u16,
                limb_index: *limb_index as u16,
            })
        }
        CC::LoadStoreIsLoad { opcode_byte_offset } => {
            Some(EmitterColumn::LoadStoreIsLoad {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
            })
        }
        CC::LoadStoreFlag { opcode_byte_offset, shift_byte_offset, flag_index } => {
            Some(EmitterColumn::LoadStoreFlag {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                shift_off: *shift_byte_offset as u16,
                flag_index: *flag_index as u16,
            })
        }
        CC::LoadStoreWriteData { opcode_byte_offset, shift_byte_offset, read_data_byte_offset, prev_data_byte_offset, limb_index } => {
            Some(EmitterColumn::LoadStoreWriteData {
                apc_col,
                opcode_off: *opcode_byte_offset as u16,
                shift_off: *shift_byte_offset as u16,
                read_data_off: *read_data_byte_offset as u16,
                prev_data_off: *prev_data_byte_offset as u16,
                limb_index: *limb_index as u16,
            })
        }
    }
}

/// Convert a Rust ColumnComputation to a CUDA JitColumnDesc.
fn column_comp_to_jit_desc(
    comp: &super::jit_mapping::ColumnComputation,
    apc_col: u16,
) -> crate::cuda_abi::JitColumnDesc {
    use super::jit_mapping::ColumnComputation as CC;
    use crate::cuda_abi::JitColumnDesc;

    let mut desc = JitColumnDesc {
        comp_type: 0,
        apc_col,
        p: [0; 6],
    };

    match comp {
        CC::DirectU32 { record_byte_offset } => {
            desc.comp_type = 0; // JIT_DIRECT_U32
            desc.p[0] = *record_byte_offset as u16;
        }
        CC::DirectU8 { record_byte_offset } => {
            desc.comp_type = 1; // JIT_DIRECT_U8
            desc.p[0] = *record_byte_offset as u16;
        }
        CC::DirectU16 { record_byte_offset } => {
            desc.comp_type = 2; // JIT_DIRECT_U16
            desc.p[0] = *record_byte_offset as u16;
        }
        CC::TimestampDecomp { curr_ts_byte_offset, curr_ts_delta, prev_ts_byte_offset, limb_index } => {
            desc.comp_type = 3; // JIT_TIMESTAMP_DECOMP
            desc.p[0] = *curr_ts_byte_offset as u16;
            desc.p[1] = *curr_ts_delta as u16;
            desc.p[2] = *prev_ts_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::AluResult { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            desc.comp_type = 4; // JIT_ALU_RESULT
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
            desc.p[2] = *c_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::BoolFromOpcode { opcode_byte_offset, expected_opcode } => {
            desc.comp_type = 5; // JIT_BOOL_FROM_OPCODE
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *expected_opcode as u16;
        }
        CC::PointerLimb { val_byte_offset, imm_byte_offset, imm_sign_byte_offset, limb_index } => {
            desc.comp_type = 6; // JIT_POINTER_LIMB
            desc.p[0] = *val_byte_offset as u16;
            desc.p[1] = *imm_byte_offset as u16;
            desc.p[2] = *imm_sign_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::Conditional { condition_byte_offset, then_comp } => {
            // Encode inner computation with conditional flag
            let mut inner = column_comp_to_jit_desc(then_comp, apc_col);
            inner.comp_type |= 0x80; // JIT_COND_FLAG
            inner.p[5] = *condition_byte_offset as u16;
            return inner;
        }
        CC::Constant(val) => {
            desc.comp_type = 16; // JIT_CONSTANT
            desc.p[0] = (*val & 0xFFFF) as u16;
            desc.p[1] = ((*val >> 16) & 0xFFFF) as u16;
        }
        CC::ShiftResult { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            desc.comp_type = 7;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
            desc.p[2] = *c_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::ShiftBitMulLeft { opcode_byte_offset, c_byte_offset } => {
            desc.comp_type = 8;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *c_byte_offset as u16;
        }
        CC::ShiftBitMulRight { opcode_byte_offset, c_byte_offset } => {
            desc.comp_type = 9;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *c_byte_offset as u16;
        }
        CC::ShiftBSign { opcode_byte_offset, b_byte_offset } => {
            desc.comp_type = 10;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
        }
        CC::ShiftBitMarker { c_byte_offset, marker_index } => {
            desc.comp_type = 11;
            desc.p[0] = *c_byte_offset as u16;
            desc.p[1] = *marker_index as u16;
        }
        CC::ShiftLimbMarker { c_byte_offset, marker_index } => {
            desc.comp_type = 12;
            desc.p[0] = *c_byte_offset as u16;
            desc.p[1] = *marker_index as u16;
        }
        CC::ShiftBitCarry { opcode_byte_offset, b_byte_offset, c_byte_offset, limb_index } => {
            desc.comp_type = 13;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
            desc.p[2] = *c_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::BranchEqualCmpResult { a_byte_offset, b_byte_offset, opcode_byte_offset } => {
            desc.comp_type = 14;
            desc.p[0] = *a_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
            desc.p[2] = *opcode_byte_offset as u16;
        }
        CC::BranchEqualDiffInvMarker { a_byte_offset, b_byte_offset, opcode_byte_offset, marker_index } => {
            desc.comp_type = 15;
            desc.p[0] = *a_byte_offset as u16;
            desc.p[1] = *b_byte_offset as u16;
            desc.p[2] = *opcode_byte_offset as u16;
            desc.p[3] = *marker_index as u16;
        }
        CC::LoadStoreRdRs2Ptr { rd_rs2_ptr_byte_offset } => {
            desc.comp_type = 17;
            desc.p[0] = *rd_rs2_ptr_byte_offset as u16;
        }
        CC::LoadStoreNeedsWrite { rd_rs2_ptr_byte_offset } => {
            desc.comp_type = 18;
            desc.p[0] = *rd_rs2_ptr_byte_offset as u16;
        }
        CC::LoadStoreWriteAuxPrevTs { write_prev_ts_byte_offset, rd_rs2_ptr_byte_offset } => {
            desc.comp_type = 19;
            desc.p[0] = *write_prev_ts_byte_offset as u16;
            desc.p[1] = *rd_rs2_ptr_byte_offset as u16;
        }
        CC::LoadStoreWriteAuxDecomp { from_ts_byte_offset, write_prev_ts_byte_offset, rd_rs2_ptr_byte_offset, limb_index } => {
            desc.comp_type = 20;
            desc.p[0] = *from_ts_byte_offset as u16;
            desc.p[1] = *write_prev_ts_byte_offset as u16;
            desc.p[2] = *rd_rs2_ptr_byte_offset as u16;
            desc.p[3] = *limb_index as u16;
        }
        CC::LoadStoreIsLoad { opcode_byte_offset } => {
            desc.comp_type = 21;
            desc.p[0] = *opcode_byte_offset as u16;
        }
        CC::LoadStoreFlag { opcode_byte_offset, shift_byte_offset, flag_index } => {
            desc.comp_type = 22;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *shift_byte_offset as u16;
            desc.p[2] = *flag_index as u16;
        }
        CC::LoadStoreWriteData { opcode_byte_offset, shift_byte_offset, read_data_byte_offset, prev_data_byte_offset, limb_index } => {
            desc.comp_type = 23;
            desc.p[0] = *opcode_byte_offset as u16;
            desc.p[1] = *shift_byte_offset as u16;
            desc.p[2] = *read_data_byte_offset as u16;
            desc.p[3] = *prev_data_byte_offset as u16;
            desc.p[4] = *limb_index as u16;
        }
    }

    desc
}

impl<R, PB: ProverBackend<Matrix = DeviceMatrix<BabyBear>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipGpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let backend = pick_jit_backend();
        let arenas = self.record_arena_by_air_name.take();

        let trace = match backend {
            JitBackend::Off => self
                .trace_generator
                .try_generate_witness(arenas)
                .unwrap_or_else(DeviceMatrix::dummy),
            JitBackend::Descriptor => match self.trace_generator.try_generate_witness_jit(arenas) {
                Ok(trace) => {
                    tracing::info!(
                        "GPU JIT (descriptor) trace gen used for PowdrChip {}",
                        self.name
                    );
                    trace
                }
                Err(arenas) => {
                    tracing::warn!(
                        "GPU JIT (descriptor) not available for PowdrChip {}, falling back",
                        self.name
                    );
                    self.trace_generator
                        .try_generate_witness(arenas)
                        .unwrap_or_else(DeviceMatrix::dummy)
                }
            },
            JitBackend::Nvrtc => match self.trace_generator.try_generate_witness_nvrtc(arenas) {
                Ok(trace) => {
                    tracing::info!(
                        "GPU JIT (nvrtc) trace gen used for PowdrChip {}",
                        self.name
                    );
                    trace
                }
                Err(arenas) => {
                    tracing::debug!(
                        "GPU JIT (nvrtc) not yet supported for PowdrChip {}, trying descriptor",
                        self.name
                    );
                    match self.trace_generator.try_generate_witness_jit(arenas) {
                        Ok(trace) => {
                            tracing::info!(
                                "GPU JIT (descriptor) trace gen used for PowdrChip {}",
                                self.name
                            );
                            trace
                        }
                        Err(arenas) => self
                            .trace_generator
                            .try_generate_witness(arenas)
                            .unwrap_or_else(DeviceMatrix::dummy),
                    }
                }
            },
        };

        AirProvingContext {
            cached_mains: vec![],
            common_main: trace,
            public_values: vec![],
        }
    }
}
