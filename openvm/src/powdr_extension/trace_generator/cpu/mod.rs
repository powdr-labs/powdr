use std::{collections::HashMap, sync::Arc};

use itertools::Itertools;
use openvm_circuit::{arch::MatrixRecordArena, utils::next_power_of_two_or_zero};
use openvm_circuit_primitives::Chip;
use openvm_stark_backend::p3_maybe_rayon::prelude::*;
use openvm_stark_backend::{
    p3_field::{Field, PrimeCharacteristicRing, PrimeField32},
    p3_matrix::dense::{DenseMatrix, RowMajorMatrix},
    prover::{AirProvingContext, ProverBackend},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::trace_handler::TraceTrait;
use powdr_autoprecompiles::InstructionHandler;
use powdr_constraint_solver::constraint_system::ComputationMethod;

use super::jit_mapping::{self, AirColumnMapping, ColumnComputation};
use crate::{
    extraction_utils::{OriginalAirs, OriginalVmConfig},
    isa::IsaApc,
    isa::OpenVmISA,
    powdr_extension::{chip::PowdrChipCpu, executor::OriginalArenas},
};

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use inventory::DummyChipComplex;
pub use periphery::{
    PowdrPeripheryInstancesCpu, SharedPeripheryChipsCpu, SharedPeripheryChipsCpuProverExt,
};

/// A wrapper around a DenseMatrix to implement `TraceTrait` which is required for `generate_trace`.
pub struct SharedCpuTrace<F> {
    pub matrix: Arc<RowMajorMatrix<F>>,
}

impl<F: Send + Sync> TraceTrait<F> for SharedCpuTrace<F> {
    type Values = Vec<F>;

    fn width(&self) -> usize {
        self.matrix.width
    }

    fn values(&self) -> &Self::Values {
        &self.matrix.values
    }
}

impl<F> From<Arc<RowMajorMatrix<F>>> for SharedCpuTrace<F> {
    fn from(matrix: Arc<RowMajorMatrix<F>>) -> Self {
        Self { matrix }
    }
}

impl<R, PB: ProverBackend<Matrix = RowMajorMatrix<BabyBear>>, ISA: OpenVmISA> Chip<R, PB>
    for PowdrChipCpu<ISA>
{
    fn generate_proving_ctx(&self, _: R) -> AirProvingContext<PB> {
        tracing::trace!("Generating air proof input for PowdrChip {}", self.name);

        let use_jit = std::env::var("POWDR_JIT_TRACEGEN").is_ok();
        let validate_jit = std::env::var("POWDR_JIT_VALIDATE").is_ok();

        let row_major = if use_jit {
            let arenas = self.record_arena_by_air_name.take();
            match self.trace_generator.generate_witness_jit(arenas) {
                Ok(trace) => {
                    tracing::info!("JIT trace gen used for PowdrChip {}", self.name);
                    trace
                }
                Err(arenas) => {
                    tracing::warn!(
                        "JIT trace gen not available for PowdrChip {}, falling back to standard path",
                        self.name
                    );
                    self.trace_generator.generate_witness(arenas)
                }
            }
        } else if validate_jit {
            // Validation mode: run standard path first, then run JIT path on a
            // clone of the arenas and compare the APC columns that JIT fills.
            // This validates the JIT mapping is correct without requiring all
            // AIR types to be supported.
            self.trace_generator.generate_witness_and_validate_jit(
                self.record_arena_by_air_name.take(),
            )
        } else {
            self.trace_generator
                .generate_witness(self.record_arena_by_air_name.take())
        };

        AirProvingContext::simple(row_major, vec![])
    }
}

pub struct PowdrTraceGeneratorCpu<ISA: OpenVmISA> {
    pub apc: IsaApc<BabyBear, ISA>,
    pub original_airs: OriginalAirs<BabyBear, ISA>,
    pub config: OriginalVmConfig<ISA>,
    pub periphery: PowdrPeripheryInstancesCpu<ISA>,
}

impl<ISA: OpenVmISA> PowdrTraceGeneratorCpu<ISA> {
    pub fn new(
        apc: IsaApc<BabyBear, ISA>,
        original_airs: OriginalAirs<BabyBear, ISA>,
        config: OriginalVmConfig<ISA>,
        periphery: PowdrPeripheryInstancesCpu<ISA>,
    ) -> Self {
        Self {
            apc,
            original_airs,
            config,
            periphery,
        }
    }

    pub fn generate_witness(
        &self,
        original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> DenseMatrix<BabyBear> {
        use powdr_autoprecompiles::trace_handler::{generate_trace, TraceData};

        let width = self.apc.machine().main_columns().count();

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                // if the arenas are uninitialized, the apc was not called, so we return an empty trace
                return RowMajorMatrix::new(vec![], width);
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        let chip_inventory = {
            let airs = ISA::create_dummy_airs(self.config.config(), self.periphery.dummy.clone())
                .expect("Failed to create dummy airs");

            ISA::create_dummy_chip_complex_cpu(
                self.config.config(),
                airs,
                self.periphery.dummy.clone(),
            )
            .expect("Failed to create chip complex")
            .inventory
        };

        let jit_debug = std::env::var("POWDR_JIT_DEBUG").is_ok();

        let dummy_trace_by_air_name: HashMap<String, SharedCpuTrace<BabyBear>> = chip_inventory
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

                if jit_debug {
                    // Dump arena layout before fill_trace_row consumes it
                    let arena_width = record_arena.width;
                    let arena_offset = record_arena.trace_offset;
                    let rows_used = arena_offset / arena_width;

                    tracing::info!(
                        "JIT_DEBUG: AIR '{}' arena: width={}, rows_used={}, total_values={}",
                        air_name, arena_width, rows_used, record_arena.trace_buffer.len()
                    );

                    // Dump first row's raw record bytes
                    if rows_used > 0 {
                        let row0 = &record_arena.trace_buffer[..arena_width];
                        // Print raw u32 values (NOT Montgomery-decoded) — these are record struct bytes
                        let row0_raw: Vec<u32> = row0
                            .iter()
                            .map(|v| {
                                let bytes: [u8; 4] = unsafe { std::mem::transmute_copy(v) };
                                u32::from_le_bytes(bytes)
                            })
                            .collect();
                        tracing::info!(
                            "JIT_DEBUG:   row0 raw bytes (as u32 LE): {:?}",
                            row0_raw
                        );
                        // Also print as hex for easier struct field reading
                        let row0_hex: Vec<String> = row0_raw.iter().map(|v| format!("{:#010x}", v)).collect();
                        tracing::info!(
                            "JIT_DEBUG:   row0 raw bytes (hex): {:?}",
                            row0_hex
                        );
                    }
                }

                let row_major_trace = chip.generate_proving_ctx(record_arena).common_main;

                if jit_debug {
                    // Dump first row after fill_trace_row
                    let trace_width = row_major_trace.width;
                    let trace_height = row_major_trace.values.len() / trace_width;
                    tracing::info!(
                        "JIT_DEBUG: AIR '{}' trace: width={}, height={}",
                        air_name, trace_width, trace_height
                    );
                    if trace_height > 0 {
                        let row0 = &row_major_trace.values[..trace_width];
                        let row0_u32: Vec<u32> = row0.iter().map(|v| v.as_canonical_u32()).collect();
                        tracing::info!(
                            "JIT_DEBUG:   row0 after fill_trace (as u32): {:?}",
                            row0_u32
                        );
                    }
                }

                Some((air_name, SharedCpuTrace::from(Arc::new(row_major_trace))))
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            apc_poly_id_to_index,
            columns_to_compute,
        } = generate_trace(
            &dummy_trace_by_air_name,
            &self.original_airs,
            num_apc_calls,
            &self.apc,
        );

        // Build dense Vec indexed by poly ID for O(1) column lookups in the hot loop.
        // Poly IDs may be sparse (gaps between IDs), so the Vec is sized to max_id + 1.
        let width = apc_poly_id_to_index.len();
        let max_poly_id = apc_poly_id_to_index.keys().last().copied().unwrap_or(0) as usize;
        let apc_poly_id_to_index: Vec<usize> = (0..=max_poly_id)
            .map(|id| apc_poly_id_to_index.get(&(id as u64)).copied().unwrap_or(0))
            .collect();

        // Compile bus interactions once before the hot loop
        let compiled_interactions = {
            use powdr_autoprecompiles::expression::CompiledBusInteraction;
            CompiledBusInteraction::compile_all(
                &self.apc.machine().bus_interactions,
                &apc_poly_id_to_index,
                BabyBear::ZERO,
                BabyBear::ONE,
            )
        };

        // allocate for apc trace
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut values = <BabyBear as PrimeCharacteristicRing>::zero_vec(height * width);

        // Go through the final table and fill in the values.
        // Parallelized: the original code used chunks_mut (serial) because the
        // periphery.apply() calls were not thread-safe. Now that periphery chips
        // use AtomicU32 counters, we can use par_chunks_mut safely.
        // Extract references to avoid capturing `self` (ISA::Config is not Sync).
        let periphery_real = &self.periphery.real;
        let periphery_bus_ids = &self.periphery.bus_ids;

        values
            .par_chunks_mut(width)
            .zip(dummy_values.into_par_iter())
            .for_each(|(row_slice, dummy_values)| {
                // Copy dummy rows to APC row
                for (dummy_row, dummy_trace_index_to_apc_index) in dummy_values
                    .iter()
                    .map(|r| &r.data[r.start..r.start + r.length])
                    .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Compute derived columns
                for derived_column in columns_to_compute {
                    let col_index = apc_poly_id_to_index[derived_column.variable.id as usize];
                    row_slice[col_index] = match &derived_column.computation_method {
                        ComputationMethod::Constant(c) => *c,
                        ComputationMethod::QuotientOrZero(e1, e2) => {
                            use powdr_number::ExpressionConvertible;

                            let divisor_val = e2.to_expression(&|n| *n, &|column_ref| {
                                row_slice[apc_poly_id_to_index[column_ref.id as usize]]
                            });
                            if divisor_val.is_zero() {
                                BabyBear::ZERO
                            } else {
                                divisor_val.inverse()
                                    * e1.to_expression(&|n| *n, &|column_ref| {
                                        row_slice[apc_poly_id_to_index[column_ref.id as usize]]
                                    })
                            }
                        }
                    };
                }

                // Evaluate bus interactions using compiled expressions.
                // Periphery chips use AtomicU32 counters — thread-safe.
                for ci in &compiled_interactions {
                    let mult = ci.mult.eval(row_slice);
                    periphery_real.apply(
                        ci.id as u16,
                        mult.as_canonical_u32(),
                        ci.args.iter().map(|a| a.eval(row_slice).as_canonical_u32()),
                        periphery_bus_ids,
                    );
                }
            });

        RowMajorMatrix::new(values, width)
    }

    /// JIT trace generation: reads record bytes directly from arenas,
    /// computes only surviving APC columns, bypassing fill_trace_row entirely.
    ///
    /// Returns `Ok(trace)` if JIT was used, `Err(arenas)` if unsupported
    /// (arenas are returned unconsumed for fallback to standard path).
    pub fn generate_witness_jit(
        &self,
        original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> Result<DenseMatrix<BabyBear>, OriginalArenas<MatrixRecordArena<BabyBear>>> {
        use std::collections::BTreeMap;

        let width = self.apc.machine().main_columns().count();

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                return Ok(RowMajorMatrix::new(vec![], width));
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;
        let jit_debug = std::env::var("POWDR_JIT_DEBUG").is_ok();

        // Build the same instruction metadata as generate_trace, but we'll use
        // column mappings instead of dummy traces.

        // Keep only instructions with surviving substitutions
        let instructions_with_subs: Vec<_> = self
            .apc
            .instructions()
            .zip_eq(self.apc.subs().iter())
            .filter(|(_, subs)| !subs.is_empty())
            .collect();

        // Get AIR names for each instruction
        let air_names: Vec<String> = instructions_with_subs
            .iter()
            .map(|(instr, _)| {
                let (air_id, _) = self.original_airs.get_instruction_air_and_id(instr);
                air_id
            })
            .collect();

        // Count occurrences per AIR (= instructions of this type per APC call)
        let air_id_occurrences: HashMap<&str, usize> = air_names
            .iter()
            .map(|s| s.as_str())
            .counts();

        // Compute per-instruction offset within its AIR type
        let instruction_offsets: Vec<usize> = air_names
            .iter()
            .scan(
                HashMap::<&str, usize>::default(),
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
            .machine()
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        // Build substitution mapping: for each instruction, (original_poly_index -> apc_col_index)
        let subs_by_instruction: Vec<Vec<(usize, usize)>> = instructions_with_subs
            .iter()
            .map(|(_, subs)| {
                subs.iter()
                    .map(|s| (s.original_poly_index, apc_poly_id_to_index[&s.apc_poly_id]))
                    .collect()
            })
            .collect();

        // Look up the column mapping for each AIR type
        let mappings_by_air: HashMap<&str, AirColumnMapping> = {
            let mut m = HashMap::new();
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, BaseAluCoreAir<4, 8>",
                jit_mapping::base_alu_mapping(),
            );
            m.insert(
                "VmAirWrapper<Rv32LoadStoreAdapterAir, LoadStoreCoreAir<4>",
                jit_mapping::loadstore_mapping(),
            );
            m
        };

        // Pre-check: verify ALL AIR types have mappings BEFORE consuming any arenas
        for air_name in air_names.iter().unique() {
            if !mappings_by_air.contains_key(air_name.as_str()) {
                if jit_debug {
                    tracing::warn!(
                        "JIT_DEBUG: No JIT mapping for AIR '{}', falling back to standard path",
                        air_name
                    );
                }
                // Wrap arenas back into OriginalArenas::Initialized and return
                return Err(OriginalArenas::Initialized(original_arenas));
            }
        }

        // All AIR types are supported. Now take arenas.
        let mut arena_bytes_by_air: HashMap<String, (Vec<u8>, usize)> = HashMap::new();

        for air_name in air_names.iter().unique() {
            if let Some(arena) = original_arenas.take_real_arena(air_name) {
                let arena_width = arena.width;
                // Convert trace_buffer Vec<BabyBear> to Vec<u8> by reinterpreting
                let byte_vec: Vec<u8> = unsafe {
                    let ptr = arena.trace_buffer.as_ptr() as *const u8;
                    let len = arena.trace_buffer.len() * 4;
                    std::slice::from_raw_parts(ptr, len).to_vec()
                };
                arena_bytes_by_air.insert(air_name.clone(), (byte_vec, arena_width));
            }
        }

        // Build per-instruction JIT info
        // (We can't use InstructionJitInfo with lifetimes easily, so flatten the data)
        struct FlatJitInfo {
            arena_key: String,
            occurrences: usize,
            offset: usize,
            #[allow(dead_code)]
            mapping_air_name: &'static str,
        }

        let jit_infos: Vec<FlatJitInfo> = air_names
            .iter()
            .zip(instruction_offsets.iter())
            .map(|(air_name, offset)| {
                let occurrences = *air_id_occurrences.get(air_name.as_str()).unwrap();
                let mapping = mappings_by_air.get(air_name.as_str()).unwrap();
                FlatJitInfo {
                    arena_key: air_name.clone(),
                    occurrences,
                    offset: *offset,
                    mapping_air_name: mapping.air_name,
                }
            })
            .collect();

        // Get range_max_bits from the periphery's range checker
        let range_max_bits = self.periphery.real.range_checker.air.bus.range_max_bits as u32;

        if jit_debug {
            tracing::info!(
                "JIT_DEBUG: Starting JIT trace gen: {} APC calls, {} instructions with subs, {} APC cols, range_max_bits={}",
                num_apc_calls, instructions_with_subs.len(), width, range_max_bits
            );
        }

        // Build dense apc_poly_id_to_index for hot loop
        let max_poly_id = apc_poly_id_to_index.keys().last().copied().unwrap_or(0) as usize;
        let apc_poly_id_to_index_dense: Vec<usize> = (0..=max_poly_id)
            .map(|id| apc_poly_id_to_index.get(&(id as u64)).copied().unwrap_or(0))
            .collect();

        // Compile bus interactions
        let compiled_interactions = {
            use powdr_autoprecompiles::expression::CompiledBusInteraction;
            CompiledBusInteraction::compile_all(
                &self.apc.machine().bus_interactions,
                &apc_poly_id_to_index_dense,
                BabyBear::ZERO,
                BabyBear::ONE,
            )
        };

        let columns_to_compute = &self.apc.machine().derived_columns;

        // Allocate APC trace
        let height = next_power_of_two_or_zero(num_apc_calls);
        let mut values = <BabyBear as PrimeCharacteristicRing>::zero_vec(height * width);

        let periphery_real = &self.periphery.real;
        let periphery_bus_ids = &self.periphery.bus_ids;

        // Process each APC row
        values
            .par_chunks_mut(width)
            .enumerate()
            .for_each(|(row_idx, row_slice)| {
                if row_idx >= num_apc_calls {
                    return; // padding rows stay zero
                }

                // For each instruction with surviving subs
                for (_instr_idx, (jit_info, subs)) in
                    jit_infos.iter().zip(subs_by_instruction.iter()).enumerate()
                {
                    let (arena_bytes, arena_width) = {
                        let (bytes, w) = arena_bytes_by_air.get(&jit_info.arena_key).unwrap();
                        (bytes.as_slice(), *w)
                    };
                    let mapping = mappings_by_air.get(jit_info.arena_key.as_str()).unwrap();

                    // Compute the record byte offset in the arena
                    let arena_row_idx =
                        row_idx * jit_info.occurrences + jit_info.offset;
                    let row_bytes = arena_width * 4;
                    let record_byte_start = arena_row_idx * row_bytes;
                    let record_byte_end = record_byte_start + row_bytes;
                    let record_bytes = &arena_bytes[record_byte_start..record_byte_end];

                    // For each surviving substitution, compute the column value
                    for &(original_poly_index, apc_col_index) in subs {
                        // Look up the computation for this column
                        let col_mapping = &mapping.columns[original_poly_index];
                        debug_assert_eq!(col_mapping.col_index, original_poly_index);

                        let value: BabyBear = jit_mapping::eval_column(
                            &col_mapping.computation,
                            record_bytes,
                            range_max_bits,
                        );
                        row_slice[apc_col_index] = value;
                    }
                }

                // Compute derived columns (same as standard path)
                for derived_column in columns_to_compute {
                    let col_index =
                        apc_poly_id_to_index_dense[derived_column.variable.id as usize];
                    row_slice[col_index] = match &derived_column.computation_method {
                        ComputationMethod::Constant(c) => *c,
                        ComputationMethod::QuotientOrZero(e1, e2) => {
                            use powdr_number::ExpressionConvertible;
                            let divisor_val = e2.to_expression(&|n| *n, &|column_ref| {
                                row_slice
                                    [apc_poly_id_to_index_dense[column_ref.id as usize]]
                            });
                            if divisor_val.is_zero() {
                                BabyBear::ZERO
                            } else {
                                divisor_val.inverse()
                                    * e1.to_expression(&|n| *n, &|column_ref| {
                                        row_slice[apc_poly_id_to_index_dense
                                            [column_ref.id as usize]]
                                    })
                            }
                        }
                    };
                }

                // Evaluate bus interactions (same as standard path)
                for ci in &compiled_interactions {
                    let mult = ci.mult.eval(row_slice);
                    periphery_real.apply(
                        ci.id as u16,
                        mult.as_canonical_u32(),
                        ci.args.iter().map(|a| a.eval(row_slice).as_canonical_u32()),
                        periphery_bus_ids,
                    );
                }
            });

        if jit_debug {
            tracing::info!("JIT_DEBUG: JIT trace gen complete: {}x{}", height, width);
            if height > 0 {
                let row0_u32: Vec<u32> = values[..width]
                    .iter()
                    .map(|v| v.as_canonical_u32())
                    .collect();
                tracing::info!("JIT_DEBUG:   JIT row0: {:?}", row0_u32);
            }
        }

        Ok(RowMajorMatrix::new(values, width))
    }

    /// Validation mode: runs the standard path, then independently computes
    /// what the JIT path would produce for supported AIR types, and compares.
    /// Returns the standard-path trace (so the proof is unaffected).
    pub fn generate_witness_and_validate_jit(
        &self,
        original_arenas: OriginalArenas<MatrixRecordArena<BabyBear>>,
    ) -> DenseMatrix<BabyBear> {
        use std::collections::BTreeMap;

        let width = self.apc.machine().main_columns().count();

        let mut original_arenas = match original_arenas {
            OriginalArenas::Initialized(arenas) => arenas,
            OriginalArenas::Uninitialized => {
                return RowMajorMatrix::new(vec![], width);
            }
        };

        let num_apc_calls = original_arenas.number_of_calls;

        // Build instruction metadata (same as in generate_witness_jit)
        let instructions_with_subs: Vec<_> = self
            .apc
            .instructions()
            .zip_eq(self.apc.subs().iter())
            .filter(|(_, subs)| !subs.is_empty())
            .collect();

        let air_names: Vec<String> = instructions_with_subs
            .iter()
            .map(|(instr, _)| {
                let (air_id, _) = self.original_airs.get_instruction_air_and_id(instr);
                air_id
            })
            .collect();

        let air_id_occurrences: HashMap<&str, usize> =
            air_names.iter().map(|s| s.as_str()).counts();

        let instruction_offsets: Vec<usize> = air_names
            .iter()
            .scan(
                HashMap::<&str, usize>::default(),
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
            .machine()
            .main_columns()
            .enumerate()
            .map(|(index, c)| (c.id, index))
            .collect();

        let subs_by_instruction: Vec<Vec<(usize, usize)>> = instructions_with_subs
            .iter()
            .map(|(_, subs)| {
                subs.iter()
                    .map(|s| (s.original_poly_index, apc_poly_id_to_index[&s.apc_poly_id]))
                    .collect()
            })
            .collect();

        let mappings_by_air: HashMap<&str, jit_mapping::AirColumnMapping> = {
            let mut m = HashMap::new();
            m.insert(
                "VmAirWrapper<Rv32BaseAluAdapterAir, BaseAluCoreAir<4, 8>",
                jit_mapping::base_alu_mapping(),
            );
            m.insert(
                "VmAirWrapper<Rv32LoadStoreAdapterAir, LoadStoreCoreAir<4>",
                jit_mapping::loadstore_mapping(),
            );
            m
        };

        let range_max_bits = self.periphery.real.range_checker.air.bus.range_max_bits as u32;

        // Clone arena bytes for the AIR types we can JIT, BEFORE the standard path consumes them.
        // We use peek_real_arena to borrow without taking.
        let mut jit_arena_bytes: HashMap<String, (Vec<u8>, usize)> = HashMap::new();
        for air_name in air_names.iter().unique() {
            if mappings_by_air.contains_key(air_name.as_str()) {
                if let Some(arena) = original_arenas.peek_real_arena(air_name) {
                    let arena_width = arena.width;
                    let byte_vec: Vec<u8> = unsafe {
                        let ptr = arena.trace_buffer.as_ptr() as *const u8;
                        let len = arena.trace_buffer.len() * 4;
                        std::slice::from_raw_parts(ptr, len).to_vec()
                    };
                    jit_arena_bytes.insert(air_name.clone(), (byte_vec, arena_width));
                }
            }
        }

        // Run standard path (consumes arenas)
        let reference_trace = self
            .generate_witness(OriginalArenas::Initialized(original_arenas));

        // Now compute JIT values and compare with the reference trace
        let mut mismatches = 0usize;
        let mut checked = 0usize;

        for row_idx in 0..num_apc_calls {
            let ref_row = &reference_trace.values[row_idx * width..(row_idx + 1) * width];

            for (instr_idx, ((air_name, offset), subs)) in air_names
                .iter()
                .zip(instruction_offsets.iter())
                .zip(subs_by_instruction.iter())
                .enumerate()
            {
                let mapping = match mappings_by_air.get(air_name.as_str()) {
                    Some(m) => m,
                    None => continue, // Skip unsupported AIR types
                };

                let (arena_bytes, arena_width) = match jit_arena_bytes.get(air_name) {
                    Some(x) => (x.0.as_slice(), x.1),
                    None => continue,
                };

                let occurrences = *air_id_occurrences.get(air_name.as_str()).unwrap();
                let arena_row_idx = row_idx * occurrences + offset;
                let row_bytes = arena_width * 4;
                let record_byte_start = arena_row_idx * row_bytes;
                let record_byte_end = record_byte_start + row_bytes;

                if record_byte_end > arena_bytes.len() {
                    continue;
                }
                let record_bytes = &arena_bytes[record_byte_start..record_byte_end];

                for &(original_poly_index, apc_col_index) in subs {
                    let col_mapping = &mapping.columns[original_poly_index];
                    let jit_val: BabyBear = jit_mapping::eval_column(
                        &col_mapping.computation,
                        record_bytes,
                        range_max_bits,
                    );
                    let ref_val = ref_row[apc_col_index];
                    checked += 1;

                    if jit_val != ref_val {
                        mismatches += 1;
                        if mismatches <= 20 {
                            tracing::error!(
                                "JIT MISMATCH: row={}, instr={}, air='{}', orig_col={}, apc_col={}: JIT={}, REF={}",
                                row_idx, instr_idx, air_name, original_poly_index, apc_col_index,
                                jit_val.as_canonical_u32(), ref_val.as_canonical_u32()
                            );
                        }
                    }
                }
            }
        }

        if mismatches > 0 {
            tracing::error!(
                "JIT VALIDATION FAILED: {mismatches}/{checked} mismatches across {} rows",
                num_apc_calls
            );
        } else {
            tracing::info!(
                "JIT VALIDATION PASSED: {checked} column values checked across {} rows, all match!",
                num_apc_calls
            );
        }

        reference_trace
    }
}
