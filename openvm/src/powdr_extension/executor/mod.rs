use std::{
    collections::{BTreeMap, HashMap},
    sync::{Arc, Mutex},
};

use crate::{
    extraction_utils::OriginalAirs,
    powdr_extension::executor::{
        inventory::{DummyChipComplex, DummyInventory},
        periphery::SharedPeripheryChips,
    },
    Instr,
};

use super::{
    chip::{RangeCheckerSend, RowEvaluator},
    vm::OriginalInstruction,
};
use itertools::Itertools;
use openvm_circuit::{
    arch::VmConfig, system::memory::MemoryController, utils::next_power_of_two_or_zero,
};
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult, VmInventoryError},
    system::memory::{
        online::{ApcRange, MemoryLogEntry},
        OfflineMemory,
    },
};
use openvm_native_circuit::CastFExtension;
use openvm_sdk::config::SdkVmConfig;
use openvm_stark_backend::{
    p3_field::FieldAlgebra, p3_matrix::Matrix, p3_maybe_rayon::prelude::ParallelIterator,
};

use openvm_stark_backend::{
    air_builders::symbolic::symbolic_expression::SymbolicEvaluator,
    config::StarkGenericConfig,
    p3_commit::{Pcs, PolynomialSpace},
    p3_maybe_rayon::prelude::ParallelSliceMut,
    Chip,
};
use openvm_stark_backend::{
    p3_field::PrimeField32, p3_matrix::dense::RowMajorMatrix,
    p3_maybe_rayon::prelude::IntoParallelIterator,
};
use openvm_stark_backend::{p3_maybe_rayon::prelude::IndexedParallelIterator, ChipUsageGetter};
use powdr_autoprecompiles::{
    expression::AlgebraicReference, InstructionMachineHandler, SymbolicBusInteraction,
};

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrExecutor
mod periphery;

pub use periphery::PowdrPeripheryInstances;

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor<F: PrimeField32> {
    instructions: Vec<OriginalInstruction<F>>,
    air_by_opcode_id: OriginalAirs<F>,
    is_valid_poly_id: u64,
    inventory: DummyInventory<F>,
    number_of_calls: usize,
    periphery: SharedPeripheryChips,
}

impl<F: PrimeField32> PowdrExecutor<F> {
    pub fn new(
        instructions: Vec<OriginalInstruction<F>>,
        air_by_opcode_id: OriginalAirs<F>,
        is_valid_column: AlgebraicReference,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: SdkVmConfig,
        periphery: PowdrPeripheryInstances,
    ) -> Self {
        Self {
            instructions,
            air_by_opcode_id,
            is_valid_poly_id: is_valid_column.id,
            inventory: create_chip_complex_with_memory(
                memory,
                periphery.dummy,
                base_config.clone(),
            )
            .unwrap()
            .inventory,
            number_of_calls: 0,
            periphery: periphery.real,
        }
    }

    pub fn number_of_calls(&self) -> usize {
        self.number_of_calls
    }

    pub fn execute(
        &mut self,
        memory: &mut MemoryController<F>,
        from_state: ExecutionState<u32>,
    ) -> ExecutionResult<ExecutionState<u32>> {
        // save the next available `RecordId`
        let from_record_id = memory.get_memory_logs().len();

        // execute the original instructions one by one
        let res = self
            .instructions
            .iter()
            .try_fold(from_state, |execution_state, instruction| {
                let executor = self
                    .inventory
                    .get_mut_executor(&instruction.opcode())
                    .unwrap();
                executor.execute(memory, instruction.as_ref(), execution_state)
            });

        self.number_of_calls += 1;
        let memory_logs = memory.get_memory_logs(); // exclusive range

        let to_record_id = memory_logs.len();

        let last_read_write = memory_logs[from_record_id..to_record_id]
            .iter()
            .rposition(|entry| {
                matches!(
                    entry,
                    MemoryLogEntry::Read { .. } | MemoryLogEntry::Write { .. }
                )
            })
            .map(|idx| idx + from_record_id);

        tracing::trace!(
            "APC range (exclusive): {}..{} (last read/write at {})",
            from_record_id,
            to_record_id,
            last_read_write.unwrap_or(to_record_id)
        );

        memory
            .memory
            .apc_ranges
            .push(ApcRange::new(from_record_id, to_record_id, last_read_write));

        res
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness<SC>(
        self,
        column_index_by_poly_id: &BTreeMap<u64, usize>,
        bus_interactions: &[SymbolicBusInteraction<F>],
    ) -> RowMajorMatrix<F>
    where
        SC: StarkGenericConfig,
        <SC::Pcs as Pcs<SC::Challenge, SC::Challenger>>::Domain: PolynomialSpace<Val = F>,
    {
        let is_valid_index = column_index_by_poly_id[&self.is_valid_poly_id];
        let width = column_index_by_poly_id.len();
        let height = next_power_of_two_or_zero(self.number_of_calls);
        let mut values = <F as FieldAlgebra>::zero_vec(height * width);

        // for each original opcode, the name of the dummy air it corresponds to
        let air_name_by_opcode = self
            .instructions
            .iter()
            .map(|instruction| instruction.opcode())
            .unique()
            .map(|opcode| {
                (
                    opcode,
                    self.inventory.get_executor(opcode).unwrap().air_name(),
                )
            })
            .collect::<HashMap<_, _>>();

        let dummy_trace_by_air_name: HashMap<_, _> =
            self.inventory
                .executors
                .into_iter()
                .map(|executor| {
                    (
                        executor.air_name().clone(),
                        tracing::debug_span!("dummy trace", air_name = executor.air_name())
                            .in_scope(|| {
                                Chip::<SC>::generate_air_proof_input(executor)
                                    .raw
                                    .common_main
                                    .unwrap()
                            }),
                    )
                })
                .collect();

        let instruction_index_to_table_offset = self
            .instructions
            .iter()
            .enumerate()
            .scan(
                HashMap::default(),
                |counts: &mut HashMap<&str, usize>, (index, instruction)| {
                    let air_name = air_name_by_opcode.get(&instruction.opcode()).unwrap();
                    let count = counts.entry(air_name).or_default();
                    let current_count = *count;
                    *count += 1;
                    Some((index, (air_name, current_count)))
                },
            )
            .collect::<HashMap<_, _>>();

        let occurrences_by_table_name: HashMap<&String, usize> = self
            .instructions
            .iter()
            .map(|instruction| air_name_by_opcode.get(&instruction.opcode()).unwrap())
            .counts();

        // A vector of HashMap<dummy_trace_index, apc_trace_index> by instruction, empty HashMap if none maps to apc
        let dummy_trace_index_to_apc_index_by_instruction: Vec<HashMap<usize, usize>> = self
            .instructions
            .iter()
            .map(|instruction| {
                // look up how many dummy‐cells this AIR produces:
                let air_width = dummy_trace_by_air_name
                    .get(air_name_by_opcode.get(&instruction.opcode()).unwrap())
                    .unwrap()
                    .width();

                // build a map only of the (dummy_index -> apc_index) pairs
                let mut map = HashMap::with_capacity(air_width);
                for dummy_trace_index in 0..air_width {
                    if let Ok(apc_index) =
                        global_index(dummy_trace_index, instruction, column_index_by_poly_id)
                    {
                        if map.insert(dummy_trace_index, apc_index).is_some() {
                            panic!(
                                "duplicate dummy_trace_index {} for instruction opcode {:?}",
                                dummy_trace_index,
                                instruction.opcode()
                            );
                        }
                    }
                }
                map
            })
            .collect();

        assert_eq!(
            self.instructions.len(),
            dummy_trace_index_to_apc_index_by_instruction.len()
        );

        let dummy_values = (0..self.number_of_calls)
            .into_par_iter()
            .map(|record_index| {
                (0..self.instructions.len())
                    .map(|index| {
                        // get the air name and offset for this instruction (by index)
                        let (air_name, offset) =
                            instruction_index_to_table_offset.get(&index).unwrap();
                        // get the table
                        let table = dummy_trace_by_air_name.get(*air_name).unwrap();
                        // get how many times this table is used per record
                        let occurrences_per_record =
                            occurrences_by_table_name.get(air_name).unwrap();
                        // get the width of each occurrence
                        let width = table.width();
                        // start after the previous record ended, and offset by the correct offset
                        let start = (record_index * occurrences_per_record + offset) * width;
                        // end at the start + width
                        let end = start + width;
                        &table.values[start..end]
                    })
                    .collect_vec()
            });

        // precompute the symbolic bus sends to the range checker for each original instruction
        let range_checker_sends_per_original_instruction: Vec<Vec<RangeCheckerSend<_>>> = self
            .instructions
            .iter()
            .map(|instruction| {
                self.air_by_opcode_id
                    // TODO: avoid cloning the instruction
                    .get_instruction_air(&Instr(instruction.instruction.clone()))
                    .unwrap()
                    .bus_interactions
                    .iter()
                    .filter_map(|interaction| interaction.try_into().ok())
                    .collect_vec()
            })
            .collect_vec();

        // precompute the symbolic bus interactions for the autoprecompile
        let bus_interactions: Vec<crate::powdr_extension::chip::SymbolicBusInteraction<_>> =
            bus_interactions
                .iter()
                .map(|interaction| interaction.clone().into())
                .collect_vec();

        // go through the final table and fill in the values
        values
            // a record is `width` values
            .par_chunks_mut(width)
            .zip(dummy_values)
            .for_each(|(row_slice, dummy_values)| {
                // map the dummy rows to the autoprecompile row
                for ((dummy_row, range_checker_sends), dummy_trace_index_to_apc_index) in
                    dummy_values
                        .iter()
                        .zip_eq(&range_checker_sends_per_original_instruction)
                        .zip_eq(&dummy_trace_index_to_apc_index_by_instruction)
                {
                    let evaluator = RowEvaluator::new(dummy_row, None);

                    // first remove the side effects of this row on the main periphery
                    for range_checker_send in range_checker_sends {
                        let mult = evaluator
                            .eval_expr(&range_checker_send.mult)
                            .as_canonical_u32();
                        let value = evaluator
                            .eval_expr(&range_checker_send.value)
                            .as_canonical_u32();
                        let max_bits = evaluator
                            .eval_expr(&range_checker_send.max_bits)
                            .as_canonical_u32();
                        for _ in 0..mult {
                            self.periphery
                                .range_checker
                                .remove_count(value, max_bits as usize);
                        }
                    }

                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Set the is_valid column to 1
                row_slice[is_valid_index] = F::ONE;

                let evaluator = RowEvaluator::new(row_slice, Some(column_index_by_poly_id));

                // replay the side effects of this row on the main periphery
                // TODO: this could be done in parallel since `self.periphery` is thread safe, but is it worth it? cc @qwang98
                for bus_interaction in &bus_interactions {
                    let mult = evaluator
                        .eval_expr(&bus_interaction.mult)
                        .as_canonical_u32();
                    let args = bus_interaction
                        .args
                        .iter()
                        .map(|arg| evaluator.eval_expr(arg).as_canonical_u32());

                    self.periphery.apply(bus_interaction.id, mult, args);
                }
            });

        RowMajorMatrix::new(values, width)
    }
}

enum IndexError {
    NotInDummy,
    NotInAutoprecompile,
}

/// Maps the index of a column in the original AIR of a given instruction to the corresponding
/// index in the autoprecompile AIR.
fn global_index<F>(
    local_index: usize,
    instruction: &OriginalInstruction<F>,
    autoprecompile_index_by_poly_id: &BTreeMap<u64, usize>,
) -> Result<usize, IndexError> {
    // Map to the poly_id in the original instruction to the poly_id in the autoprecompile.
    let autoprecompile_poly_id = instruction
        .subs
        .get(local_index)
        .ok_or(IndexError::NotInDummy)?;
    // Map to the index in the autoprecompile.
    let variable_index = autoprecompile_index_by_poly_id
        .get(autoprecompile_poly_id)
        .ok_or(IndexError::NotInAutoprecompile)?;
    Ok(*variable_index)
}

// Extracted from openvm, extended to create an inventory with the correct memory and periphery chips.
fn create_chip_complex_with_memory<F: PrimeField32>(
    memory: Arc<Mutex<OfflineMemory<F>>>,
    shared_chips: SharedPeripheryChips,
    base_config: SdkVmConfig,
) -> std::result::Result<DummyChipComplex<F>, VmInventoryError> {
    use openvm_keccak256_circuit::Keccak256;
    use openvm_native_circuit::Native;
    use openvm_rv32im_circuit::{Rv32I, Rv32Io};
    use openvm_sha256_circuit::Sha256;

    let this = base_config;
    let mut complex: DummyChipComplex<F> = this.system.config.create_chip_complex()?.transmute();

    // CHANGE: inject the correct memory here to be passed to the chips, to be accessible in their get_proof_input
    complex.base.memory_controller.offline_memory = memory.clone();
    complex.base.range_checker_chip = shared_chips.range_checker.clone();
    // END CHANGE

    // CHANGE: inject the periphery chips so that they are not created by the extensions. This is done for memory footprint: the dummy periphery chips are thrown away anyway, so we reuse a single one for all APCs.
    complex = complex.extend(&shared_chips)?;
    // END CHANGE

    if this.rv32i.is_some() {
        complex = complex.extend(&Rv32I)?;
    }
    if this.io.is_some() {
        complex = complex.extend(&Rv32Io)?;
    }
    if this.keccak.is_some() {
        complex = complex.extend(&Keccak256)?;
    }
    if this.sha256.is_some() {
        complex = complex.extend(&Sha256)?;
    }
    if this.native.is_some() {
        complex = complex.extend(&Native)?;
    }
    if this.castf.is_some() {
        complex = complex.extend(&CastFExtension)?;
    }

    if let Some(rv32m) = this.rv32m {
        let mut rv32m = rv32m;
        if let Some(ref bigint) = this.bigint {
            rv32m.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            rv32m.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&rv32m)?;
    }
    if let Some(bigint) = this.bigint {
        let mut bigint = bigint;
        if let Some(ref rv32m) = this.rv32m {
            bigint.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            bigint.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&bigint)?;
    }
    if let Some(ref modular) = this.modular {
        complex = complex.extend(modular)?;
    }
    if let Some(ref fp2) = this.fp2 {
        complex = complex.extend(fp2)?;
    }
    if let Some(ref pairing) = this.pairing {
        complex = complex.extend(pairing)?;
    }
    if let Some(ref ecc) = this.ecc {
        complex = complex.extend(ecc)?;
    }

    Ok(complex)
}
