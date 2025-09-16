use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    bus_map::DEFAULT_VARIABLE_RANGE_CHECKER,
    extraction_utils::{get_name, OriginalAirs},
    powdr_extension::executor::{
        inventory::{DummyChipComplex, DummyInventory},
        periphery::SharedPeripheryChips,
    },
    ExtendedVmConfig, Instr,
};

use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, ConcreteBusInteraction, MappingRowEvaluator, RowEvaluator},
    trace_handler::{generate_trace, ComputationMethod, Trace, TraceData},
    Apc,
};

use itertools::Itertools;
use openvm_circuit::{
    arch::VmConfig, system::memory::MemoryController, utils::next_power_of_two_or_zero,
};
use openvm_circuit::{
    arch::{ExecutionState, InstructionExecutor, Result as ExecutionResult, VmInventoryError},
    system::memory::{online::MemoryLogEntry, OfflineMemory},
};
use openvm_native_circuit::CastFExtension;
use openvm_stark_backend::{
    p3_field::FieldAlgebra, p3_matrix::dense::DenseMatrix,
    p3_maybe_rayon::prelude::ParallelIterator,
};

use openvm_stark_backend::p3_maybe_rayon::prelude::IndexedParallelIterator;
use openvm_stark_backend::{
    config::StarkGenericConfig,
    p3_commit::{Pcs, PolynomialSpace},
    p3_maybe_rayon::prelude::ParallelSliceMut,
    Chip,
};
use openvm_stark_backend::{p3_field::PrimeField32, p3_matrix::dense::RowMajorMatrix};
use powdr_autoprecompiles::InstructionHandler;

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrExecutor
mod periphery;

pub use periphery::PowdrPeripheryInstances;
use powdr_openvm_hints_circuit::HintsExtension;

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor<F: PrimeField32> {
    air_by_opcode_id: OriginalAirs<F>,
    inventory: DummyInventory<F>,
    number_of_calls: usize,
    periphery: SharedPeripheryChips,
    apc: Arc<Apc<F, Instr<F>>>,
}

impl<F: PrimeField32> PowdrExecutor<F> {
    pub fn new(
        air_by_opcode_id: OriginalAirs<F>,
        memory: Arc<Mutex<OfflineMemory<F>>>,
        base_config: ExtendedVmConfig,
        periphery: PowdrPeripheryInstances,
        apc: Arc<Apc<F, Instr<F>>>,
    ) -> Self {
        Self {
            air_by_opcode_id,
            inventory: create_chip_complex_with_memory(
                memory,
                periphery.dummy,
                base_config.clone(),
            )
            .unwrap()
            .inventory,
            number_of_calls: 0,
            periphery: periphery.real,
            apc,
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
        let res =
            self.apc
                .instructions()
                .iter()
                .try_fold(from_state, |execution_state, instruction| {
                    let executor = self
                        .inventory
                        .get_mut_executor(&instruction.0.opcode)
                        .unwrap();
                    executor.execute(memory, &instruction.0, execution_state)
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

        res
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness<SC>(self) -> RowMajorMatrix<F>
    where
        SC: StarkGenericConfig,
        <SC::Pcs as Pcs<SC::Challenge, SC::Challenger>>::Domain: PolynomialSpace<Val = F>,
    {
        let dummy_trace_by_air_name: HashMap<_, _> = self
            .inventory
            .executors
            .into_iter()
            .map(|executor| {
                let air_name = get_name::<SC>(executor.air());
                let DenseMatrix { values, width, .. } =
                    tracing::debug_span!("dummy trace", air_name = air_name.clone()).in_scope(
                        || {
                            Chip::<SC>::generate_air_proof_input(executor)
                                .raw
                                .common_main
                                .unwrap()
                        },
                    );
                (air_name.clone(), Trace::new(values, width))
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            apc_poly_id_to_index,
            columns_to_compute,
        } = generate_trace(
            &dummy_trace_by_air_name,
            &self.air_by_opcode_id,
            self.number_of_calls,
            &self.apc,
        );

        // precompute the symbolic bus sends to the range checker for each original instruction
        let range_checker_sends_per_original_instruction = self
            .apc
            .instructions()
            .iter()
            .map(|instruction| {
                self.air_by_opcode_id
                    .get_instruction_air_and_id(instruction)
                    .1
                    .bus_interactions
                    .iter()
                    .filter(|interaction| interaction.id == DEFAULT_VARIABLE_RANGE_CHECKER)
                    .collect_vec()
            })
            .collect_vec();

        // allocate for apc trace
        let width = apc_poly_id_to_index.len();
        let height = next_power_of_two_or_zero(self.number_of_calls);
        let mut values = <F as FieldAlgebra>::zero_vec(height * width);

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
                    let evaluator = RowEvaluator::new(dummy_row);

                    range_checker_sends.iter().for_each(|interaction| {
                        let ConcreteBusInteraction { mult, mut args, .. } =
                            evaluator.eval_bus_interaction(interaction);
                        for _ in 0..mult.as_canonical_u32() {
                            self.periphery.range_checker.remove_count(
                                args.next().unwrap().as_canonical_u32(),
                                args.next().unwrap().as_canonical_u32() as usize,
                            );
                        }
                    });

                    for (dummy_trace_index, apc_index) in dummy_trace_index_to_apc_index {
                        row_slice[*apc_index] = dummy_row[*dummy_trace_index];
                    }
                }

                // Fill in the columns we have to compute from other columns
                // (these are either new columns or for example the "is_valid" column).
                for (col_index, computation_method) in &columns_to_compute {
                    row_slice[*col_index] = match computation_method {
                        ComputationMethod::Constant(c) => F::from_canonical_u64(*c),
                        ComputationMethod::InverseOfSum(columns_to_sum) => columns_to_sum
                            .iter()
                            .map(|col| row_slice[*col])
                            .reduce(|a, b| a + b)
                            .unwrap()
                            .inverse(),
                    };
                }

                let evaluator = MappingRowEvaluator::new(row_slice, &apc_poly_id_to_index);

                // replay the side effects of this row on the main periphery
                self.apc
                    .machine()
                    .bus_interactions
                    .iter()
                    .for_each(|interaction| {
                        let ConcreteBusInteraction { id, mult, args } =
                            evaluator.eval_bus_interaction(interaction);
                        self.periphery.apply(
                            id as u16,
                            mult.as_canonical_u32(),
                            args.map(|arg| arg.as_canonical_u32()),
                        );
                    });
            });

        RowMajorMatrix::new(values, width)
    }
}

// Extracted from openvm, extended to create an inventory with the correct memory and periphery chips.
fn create_chip_complex_with_memory<F: PrimeField32>(
    memory: Arc<Mutex<OfflineMemory<F>>>,
    shared_chips: SharedPeripheryChips,
    base_config: ExtendedVmConfig,
) -> std::result::Result<DummyChipComplex<F>, VmInventoryError> {
    use openvm_keccak256_circuit::Keccak256;
    use openvm_native_circuit::Native;
    use openvm_rv32im_circuit::{Rv32I, Rv32Io};
    use openvm_sha256_circuit::Sha256;

    let this = base_config;
    let mut complex: DummyChipComplex<F> = this
        .sdk_vm_config
        .system
        .config
        .create_chip_complex()?
        .transmute();

    // CHANGE: inject the correct memory here to be passed to the chips, to be accessible in their get_proof_input
    complex.base.memory_controller.offline_memory = memory.clone();
    complex.base.range_checker_chip = shared_chips.range_checker.clone();
    // END CHANGE

    // CHANGE: inject the periphery chips so that they are not created by the extensions. This is done for memory footprint: the dummy periphery chips are thrown away anyway, so we reuse a single one for all APCs.
    complex = complex.extend(&shared_chips)?;
    // END CHANGE

    if this.sdk_vm_config.rv32i.is_some() {
        complex = complex.extend(&Rv32I)?;
    }
    if this.sdk_vm_config.io.is_some() {
        complex = complex.extend(&Rv32Io)?;
    }
    if this.sdk_vm_config.keccak.is_some() {
        complex = complex.extend(&Keccak256)?;
    }
    if this.sdk_vm_config.sha256.is_some() {
        complex = complex.extend(&Sha256)?;
    }
    if this.sdk_vm_config.native.is_some() {
        complex = complex.extend(&Native)?;
    }
    if this.sdk_vm_config.castf.is_some() {
        complex = complex.extend(&CastFExtension)?;
    }

    if let Some(rv32m) = this.sdk_vm_config.rv32m {
        let mut rv32m = rv32m;
        if let Some(ref bigint) = this.sdk_vm_config.bigint {
            rv32m.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            rv32m.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&rv32m)?;
    }
    if let Some(bigint) = this.sdk_vm_config.bigint {
        let mut bigint = bigint;
        if let Some(ref rv32m) = this.sdk_vm_config.rv32m {
            bigint.range_tuple_checker_sizes[0] =
                rv32m.range_tuple_checker_sizes[0].max(bigint.range_tuple_checker_sizes[0]);
            bigint.range_tuple_checker_sizes[1] =
                rv32m.range_tuple_checker_sizes[1].max(bigint.range_tuple_checker_sizes[1]);
        }
        complex = complex.extend(&bigint)?;
    }
    if let Some(ref modular) = this.sdk_vm_config.modular {
        complex = complex.extend(modular)?;
    }
    if let Some(ref fp2) = this.sdk_vm_config.fp2 {
        complex = complex.extend(fp2)?;
    }
    if let Some(ref pairing) = this.sdk_vm_config.pairing {
        complex = complex.extend(pairing)?;
    }
    if let Some(ref ecc) = this.sdk_vm_config.ecc {
        complex = complex.extend(ecc)?;
    }

    // add custom extensions
    complex = complex.extend(&HintsExtension)?;

    Ok(complex)
}
