use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    bus_map::DEFAULT_VARIABLE_RANGE_CHECKER,
    extraction_utils::OriginalAirs,
    powdr_extension::executor::{inventory::DummyChipComplex, periphery::SharedPeripheryChips},
    BabyBearSC, ExtendedVmConfig, Instr,
};

use openvm_algebra_circuit::AlgebraCpuProverExt;
use openvm_bigint_circuit::Int256CpuProverExt;
use openvm_ecc_circuit::EccCpuProverExt;
use openvm_instructions::instruction::Instruction;
use openvm_keccak256_circuit::Keccak256CpuProverExt;
use openvm_native_circuit::NativeCpuProverExt;
use openvm_pairing_circuit::PairingProverExt;
use openvm_rv32im_circuit::Rv32ImCpuProverExt;
use openvm_sdk::config::{SdkVmConfig, SdkVmConfigExecutor};
use openvm_sha256_circuit::Sha2CpuProverExt;
use openvm_stark_backend::{p3_field::Field, prover::cpu::CpuBackend};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    expression::{AlgebraicEvaluator, ConcreteBusInteraction, MappingRowEvaluator, RowEvaluator},
    trace_handler::{generate_trace, ComputationMethod, Trace, TraceData},
    Apc,
};

use itertools::Itertools;
use openvm_circuit::{
    arch::{
        AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, Executor,
        ExecutorInventory, MeteredExecutor, PreflightExecutor, VmBuilder, VmCircuitExtension,
        VmProverExtension, VmStateMut,
    },
    system::SystemCpuBuilder,
};
use openvm_circuit::{
    arch::{ExecutionError, MatrixRecordArena},
    system::memory::online::TracingMemory,
    utils::next_power_of_two_or_zero,
};
use openvm_stark_backend::{p3_field::FieldAlgebra, p3_maybe_rayon::prelude::ParallelIterator};

use openvm_stark_backend::p3_maybe_rayon::prelude::IndexedParallelIterator;
use openvm_stark_backend::{p3_field::PrimeField32, p3_matrix::dense::RowMajorMatrix};
use powdr_autoprecompiles::InstructionHandler;

/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrExecutor
mod periphery;

pub use periphery::PowdrPeripheryInstances;
// use powdr_openvm_hints_circuit::HintsExtension;

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor {
    air_by_opcode_id: OriginalAirs<BabyBear>,
    chip_inventory:
        Mutex<ChipInventory<BabyBearSC, MatrixRecordArena<BabyBear>, CpuBackend<BabyBearSC>>>,
    executor_inventory: ExecutorInventory<SdkVmConfigExecutor<BabyBear>>,
    number_of_calls: usize,
    periphery: SharedPeripheryChips,
    apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
}

impl Executor<BabyBear> for PowdrExecutor {
    fn pre_compute_size(&self) -> usize {
        todo!()
    }

    fn pre_compute<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<
        openvm_circuit::arch::ExecuteFunc<BabyBear, Ctx>,
        openvm_circuit::arch::StaticProgramError,
    >
    where
        Ctx: openvm_circuit::arch::ExecutionCtxTrait,
    {
        todo!()
    }
}

impl<F: PrimeField32> MeteredExecutor<F> for PowdrExecutor {
    fn metered_pre_compute_size(&self) -> usize {
        todo!()
    }

    fn metered_pre_compute<Ctx>(
        &self,
        air_idx: usize,
        pc: u32,
        inst: &Instruction<F>,
        data: &mut [u8],
    ) -> Result<openvm_circuit::arch::ExecuteFunc<F, Ctx>, openvm_circuit::arch::StaticProgramError>
    where
        Ctx: openvm_circuit::arch::ExecutionCtxTrait,
    {
        todo!()
    }
}

impl<F: PrimeField32> PreflightExecutor<F> for PowdrExecutor {
    fn execute(
        &self,
        state: openvm_circuit::arch::VmStateMut<
            F,
            openvm_circuit::system::memory::online::TracingMemory,
            MatrixRecordArena<F>,
        >,
        instruction: &openvm_instructions::instruction::Instruction<F>,
    ) -> Result<(), openvm_circuit::arch::ExecutionError> {
        todo!()
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
        todo!()
    }
}

use openvm_circuit::arch::VmCircuitConfig;
use openvm_circuit::arch::VmExecutionConfig;
use openvm_stark_sdk::config::baby_bear_poseidon2::BabyBearPoseidon2Engine;

impl PowdrExecutor {
    pub fn new(
        air_by_opcode_id: OriginalAirs<BabyBear>,
        // memory: Arc<Mutex<TracingMemory>>,
        base_config: ExtendedVmConfig,
        periphery: PowdrPeripheryInstances,
        apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    ) -> Self {
        Self {
            air_by_opcode_id,
            chip_inventory: Mutex::new({
                let airs: AirInventory<BabyBearSC> =
                    create_dummy_airs(&base_config.sdk_vm_config, periphery.dummy.clone())
                        .expect("Failed to create dummy airs");

                create_dummy_chip_complex(&base_config.sdk_vm_config, airs, periphery.dummy)
                    .expect("Failed to create chip complex")
                    .inventory
            }),
            executor_inventory: base_config.sdk_vm_config.create_executors().unwrap(),
            number_of_calls: 0,
            periphery: periphery.real,
            apc,
        }
    }

    pub fn number_of_calls(&self) -> usize {
        self.number_of_calls
    }

    pub fn execute(
        &self,
        state: openvm_circuit::arch::VmStateMut<
            BabyBear,
            TracingMemory,
            MatrixRecordArena<BabyBear>,
        >,
    ) -> Result<(), ExecutionError> {
        // Extract the state components, since `execute` consumes the state but we need to pass it to each instruction execution
        let VmStateMut {
            pc,
            memory,
            streams,
            rng,
            custom_pvs,
            ctx,
        } = state;

        // save the next available `RecordId`
        // let from_record_id = state.memory.get_memory_logs().len();
        // execute the original instructions one by one
        for instruction in self.apc.instructions().iter() {
            let executor = self
                .executor_inventory
                .get_executor(instruction.0.opcode)
                .unwrap();
            use openvm_circuit::arch::PreflightExecutor;
            let state = VmStateMut {
                pc,
                memory,
                streams,
                rng,
                custom_pvs,
                ctx,
            };
            executor.execute(state, &instruction.0)?;
        }

        // self.number_of_calls += 1;
        // let memory_logs = state.memory.get_memory_logs(); // exclusive range

        // let to_record_id = memory_logs.len();

        // let last_read_write = memory_logs[from_record_id..to_record_id]
        //     .iter()
        //     .rposition(|entry| {
        //         matches!(
        //             entry,
        //             MemoryLogEntry::Read { .. } | MemoryLogEntry::Write { .. }
        //         )
        //     })
        //     .map(|idx| idx + from_record_id);

        // tracing::trace!(
        //     "APC range (exclusive): {}..{} (last read/write at {})",
        //     from_record_id,
        //     to_record_id,
        //     last_read_write.unwrap_or(to_record_id)
        // );

        Ok(())
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness(&self) -> RowMajorMatrix<BabyBear> {
        let dummy_trace_by_air_name: HashMap<String, Trace<BabyBear>> = self
            .executor_inventory
            .executors
            .iter()
            .map(|executor| {
                // let air_name = get_name::<SC>(executor.air());
                // let DenseMatrix { values, width, .. } =
                //     *tracing::debug_span!("dummy trace", air_name = air_name.clone()).in_scope(
                //         || {
                //             Chip::generate_proving_ctx(&executor, unimplemented!())
                //                 .common_main
                //                 .unwrap()
                //         },
                //     );
                // (air_name.clone(), Trace::new(values, width))
                unimplemented!()
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            apc_poly_id_to_index,
            columns_to_compute,
        } = generate_trace::<OriginalAirs<BabyBear>>(
            &dummy_trace_by_air_name,
            // &self.air_by_opcode_id,
            unimplemented!("not Sync because of RefCell"),
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
        let mut values = <BabyBear as FieldAlgebra>::zero_vec(height * width);

        // go through the final table and fill in the values
        values
            // a record is `width` values
            // TODO: optimize by parallelizing on chunks of rows, currently fails because `dyn AnyChip<MatrixRecordArena<Val<SC>>>` is not `Senf`
            .chunks_mut(width)
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
                        let ConcreteBusInteraction { mult, .. } =
                            evaluator.eval_bus_interaction(interaction);
                        for _ in 0..mult.as_canonical_u32() {
                            // TODO: remove count is not implemented in openvm 1.4.0
                            // self.periphery.range_checker.remove_count(
                            //     args.next().unwrap().as_canonical_u32(),
                            //     args.next().unwrap().as_canonical_u32() as usize,
                            // );
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
                        ComputationMethod::Constant(c) => BabyBear::from_canonical_u64(*c),
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

fn create_dummy_airs(
    config: &SdkVmConfig,
    shared_chips: SharedPeripheryChips,
) -> Result<AirInventory<BabyBearSC>, AirInventoryError> {
    let config = config.to_inner();
    let mut inventory = config.system.create_airs()?;

    // CHANGE: add dummy periphery
    inventory.start_new_extension();
    VmCircuitExtension::extend_circuit(&shared_chips, &mut inventory)?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmCircuitExtension::extend_circuit(rv32i, &mut inventory)?;
    }
    if let Some(io) = &config.io {
        VmCircuitExtension::extend_circuit(io, &mut inventory)?;
    }
    if let Some(keccak) = &config.keccak {
        VmCircuitExtension::extend_circuit(keccak, &mut inventory)?;
    }
    if let Some(sha256) = &config.sha256 {
        VmCircuitExtension::extend_circuit(sha256, &mut inventory)?;
    }
    if let Some(native) = &config.native {
        VmCircuitExtension::extend_circuit(native, &mut inventory)?;
    }
    if let Some(castf) = &config.castf {
        VmCircuitExtension::extend_circuit(castf, &mut inventory)?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmCircuitExtension::extend_circuit(rv32m, &mut inventory)?;
    }
    if let Some(bigint) = &config.bigint {
        VmCircuitExtension::extend_circuit(bigint, &mut inventory)?;
    }
    if let Some(modular) = &config.modular {
        VmCircuitExtension::extend_circuit(modular, &mut inventory)?;
    }
    if let Some(fp2) = &config.fp2 {
        VmCircuitExtension::extend_circuit(fp2, &mut inventory)?;
    }
    if let Some(pairing) = &config.pairing {
        VmCircuitExtension::extend_circuit(pairing, &mut inventory)?;
    }
    if let Some(ecc) = &config.ecc {
        VmCircuitExtension::extend_circuit(ecc, &mut inventory)?;
    }
    Ok(inventory)
}

fn create_dummy_chip_complex(
    config: &SdkVmConfig,
    circuit: AirInventory<BabyBearSC>,
    shared_chips: SharedPeripheryChips,
) -> Result<DummyChipComplex<BabyBearSC>, ChipInventoryError> {
    let config = config.to_inner();
    let mut chip_complex = VmBuilder::<BabyBearPoseidon2Engine>::create_chip_complex(
        &SystemCpuBuilder,
        &config.system,
        circuit,
    )?;
    let inventory = &mut chip_complex.inventory;

    // CHANGE: inject the periphery chips so that they are not created by the extensions. This is done for memory footprint: the dummy periphery chips are thrown away anyway, so we reuse a single one for all APCs.
    VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
        &shared_chips,
        &shared_chips,
        inventory,
    )?;
    // END CHANGE

    if let Some(rv32i) = &config.rv32i {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            rv32i,
            inventory,
        )?;
    }
    if let Some(io) = &config.io {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            io,
            inventory,
        )?;
    }
    if let Some(keccak) = &config.keccak {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Keccak256CpuProverExt,
            keccak,
            inventory,
        )?;
    }
    if let Some(sha256) = &config.sha256 {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Sha2CpuProverExt,
            sha256,
            inventory,
        )?;
    }
    if let Some(native) = &config.native {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &NativeCpuProverExt,
            native,
            inventory,
        )?;
    }
    if let Some(castf) = &config.castf {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &NativeCpuProverExt,
            castf,
            inventory,
        )?;
    }
    if let Some(rv32m) = &config.rv32m {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Rv32ImCpuProverExt,
            rv32m,
            inventory,
        )?;
    }
    if let Some(bigint) = &config.bigint {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &Int256CpuProverExt,
            bigint,
            inventory,
        )?;
    }
    if let Some(modular) = &config.modular {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &AlgebraCpuProverExt,
            modular,
            inventory,
        )?;
    }
    if let Some(fp2) = &config.fp2 {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &AlgebraCpuProverExt,
            fp2,
            inventory,
        )?;
    }
    if let Some(pairing) = &config.pairing {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &PairingProverExt,
            pairing,
            inventory,
        )?;
    }
    if let Some(ecc) = &config.ecc {
        VmProverExtension::<BabyBearPoseidon2Engine, _, _>::extend_prover(
            &EccCpuProverExt,
            ecc,
            inventory,
        )?;
    }

    Ok(chip_complex)
}
