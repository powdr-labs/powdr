use std::{
    borrow::{Borrow, BorrowMut},
    collections::HashMap,
    sync::Arc,
};

use crate::{
    bus_map::DEFAULT_VARIABLE_RANGE_CHECKER,
    extraction_utils::OriginalAirs,
    powdr_extension::executor::{inventory::DummyChipComplex, periphery::SharedPeripheryChips},
    BabyBearSC, ExtendedVmConfig, Instr,
};

use openvm_algebra_circuit::AlgebraCpuProverExt;
use openvm_bigint_circuit::Int256CpuProverExt;
use openvm_circuit::arch::{
    execution_mode::{ExecutionCtx, MeteredCostCtx},
    E2PreCompute, PreflightExecutor,
};
use openvm_circuit_primitives::AlignedBytesBorrow;
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
        AirInventory, AirInventoryError, ChipInventory, ChipInventoryError, ExecuteFunc,
        ExecutionCtxTrait, ExecutionError, Executor, ExecutorInventory, MeteredExecutionCtxTrait,
        MeteredExecutor, StaticProgramError, VmBuilder, VmCircuitExtension, VmExecState,
        VmProverExtension, VmStateMut,
    },
    system::{
        memory::online::{GuestMemory, TracingMemory},
        SystemCpuBuilder,
    },
};
use openvm_circuit::{
    arch::{Arena, MatrixRecordArena},
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

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor {
    air_by_opcode_id: OriginalAirs<BabyBear>,
    chip_inventory: ChipInventory<BabyBearSC, MatrixRecordArena<BabyBear>, CpuBackend<BabyBearSC>>,
    executor_inventory: ExecutorInventory<SdkVmConfigExecutor<BabyBear>>,
    number_of_calls: usize,
    periphery: SharedPeripheryChips,
    apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    record_arena_by_air_name: HashMap<String, MatrixRecordArena<BabyBear>>,
}

#[derive(AlignedBytesBorrow, Clone)]
#[repr(C)]
struct PowdrPreCompute<Ctx> {
    original_instructions: Vec<(ExecuteFunc<BabyBear, Ctx>, Vec<u8>)>,
}

impl Executor<BabyBear> for PowdrExecutor {
    // Note: this is only used in `get_pre_compute_max_size` in OVM to pre allocate buffers for precomputed instructions.
    // `get_pre_compute_max_size` takes the max of the pre compute sizes of all instructions (including Powdr instruction) and pad to next power of two.
    // Then, `alloc_pre_compute_buf` allocates the calculated max size * length of the program.
    // TODO: it might be a concern that Powdr pre_compute_size might be too long, as it aggregates those of all original instructions.
    fn pre_compute_size(&self) -> usize {
        // TODO: do we know `ExecutionCtx` is correct? It's only one implementation of `ExecutionCtxTrait`.
        size_of::<PowdrPreCompute<ExecutionCtx>>()
    }

    fn pre_compute<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<ExecuteFunc<BabyBear, Ctx>, StaticProgramError>
    where
        Ctx: ExecutionCtxTrait,
    {
        let pre_compute: &mut PowdrPreCompute<Ctx> = data.borrow_mut();

        self.pre_compute_impl::<Ctx>(pc, inst, pre_compute)?;

        Ok(execute_e1_impl::<Ctx>)
    }
}

impl MeteredExecutor<BabyBear> for PowdrExecutor {
    // TODO: it might be a concern that Powdr metered_pre_compute_size might be too long, as it aggregates those of all original instructions.
    fn metered_pre_compute_size(&self) -> usize {
        // TODO: do we know `MeteredCostCtx` is correct? It's only one implementation of `MeteredExecutionCtxTrait`.
        size_of::<E2PreCompute<PowdrPreCompute<MeteredCostCtx>>>()
    }

    fn metered_pre_compute<Ctx>(
        &self,
        chip_idx: usize,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<ExecuteFunc<BabyBear, Ctx>, StaticProgramError>
    where
        Ctx: MeteredExecutionCtxTrait,
    {
        let pre_compute: &mut E2PreCompute<PowdrPreCompute<Ctx>> = data.borrow_mut();
        pre_compute.chip_idx = chip_idx as u32;

        self.pre_compute_impl::<Ctx>(pc, inst, &mut pre_compute.data)?;

        Ok(execute_e2_impl::<Ctx>)
    }
}

impl PowdrExecutor {
    #[inline]

    fn pre_compute_impl<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut PowdrPreCompute<Ctx>,
    ) -> Result<(), StaticProgramError>
    where
        Ctx: ExecutionCtxTrait,
    {
        let &Instruction {
            a,
            b,
            c,
            d,
            e,
            f,
            g,
            ..
        } = inst;

        // TODO: assert that the opcode is the one we expect

        if !a.is_zero()
            || !b.is_zero()
            || !c.is_zero()
            || !d.is_zero()
            || !e.is_zero()
            || !f.is_zero()
            || !g.is_zero()
        {
            return Err(StaticProgramError::InvalidInstruction(pc));
        }

        *data = PowdrPreCompute {
            original_instructions: self
                .apc
                .instructions()
                .iter()
                .map(|instruction| {
                    let executor = self
                        .executor_inventory
                        .get_executor(instruction.0.opcode)
                        .ok_or(StaticProgramError::InvalidInstruction(pc))?;
                    let pre_compute_size = executor.pre_compute_size();
                    let mut pre_compute_data = vec![0u8; pre_compute_size];
                    let execute_func =
                        executor.pre_compute::<Ctx>(pc, &instruction.0, &mut pre_compute_data)?;
                    Ok((execute_func, pre_compute_data.to_vec()))
                })
                .collect::<Result<Vec<_>, StaticProgramError>>()?,
        };

        Ok(())
    }
}

#[inline(always)]
unsafe fn execute_e12_impl<CTX: ExecutionCtxTrait>(
    pre_compute: &PowdrPreCompute<CTX>,
    vm_state: &mut VmExecState<BabyBear, GuestMemory, CTX>,
) {
    pre_compute
        .original_instructions
        .iter()
        .fold(vm_state, |vm_state, (executor, data)| {
            executor(data, vm_state);
            vm_state
        });
}

unsafe fn execute_e1_impl<CTX: ExecutionCtxTrait>(
    pre_compute: &[u8],
    vm_state: &mut VmExecState<BabyBear, GuestMemory, CTX>,
) {
    let pre_compute: &PowdrPreCompute<CTX> = pre_compute.borrow();
    execute_e12_impl::<CTX>(pre_compute, vm_state);
}

unsafe fn execute_e2_impl<CTX: MeteredExecutionCtxTrait>(
    pre_compute: &[u8],
    vm_state: &mut VmExecState<BabyBear, GuestMemory, CTX>,
) {
    let pre_compute: &E2PreCompute<PowdrPreCompute<CTX>> = pre_compute.borrow();
    vm_state
        .ctx
        .on_height_change(pre_compute.chip_idx as usize, 1);
    execute_e12_impl::<CTX>(&pre_compute.data, vm_state);
}

impl PreflightExecutor<BabyBear> for PowdrExecutor {
    fn execute(
        &self,
        state: VmStateMut<BabyBear, TracingMemory, MatrixRecordArena<BabyBear>>,
        instruction: &Instruction<BabyBear>,
    ) -> Result<(), ExecutionError> {
        // This is pretty much done, just need to move up from `execute()` below with very small modifications
        // Extract the state components, since `execute` consumes the state but we need to pass it to each instruction execution
        let VmStateMut {
            pc,
            memory,
            streams,
            rng,
            custom_pvs,
            // Swap off the original ctx
            // TODO: `original_ctx` here is initialized for the APC at the start of preflight execution but probably is useless and just tossed away
            ctx: original_ctx,
        } = state;

        // Create dummy record arenas by air name, initialized with number of original air calls as height and original air width as width
        let mut record_arena_by_air_name: HashMap<String, MatrixRecordArena<BabyBear>> = self
            .apc
            .instructions()
            .iter()
            .fold(HashMap::new(), |mut acc, instruction| {
                let air_name = self
                    .air_by_opcode_id
                    .get_instruction_air_and_id(instruction)
                    .0;
                // TODO: main_columns might not be correct, as the RA::with_capacity() uses the following `main_width()`
                // pub fn main_width(&self) -> usize {
                //     self.cached_mains.iter().sum::<usize>() + self.common_main
                // }
                let air_width = self
                    .air_by_opcode_id
                    .get_instruction_air_stats(instruction)
                    .main_columns;

                acc.entry(air_name.clone()).or_insert((0, 0)).0 += 1;
                acc.entry(air_name).or_insert((0, 0)).1 = air_width;
                acc
            })
            .into_iter()
            .map(|(air_name, (num_calls, air_width))| {
                (
                    air_name,
                    MatrixRecordArena::with_capacity(num_calls, air_width),
                )
            })
            .collect();

        // execute the original instructions one by one
        for instruction in self.apc.instructions().iter() {
            let executor = self
                .executor_inventory
                .get_executor(instruction.0.opcode)
                .unwrap();

            // TODO: this chunk is a bit repetitive
            let air_name = self
                .air_by_opcode_id
                .get_instruction_air_and_id(instruction)
                .0;

            let state = VmStateMut {
                pc,
                memory,
                streams,
                rng,
                custom_pvs,
                // Use dummy record arena
                // Note: this has the effect of isolating APC original instruction records to the ctx object here.
                ctx: record_arena_by_air_name.get_mut(&air_name).unwrap(),
            };

            executor.execute(state, &instruction.0)?;
        }

        // After execution, put back the original ctx
        // TODO: `original_ctx` might just be useless and tossed away, so there's no need to put it back?
        // `original_ctx` is the trace initialized for APC, but we really only generate the dummy traces here and assemble them to APC trace in `generate_witness`
        // state.ctx = original_ctx;

        // Add dummy record arena to PowdrExecutor for `generate_proving_ctx` later
        // TODO: self is immutable in `PreflightExecutor::execute`, so I'm not sure how we can mutate PowdrExecutor here
        // self.record_arena_by_air_name
        //     .extend(record_arena_by_air_name);

        // self.number_of_calls += 1;

        Ok(())
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
            chip_inventory: {
                let airs: AirInventory<BabyBearSC> =
                    create_dummy_airs(&base_config.sdk_vm_config, periphery.dummy.clone())
                        .expect("Failed to create dummy airs");

                create_dummy_chip_complex(&base_config.sdk_vm_config, airs, periphery.dummy)
                    .expect("Failed to create chip complex")
                    .inventory
            },
            executor_inventory: base_config.sdk_vm_config.create_executors().unwrap(),
            number_of_calls: 0,
            periphery: periphery.real,
            apc,
            record_arena_by_air_name: Default::default(),
        }
    }

    pub fn number_of_calls(&self) -> usize {
        self.number_of_calls
    }

    /// Generates the witness for the autoprecompile. The result will be a matrix of
    /// size `next_power_of_two(number_of_calls) * width`, where `width` is the number of
    /// nodes in the APC circuit.
    pub fn generate_witness(&self) -> RowMajorMatrix<BabyBear> {
        assert_eq!(
            self.number_of_calls, 0,
            "program is not modified to run apcs yet, so this should be zero"
        );

        let dummy_trace_by_air_name: HashMap<String, Trace<BabyBear>> = self
            .executor_inventory
            .executors
            .iter()
            .enumerate()
            .map(|(executor_id, _)| {
                let insertion_index =
                    self.chip_inventory.executor_idx_to_insertion_idx[executor_id];
                let air_ref = &self.chip_inventory.airs().ext_airs()[insertion_index];
                let air_name = air_ref.name();

                // TODO: fetch the record arena from `PowdrExecutor.record_arena_by_air_name: HashMap<String, MatrixRecordArena<BabyBear>>`
                // then call generate_proving_ctx on it and the corresponding executor

                // let DenseMatrix { values, width, .. } =
                //     *tracing::debug_span!("dummy trace", air_name = air_name.clone())
                //         .in_scope(|| Chip::generate_proving_ctx(&executor, unimplemented!("HashMap<air_name, MatrixRecordArena<BabyBear>>")).common_main.unwrap());

                // TODO: replace the empty trace by the result of calling `generate_proving_ctx` above
                let values = vec![];
                let width = air_ref.width();

                (air_name, Trace::new(values, width))
            })
            .collect();

        let TraceData {
            dummy_values,
            dummy_trace_index_to_apc_index_by_instruction,
            apc_poly_id_to_index,
            columns_to_compute,
        } = generate_trace::<OriginalAirs<BabyBear>>(
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
