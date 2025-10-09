use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::Arc,
};

use crate::{
    extraction_utils::{
        record_arena_dimension_by_air_name_per_apc_call, OriginalAirs, OriginalVmConfig,
    },
    Instr,
};

use openvm_circuit::arch::{
    execution_mode::{ExecutionCtx, MeteredCtx},
    E2PreCompute, PreflightExecutor,
};
use openvm_circuit_derive::create_tco_handler;
use openvm_circuit_primitives::AlignedBytesBorrow;
use openvm_instructions::instruction::Instruction;
use openvm_sdk::config::SdkVmConfigExecutor;
use openvm_stark_backend::{
    p3_field::{Field, PrimeField32},
    p3_maybe_rayon::prelude::{IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator},
};
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::Apc;

use openvm_circuit::arch::{Arena, MatrixRecordArena};
use openvm_circuit::{
    arch::{
        ExecuteFunc, ExecutionCtxTrait, ExecutionError, Executor, ExecutorInventory,
        MeteredExecutionCtxTrait, MeteredExecutor, StaticProgramError, VmExecState, VmStateMut,
    },
    system::memory::online::{GuestMemory, TracingMemory},
};
use powdr_autoprecompiles::InstructionHandler;
/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
pub struct PowdrExecutor {
    pub air_by_opcode_id: OriginalAirs<BabyBear>,
    pub executor_inventory: ExecutorInventory<SdkVmConfigExecutor<BabyBear>>,
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
    pub original_arenas: Rc<RefCell<OriginalArenas>>,
}

/// A shared mutable reference to the arenas used to store the traces of the original instructions, accessed during preflight execution and trace generation.
/// The same reference is reused for all segments, under the assumption that segments are executed sequentially: preflight_0, tracegen_0, preflight_1, tracegen_1, ...
/// It goes through the following cycle for each segment:
/// - initialized at the beginning of preflight execution with the correct sizes for this segment
/// - written to during preflight execution
/// - read from during trace generation
/// - reset to uninitialized after trace generation
#[derive(Default)]
pub enum OriginalArenas {
    #[default]
    Uninitialized,
    Initialized(InitializedOriginalArenas),
}

impl OriginalArenas {
    /// Given an estimate of how many times the APC is called in this segment, and the original airs and apc,
    /// initializes the arenas iff not already initialized.
    fn ensure_initialized(
        &mut self,
        apc_call_count_estimate: usize,
        original_airs: &OriginalAirs<BabyBear>,
        apc: &Arc<Apc<BabyBear, Instr<BabyBear>>>,
    ) {
        match self {
            OriginalArenas::Uninitialized => {
                *self = OriginalArenas::Initialized(InitializedOriginalArenas::new(
                    apc_call_count_estimate,
                    original_airs,
                    apc,
                ));
            }
            OriginalArenas::Initialized(_) => {}
        }
    }

    /// Returns a mutable reference to the arenas.
    /// Should only be called after `initialize` is called.
    pub fn arenas_mut(&mut self) -> &mut HashMap<String, MatrixRecordArena<BabyBear>> {
        match self {
            OriginalArenas::Uninitialized => unreachable!(),
            OriginalArenas::Initialized(initialized) => &mut initialized.arenas,
        }
    }

    pub fn arenas(&self) -> &HashMap<String, MatrixRecordArena<BabyBear>> {
        match self {
            OriginalArenas::Uninitialized => unreachable!(),
            OriginalArenas::Initialized(initialized) => &initialized.arenas,
        }
    }

    /// Returns a mutable reference to the number of calls.
    /// Should only be called after `initialize` is called.
    pub fn number_of_calls_mut(&mut self) -> &mut usize {
        match self {
            OriginalArenas::Uninitialized => unreachable!(),
            OriginalArenas::Initialized(initialized) => &mut initialized.number_of_calls,
        }
    }

    /// Returns the number of calls. If not initialized, `Preflight::execute` is never called, and thus return 0.
    pub fn number_of_calls(&self) -> usize {
        match self {
            OriginalArenas::Uninitialized => 0,
            OriginalArenas::Initialized(initialized) => initialized.number_of_calls,
        }
    }
}

/// A collection of arenas used to store the records of the original instructions, one per air name.
/// Each arena is initialized with a capacity based on an estimate of how many times the APC is called in this segment
/// and how many calls to each air are made per APC call.
#[derive(Default)]
pub struct InitializedOriginalArenas {
    pub arenas: HashMap<String, MatrixRecordArena<BabyBear>>,
    pub number_of_calls: usize,
}

impl InitializedOriginalArenas {
    /// Creates a new instance of `InitializedOriginalArenas`.
    pub fn new(
        apc_call_count_estimate: usize,
        original_airs: &OriginalAirs<BabyBear>,
        apc: &Arc<Apc<BabyBear, Instr<BabyBear>>>,
    ) -> Self {
        let record_arena_dimensions =
            record_arena_dimension_by_air_name_per_apc_call(apc, original_airs);
        Self {
            arenas: record_arena_dimensions
                .iter()
                .map(
                    |(
                        air_name,
                        RecordArenaDimension {
                            height: num_calls,
                            width: air_width,
                        },
                    )| {
                        (
                            air_name.clone(),
                            MatrixRecordArena::with_capacity(
                                *num_calls * apc_call_count_estimate,
                                *air_width,
                            ),
                        )
                    },
                )
                .collect(),
            // This is the actual number of calls, which we don't know yet. It will be updated during preflight execution.
            number_of_calls: 0,
        }
    }
}

/// The dimensions of a record arena for a given air name, used to initialize the arenas.
pub struct RecordArenaDimension {
    pub height: usize,
    pub width: usize,
}

/// A struct to interpret the pre-compute data as for PowdrExecutor.
#[derive(AlignedBytesBorrow, Clone)]
#[repr(C)]
struct PowdrPreCompute<F, Ctx> {
    original_instructions: Vec<(ExecuteFunc<F, Ctx>, Vec<u8>)>,
}

impl Executor<BabyBear> for PowdrExecutor {
    fn pre_compute_size(&self) -> usize {
        // TODO: do we know `ExecutionCtx` is correct? It's only one implementation of `ExecutionCtxTrait`.
        // A clean fix would be to add `Ctx` as a generic parameter to this method in the `Executor` trait, but that would be a breaking change.
        size_of::<PowdrPreCompute<BabyBear, ExecutionCtx>>()
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
        let pre_compute: &mut PowdrPreCompute<BabyBear, Ctx> = data.borrow_mut();

        self.pre_compute_impl::<Ctx>(pc, inst, pre_compute)?;

        Ok(execute_e1_impl::<BabyBear, Ctx>)
    }

    #[cfg(feature = "tco")]
    fn handler<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<openvm_circuit::arch::Handler<BabyBear, Ctx>, StaticProgramError>
    where
        Ctx: ExecutionCtxTrait,
    {
        let pre_compute: &mut PowdrPreCompute<BabyBear, Ctx> = data.borrow_mut();
        self.pre_compute_impl::<Ctx>(pc, inst, pre_compute)?;
        Ok(execute_e1_tco_handler::<BabyBear, Ctx>)
    }
}

impl MeteredExecutor<BabyBear> for PowdrExecutor {
    fn metered_pre_compute_size(&self) -> usize {
        // TODO: do we know `MeteredCtx` is correct? It's only one implementation of `MeteredExecutionCtxTrait`.
        // A clean fix would be to add `Ctx` as a generic parameter to this method in the `MeteredExecutor` trait, but that would be a breaking change.
        size_of::<E2PreCompute<PowdrPreCompute<BabyBear, MeteredCtx>>>()
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
        let pre_compute: &mut E2PreCompute<PowdrPreCompute<BabyBear, Ctx>> = data.borrow_mut();
        pre_compute.chip_idx = chip_idx as u32;

        self.pre_compute_impl::<Ctx>(pc, inst, &mut pre_compute.data)?;

        Ok(execute_e2_impl::<BabyBear, Ctx>)
    }

    #[cfg(feature = "tco")]
    fn metered_handler<Ctx>(
        &self,
        chip_idx: usize,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut [u8],
    ) -> Result<openvm_circuit::arch::Handler<BabyBear, Ctx>, StaticProgramError>
    where
        Ctx: MeteredExecutionCtxTrait,
    {
        let pre_compute: &mut E2PreCompute<PowdrPreCompute<BabyBear, Ctx>> = data.borrow_mut();
        pre_compute.chip_idx = chip_idx as u32;

        self.pre_compute_impl::<Ctx>(pc, inst, &mut pre_compute.data)?;

        Ok(execute_e2_tco_handler::<BabyBear, Ctx>)
    }
}

impl PowdrExecutor {
    /// The implementation of pre_compute, shared between Executor and MeteredExecutor.
    #[inline]
    fn pre_compute_impl<Ctx>(
        &self,
        pc: u32,
        inst: &Instruction<BabyBear>,
        data: &mut PowdrPreCompute<BabyBear, Ctx>,
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

        // TODO: debug_assert that the opcode is the one we expect

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

        let executor_inventory = &self.executor_inventory;
        // Set the data using the original instructions
        *data = PowdrPreCompute {
            original_instructions: self
                .apc
                .block
                .statements
                .par_iter()
                .enumerate()
                .map(|(idx, instruction)| {
                    let executor = executor_inventory
                        .get_executor(instruction.0.opcode)
                        .ok_or(StaticProgramError::ExecutorNotFound {
                            opcode: instruction.0.opcode,
                        })?;
                    let pre_compute_size = executor.pre_compute_size();
                    let mut pre_compute_data = vec![0u8; pre_compute_size];
                    let execute_func = executor.pre_compute::<Ctx>(
                        pc + idx as u32 * 4,
                        &instruction.0,
                        &mut pre_compute_data,
                    )?;
                    Ok((execute_func, pre_compute_data.to_vec()))
                })
                .collect::<Result<Vec<_>, StaticProgramError>>()?,
        };

        Ok(())
    }
}

/// The implementation of the execute function, shared between Executor and MeteredExecutor.
#[inline(always)]
unsafe fn execute_e12_impl<F, CTX: ExecutionCtxTrait>(
    pre_compute: &PowdrPreCompute<F, CTX>,
    vm_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    // Save the current instret, as we will overwrite it during execution of original instructions
    let instret = vm_state.vm_state.instret;
    let vm_state =
        pre_compute
            .original_instructions
            .iter()
            .fold(vm_state, |vm_state, (executor, data)| {
                executor(data, vm_state);
                vm_state
            });
    // Restore the instret and increment it by one, since we executed a single apc instruction
    vm_state.vm_state.instret = instret + 1;
}

#[create_tco_handler]
unsafe fn execute_e1_impl<F: PrimeField32, CTX: ExecutionCtxTrait>(
    pre_compute: &[u8],
    vm_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    let pre_compute: &PowdrPreCompute<F, CTX> = pre_compute.borrow();
    execute_e12_impl::<F, CTX>(pre_compute, vm_state);
}

#[create_tco_handler]
unsafe fn execute_e2_impl<F: PrimeField32, CTX: MeteredExecutionCtxTrait>(
    pre_compute: &[u8],
    vm_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    let pre_compute: &E2PreCompute<PowdrPreCompute<F, CTX>> = pre_compute.borrow();
    vm_state
        .ctx
        .on_height_change(pre_compute.chip_idx as usize, 1);
    execute_e12_impl::<F, CTX>(&pre_compute.data, vm_state);
}

impl PreflightExecutor<BabyBear> for PowdrExecutor {
    fn execute(
        &self,
        state: VmStateMut<BabyBear, TracingMemory, MatrixRecordArena<BabyBear>>,
        _: &Instruction<BabyBear>,
    ) -> Result<(), ExecutionError> {
        // Extract the state components, since `execute` consumes the state but we need to pass it to each instruction execution
        let VmStateMut {
            pc,
            memory,
            streams,
            rng,
            custom_pvs,
            ctx,
            #[cfg(feature = "metrics")]
            metrics,
        } = state;

        // Initialize the original arenas if not already initialized
        let mut original_arenas = self.original_arenas.as_ref().borrow_mut();

        original_arenas.ensure_initialized(
            // Recover an estimate of how many times the APC is called in this segment based on the current ctx height and width
            ctx.trace_buffer.len() / ctx.width,
            &self.air_by_opcode_id,
            &self.apc,
        );

        let arenas = original_arenas.arenas_mut();

        // execute the original instructions one by one
        for instruction in self.apc.instructions() {
            let executor = self
                .executor_inventory
                .get_executor(instruction.0.opcode)
                .unwrap();

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
                // We execute in the context of the relevant original table
                ctx: arenas.get_mut(&air_name).unwrap(),
                // TODO: should we pass around the same metrics object, or snapshot it at the beginning of this method and apply a single update at the end?
                #[cfg(feature = "metrics")]
                metrics,
            };

            executor.execute(state, &instruction.0)?;
        }

        // Update the real number of calls to the APC
        *original_arenas.number_of_calls_mut() += 1;

        Ok(())
    }

    fn get_opcode_name(&self, _opcode: usize) -> String {
        todo!()
    }
}

use openvm_circuit::arch::VmExecutionConfig;

impl PowdrExecutor {
    pub fn new(
        air_by_opcode_id: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        apc: Arc<Apc<BabyBear, Instr<BabyBear>>>,
        record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
    ) -> Self {
        Self {
            air_by_opcode_id,
            executor_inventory: base_config.sdk_config.sdk.create_executors().unwrap(),
            apc,
            original_arenas: record_arena_by_air_name,
        }
    }
}
