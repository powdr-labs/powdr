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
use openvm_circuit_primitives::AlignedBytesBorrow;
use openvm_instructions::instruction::Instruction;
use openvm_sdk::config::SdkVmConfigExecutor;
use openvm_stark_backend::p3_field::Field;
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
    pub record_arena_by_air_name: Rc<RefCell<OriginalArenas>>,
}

#[derive(Default)]
pub enum OriginalArenas {
    #[default]
    Uninitialized,
    Initialized(InitializedOriginalArenas),
}

impl OriginalArenas {
    fn initialize(
        &mut self,
        apc_call_count_estimate: usize,
        original_airs: &OriginalAirs<BabyBear>,
        apc: &Arc<Apc<BabyBear, Instr<BabyBear>>>,
    ) {
        match self {
            OriginalArenas::Uninitialized => {
                println!(
                    "initialize originalarenas for apc with start pc: {:?}",
                    apc.start_pc()
                );
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

#[derive(Default)]
pub struct InitializedOriginalArenas {
    pub arenas: HashMap<String, MatrixRecordArena<BabyBear>>,
    pub number_of_calls: usize,
}

impl InitializedOriginalArenas {
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
                            num_calls,
                            air_width,
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
            // This is the actual number of calls, which we still don't know yet at this point
            // We only have `apc_call_count_estimate`, which is `next_power_of_two(number_of_calls)`, if using `MatrixRecordArena`
            number_of_calls: 0,
        }
    }
}

pub struct RecordArenaDimension {
    pub num_calls: usize,
    pub air_width: usize,
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
        // TODO: do we know `MeteredCtx` is correct? It's only one implementation of `MeteredExecutionCtxTrait`.
        size_of::<E2PreCompute<PowdrPreCompute<MeteredCtx>>>()
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

        // TODO: we can parallelize this now?
        *data = PowdrPreCompute {
            original_instructions: self
                .apc
                .instructions()
                .iter()
                .enumerate()
                .map(|(idx, instruction)| {
                    let executor = self
                        .executor_inventory
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

        // Initialize the original arenas if not already initialized
        let mut record_arena_by_air_name = self.record_arena_by_air_name.as_ref().borrow_mut();

        // println!("record_arena_by_air_name keys: {:?}", record_arena_by_air_name.arenas().keys());

        println!(
            "original_ctx trace_buffer len: {:?}",
            original_ctx.trace_buffer.len()
        );
        println!("original_ctx width: {:?}", original_ctx.width);
        println!("apc block: {:?}", self.apc.block);

        println!("execute apc with start pc: {:?}", self.apc.start_pc());
        record_arena_by_air_name.initialize(
            // initialized height, not available as API, is really `next_power_of_two(apc_call_count)`, if using `MatrixRecordArena` impl of `RA`
            original_ctx.trace_buffer.len() / original_ctx.width,
            &self.air_by_opcode_id,
            &self.apc,
        );

        let arenas = record_arena_by_air_name.arenas_mut();

        println!("arena air_names: {:?}", arenas.keys());

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

            println!(
                "air_name to fetch: {:?} apc start pc: {:?}",
                air_name,
                self.apc.start_pc()
            );

            let state = VmStateMut {
                pc,
                memory,
                streams,
                rng,
                custom_pvs,
                // We execute in the context of the relevant dummy table
                ctx: arenas.get_mut(&air_name).unwrap(),
            };

            executor.execute(state, &instruction.0)?;
        }

        *record_arena_by_air_name.number_of_calls_mut() += 1;

        // After execution, put back the original ctx
        // TODO: `original_ctx` might just be useless and tossed away, so there's no need to put it back?
        // `original_ctx` is the trace initialized for APC, but we really only generate the dummy traces here and assemble them to APC trace in `generate_witness`
        // state.ctx = original_ctx;

        Ok(())
    }

    fn get_opcode_name(&self, opcode: usize) -> String {
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
            record_arena_by_air_name,
        }
    }
}
