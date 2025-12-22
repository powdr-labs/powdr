use std::{
    borrow::{Borrow, BorrowMut},
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    sync::Arc,
};

use crate::{
    customize_exe::{OpenVmExecutionState, OpenVmRegisterAddress},
    extraction_utils::{
        record_arena_dimension_by_air_name_per_apc_call, OriginalAirs, OriginalVmConfig,
    },
    Instr,
};

use itertools::Itertools;
use openvm_circuit::arch::InterpreterMeteredExecutor;
use openvm_circuit::arch::{
    execution_mode::{ExecutionCtx, MeteredCtx},
    Arena, DenseRecordArena, E2PreCompute, InterpreterExecutor, MatrixRecordArena,
    PreflightExecutor,
};
#[cfg(feature = "aot")]
use openvm_circuit::arch::{AotExecutor, AotMeteredExecutor};
use openvm_circuit_derive::create_handler;
use openvm_circuit_primitives::AlignedBytesBorrow;
use openvm_instructions::instruction::Instruction;
use openvm_sdk::config::SdkVmConfigExecutor;
use openvm_stark_backend::p3_field::PrimeField32;
use openvm_stark_sdk::p3_baby_bear::BabyBear;
use powdr_autoprecompiles::{
    execution::{OptimisticConstraintEvaluator, OptimisticConstraints},
    Apc,
};

use openvm_circuit::{
    arch::{
        ExecuteFunc, ExecutionCtxTrait, ExecutionError, ExecutorInventory,
        MeteredExecutionCtxTrait, StaticProgramError, VmExecState, VmExecutionConfig, VmStateMut,
    },
    system::memory::online::{GuestMemory, TracingMemory},
};

/// A struct which holds the state of the execution based on the original instructions in this block and a dummy inventory.
/// It holds arenas for each original use for both cpu and gpu execution, so that this struct can be agnostic to the execution backend.
/// When using the cpu backend, only `original_arenas_cpu` is used, and vice versa for gpu execution.
pub struct PowdrExecutor {
    pub air_by_opcode_id: OriginalAirs<BabyBear>,
    pub executor_inventory: ExecutorInventory<SdkVmConfigExecutor<BabyBear>>,
    pub apc: Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
    pub original_arenas_cpu: Rc<RefCell<OriginalArenas<MatrixRecordArena<BabyBear>>>>,
    pub original_arenas_gpu: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
    pub height_change: u32,
    cached_instructions_meta: Vec<CachedInstructionMeta>,
}

/// A shared mutable reference to the arenas used to store the traces of the original instructions, accessed during preflight execution and trace generation.
/// The same reference is reused for all segments, under the assumption that segments are executed sequentially: preflight_0, tracegen_0, preflight_1, tracegen_1, ...
/// It goes through the following cycle for each segment:
/// - initialized at the beginning of preflight execution with the correct sizes for this segment
/// - written to during preflight execution
/// - read from during trace generation
/// - reset to uninitialized after trace generation
#[derive(Default)]
pub enum OriginalArenas<A> {
    #[default]
    Uninitialized,
    Initialized(InitializedOriginalArenas<A>),
}

impl<A: Arena> OriginalArenas<A> {
    /// Given an estimate of how many times the APC is called in this segment, and the original airs and apc,
    /// initializes the arenas iff not already initialized.
    fn ensure_initialized(
        &mut self,
        apc_call_count_estimate: impl Fn() -> usize,
        original_airs: &OriginalAirs<BabyBear>,
        apc: &Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
    ) {
        match self {
            OriginalArenas::Uninitialized => {
                *self = OriginalArenas::Initialized(InitializedOriginalArenas::new(
                    apc_call_count_estimate(),
                    original_airs,
                    apc,
                ));
            }
            OriginalArenas::Initialized(_) => {}
        }
    }

    /// Returns a mutable reference to the arena of the given vector index.
    /// - Panics if the arenas are not initialized.
    pub fn arena_mut_by_index(&mut self, index: usize) -> &mut ArenaPair<A> {
        match self {
            OriginalArenas::Uninitialized => panic!("original arenas are uninitialized"),
            OriginalArenas::Initialized(initialized) => initialized.arena_mut_by_index(index),
        }
    }

    pub fn real_arena_mut_by_index(&mut self, index: usize) -> &mut A {
        match self {
            OriginalArenas::Uninitialized => panic!("original arenas are uninitialized"),
            OriginalArenas::Initialized(initialized) => initialized.real_arena_mut_by_index(index),
        }
    }

    pub fn dummy_arena_mut_by_index(&mut self, index: usize) -> &mut A {
        match self {
            OriginalArenas::Uninitialized => panic!("original arenas are uninitialized"),
            OriginalArenas::Initialized(initialized) => initialized.dummy_arena_mut_by_index(index),
        }
    }

    /// Returns the arena of the given air name.
    /// - Panics if the arenas are not initialized.
    pub fn take_real_arena(&mut self, air_name: &str) -> Option<A> {
        match self {
            OriginalArenas::Uninitialized => panic!("original arenas are uninitialized"),
            OriginalArenas::Initialized(initialized) => initialized.take_real_arena(air_name),
        }
    }

    /// Returns a mutable reference to the number of calls.
    /// - Panics if the arenas are not initialized.
    pub fn number_of_calls_mut(&mut self) -> &mut usize {
        match self {
            OriginalArenas::Uninitialized => panic!("original arenas are uninitialized"),
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
pub struct InitializedOriginalArenas<A> {
    arenas: Vec<Option<ArenaPair<A>>>,
    air_name_to_arena_index: HashMap<String, usize>,
    pub number_of_calls: usize,
}

impl<A: Arena> InitializedOriginalArenas<A> {
    /// Creates a new instance of `InitializedOriginalArenas`.
    pub fn new(
        apc_call_count_estimate: usize,
        original_airs: &OriginalAirs<BabyBear>,
        apc: &Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
    ) -> Self {
        let record_arena_dimensions =
            record_arena_dimension_by_air_name_per_apc_call(apc, original_airs);
        let (air_name_to_arena_index, arenas) =
            record_arena_dimensions.into_iter().enumerate().fold(
                (HashMap::new(), Vec::new()),
                |(mut air_name_to_arena_index, mut arenas),
                 (
                    idx,
                    (
                        air_name,
                        RecordArenaDimension {
                            real_height,
                            width: air_width,
                            dummy_height,
                        },
                    ),
                )| {
                    air_name_to_arena_index.insert(air_name, idx);
                    arenas.push(Some(ArenaPair {
                        real: A::with_capacity(real_height * apc_call_count_estimate, air_width),
                        dummy: A::with_capacity(dummy_height * apc_call_count_estimate, air_width),
                    }));
                    (air_name_to_arena_index, arenas)
                },
            );

        Self {
            arenas,
            air_name_to_arena_index,
            // This is the actual number of calls, which we don't know yet. It will be updated during preflight execution.
            number_of_calls: 0,
        }
    }

    #[inline]
    fn arena_mut_by_index(&mut self, index: usize) -> &mut ArenaPair<A> {
        self.arenas
            .get_mut(index)
            .and_then(|arena| arena.as_mut())
            .expect("arena missing for index")
    }

    #[inline]
    fn real_arena_mut_by_index(&mut self, index: usize) -> &mut A {
        &mut self.arena_mut_by_index(index).real
    }

    #[inline]
    fn dummy_arena_mut_by_index(&mut self, index: usize) -> &mut A {
        &mut self.arena_mut_by_index(index).dummy
    }

    fn take_real_arena(&mut self, air_name: &str) -> Option<A> {
        let index = *self.air_name_to_arena_index.get(air_name)?;
        self.arenas[index].take().map(|arena_pair| arena_pair.real)
    }
}

pub struct ArenaPair<A> {
    pub real: A,
    pub dummy: A,
}

/// The dimensions of a record arena for a given air name, used to initialize the arenas.
pub struct RecordArenaDimension {
    pub real_height: usize,
    pub width: usize,
    pub dummy_height: usize,
}

#[derive(Clone, Copy)]
struct CachedInstructionMeta {
    executor_index: usize,
    arena_index: usize,
    should_use_real_arena: bool,
}

/// A struct to interpret the pre-compute data as for PowdrExecutor.
#[derive(AlignedBytesBorrow, Clone)]
#[repr(C)]
struct PowdrPreCompute<F, Ctx> {
    height_change: u32,
    original_instructions: Vec<(ExecuteFunc<F, Ctx>, Vec<u8>)>,
    optimistic_constraints: Arc<OptimisticConstraints<OpenVmRegisterAddress, u32>>,
}

impl InterpreterExecutor<BabyBear> for PowdrExecutor {
    fn pre_compute_size(&self) -> usize {
        // TODO: do we know `ExecutionCtx` is correct? It's only one implementation of `ExecutionCtxTrait`.
        // A clean fix would be to add `Ctx` as a generic parameter to this method in the `Executor` trait, but that would be a breaking change.
        size_of::<PowdrPreCompute<BabyBear, ExecutionCtx>>()
    }

    #[cfg(not(feature = "tco"))]
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
        Ok(execute_e1_handler::<BabyBear, Ctx>)
    }
}

impl InterpreterMeteredExecutor<BabyBear> for PowdrExecutor {
    fn metered_pre_compute_size(&self) -> usize {
        // TODO: do we know `MeteredCtx` is correct? It's only one implementation of `MeteredExecutionCtxTrait`.
        // A clean fix would be to add `Ctx` as a generic parameter to this method in the `MeteredExecutor` trait, but that would be a breaking change.
        size_of::<E2PreCompute<PowdrPreCompute<BabyBear, MeteredCtx>>>()
    }

    #[cfg(not(feature = "tco"))]
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

        Ok(execute_e2_handler::<BabyBear, Ctx>)
    }
}

#[cfg(feature = "aot")]
impl AotExecutor<BabyBear> for PowdrExecutor {
    fn is_aot_supported(&self, _inst: &Instruction<BabyBear>) -> bool {
        false
    }

    fn generate_x86_asm(
        &self,
        _inst: &Instruction<BabyBear>,
        _pc: u32,
    ) -> Result<String, openvm_circuit::arch::AotError> {
        std::unimplemented!()
    }
}

#[cfg(feature = "aot")]
impl AotMeteredExecutor<BabyBear> for PowdrExecutor {
    fn is_aot_metered_supported(&self, _inst: &Instruction<BabyBear>) -> bool {
        false
    }

    fn generate_x86_metered_asm(
        &self,
        _inst: &Instruction<BabyBear>,
        _pc: u32,
        _chip_idx: usize,
        _config: &openvm_circuit::arch::SystemConfig,
    ) -> Result<String, openvm_circuit::arch::AotError> {
        std::unimplemented!()
    }
}

impl PowdrExecutor {
    #[cfg(not(feature = "tco"))]
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
        use openvm_instructions::program::DEFAULT_PC_STEP;
        use openvm_stark_backend::{
            p3_field::Field,
            p3_maybe_rayon::prelude::{
                IndexedParallelIterator, IntoParallelRefIterator, ParallelIterator,
            },
        };

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
            height_change: self.height_change,
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
                        pc + idx as u32 * DEFAULT_PC_STEP,
                        &instruction.0,
                        &mut pre_compute_data,
                    )?;
                    Ok((execute_func, pre_compute_data.to_vec()))
                })
                .collect::<Result<Vec<_>, StaticProgramError>>()?,
            optimistic_constraints: self.apc.optimistic_constraints.clone(),
        };

        Ok(())
    }

    #[cfg(feature = "tco")]
    /// The implementation of pre_compute, shared between Executor and MeteredExecutor.
    #[inline]
    fn pre_compute_impl<Ctx>(
        &self,
        _pc: u32,
        _inst: &Instruction<BabyBear>,
        _data: &mut PowdrPreCompute<BabyBear, Ctx>,
    ) -> Result<(), StaticProgramError> {
        unimplemented!("tco is not implemented yet")
    }
}

/// The implementation of the execute function, shared between Executor and MeteredExecutor.
#[inline(always)]
unsafe fn execute_e12_impl<F: PrimeField32, CTX: ExecutionCtxTrait>(
    pre_compute: &PowdrPreCompute<F, CTX>,
    exec_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    let mut optimistic_constraint_evalutator =
        OptimisticConstraintEvaluator::new(pre_compute.optimistic_constraints.clone());
    // Check the state before execution
    assert!(optimistic_constraint_evalutator
        .try_next_execution_step(&OpenVmExecutionState::from(&exec_state.vm_state))
        .is_ok());
    for (executor, data) in &pre_compute.original_instructions {
        executor(data.as_ptr(), exec_state);
        // Check the state after each original instruction
        assert!(optimistic_constraint_evalutator
            .try_next_execution_step(&OpenVmExecutionState::from(&exec_state.vm_state))
            .is_ok());
    }
}

#[create_handler]
unsafe fn execute_e1_impl<F: PrimeField32, CTX: ExecutionCtxTrait>(
    pre_compute: *const u8,
    exec_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    let pre_compute: &PowdrPreCompute<F, CTX> =
        std::slice::from_raw_parts(pre_compute, size_of::<PowdrPreCompute<F, CTX>>()).borrow();
    execute_e12_impl::<F, CTX>(pre_compute, exec_state);
}

#[create_handler]
unsafe fn execute_e2_impl<F: PrimeField32, CTX: MeteredExecutionCtxTrait>(
    pre_compute: *const u8,
    exec_state: &mut VmExecState<F, GuestMemory, CTX>,
) {
    let pre_compute: &E2PreCompute<PowdrPreCompute<F, CTX>> = std::slice::from_raw_parts(
        pre_compute,
        size_of::<E2PreCompute<PowdrPreCompute<F, CTX>>>(),
    )
    .borrow();
    exec_state.ctx.on_height_change(
        pre_compute.chip_idx as usize,
        pre_compute.data.height_change,
    );
    execute_e12_impl::<F, CTX>(&pre_compute.data, exec_state);
}

// Preflight execution is implemented separately for CPU and GPU backends, because they use a different arena from `self`
// TODO: reduce code duplication between the two implementations. The main issue now is we need to use the concrete arena types.
impl PreflightExecutor<BabyBear, MatrixRecordArena<BabyBear>> for PowdrExecutor {
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
        let mut original_arenas = self.original_arenas_cpu.as_ref().borrow_mut();

        // Recover an estimate of how many times the APC is called in this segment based on the current ctx height and width
        let apc_call_count = || ctx.trace_buffer.len() / ctx.width;

        original_arenas.ensure_initialized(apc_call_count, &self.air_by_opcode_id, &self.apc);
        // execute the original instructions one by one
        for (instruction, cached_meta) in self
            .apc
            .instructions()
            .iter()
            .zip_eq(&self.cached_instructions_meta)
        {
            let executor = &self.executor_inventory.executors[cached_meta.executor_index];

            let ctx_arena = if cached_meta.should_use_real_arena {
                original_arenas.real_arena_mut_by_index(cached_meta.arena_index)
            } else {
                original_arenas.dummy_arena_mut_by_index(cached_meta.arena_index)
            };

            let state = VmStateMut {
                pc,
                memory,
                streams,
                rng,
                custom_pvs,
                // We execute in the context of the relevant original table
                ctx: ctx_arena,
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

    fn get_opcode_name(&self, opcode: usize) -> String {
        format!("APC_{opcode}")
    }
}

// The GPU preflight executor implementation
impl PreflightExecutor<BabyBear, DenseRecordArena> for PowdrExecutor {
    fn execute(
        &self,
        state: VmStateMut<BabyBear, TracingMemory, DenseRecordArena>,
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
        let mut original_arenas = self.original_arenas_gpu.as_ref().borrow_mut();

        // Recover an (over)estimate of how many times the APC is called in this segment
        // Overestimate is fine because we can initailize dummy arenas with some extra space
        // Exact apc call count from execution is used in final tracegen regardless
        let apc_call_count = || {
            let apc_width = self.apc.machine().main_columns().count();
            let bytes_per_row = apc_width * std::mem::size_of::<u32>();
            let buf = ctx.records_buffer.get_ref();
            buf.len() / bytes_per_row
        };

        original_arenas.ensure_initialized(apc_call_count, &self.air_by_opcode_id, &self.apc);
        // execute the original instructions one by one
        for (instruction, cached_meta) in self
            .apc
            .instructions()
            .iter()
            .zip(&self.cached_instructions_meta)
        {
            let executor = &self.executor_inventory.executors[cached_meta.executor_index];

            let ctx_arena = if cached_meta.should_use_real_arena {
                original_arenas.real_arena_mut_by_index(cached_meta.arena_index)
            } else {
                original_arenas.dummy_arena_mut_by_index(cached_meta.arena_index)
            };

            let state = VmStateMut {
                pc,
                memory,
                streams,
                rng,
                custom_pvs,
                // We execute in the context of the relevant original table
                ctx: ctx_arena,
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

    fn get_opcode_name(&self, opcode: usize) -> String {
        format!("APC_{opcode}")
    }
}

impl PowdrExecutor {
    pub fn new(
        air_by_opcode_id: OriginalAirs<BabyBear>,
        base_config: OriginalVmConfig,
        apc: Arc<Apc<BabyBear, Instr<BabyBear>, OpenVmRegisterAddress, u32>>,
        record_arena_by_air_name_cpu: Rc<RefCell<OriginalArenas<MatrixRecordArena<BabyBear>>>>,
        record_arena_by_air_name_gpu: Rc<RefCell<OriginalArenas<DenseRecordArena>>>,
        height_change: u32,
    ) -> Self {
        let executor_inventory = base_config.sdk_config.sdk.create_executors().unwrap();

        let arena_index_by_name =
            record_arena_dimension_by_air_name_per_apc_call(apc.as_ref(), &air_by_opcode_id)
                .iter()
                .enumerate()
                .map(|(idx, (name, _))| (name.clone(), idx))
                .collect::<HashMap<_, _>>();

        let cached_instructions_meta = apc
            .instructions()
            .iter()
            .zip_eq(apc.subs.iter())
            .map(|(instruction, sub)| {
                let executor_index = *executor_inventory
                    .instruction_lookup
                    .get(&instruction.0.opcode)
                    .expect("missing executor for opcode")
                    as usize;
                let air_name = air_by_opcode_id
                    .opcode_to_air
                    .get(&instruction.0.opcode)
                    .expect("missing air for opcode");
                let arena_index = *arena_index_by_name
                    .get(air_name)
                    .expect("missing arena for air");
                CachedInstructionMeta {
                    executor_index,
                    arena_index,
                    should_use_real_arena: !sub.is_empty(),
                }
            })
            .collect();

        Self {
            air_by_opcode_id,
            executor_inventory,
            apc,
            original_arenas_cpu: record_arena_by_air_name_cpu,
            original_arenas_gpu: record_arena_by_air_name_gpu,
            height_change,
            cached_instructions_meta,
        }
    }
}
