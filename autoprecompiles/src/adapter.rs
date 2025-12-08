use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;
use std::{fmt::Display, sync::Arc};

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::execution::{ExecutionState, OptimisticConstraints};
use crate::blocks::generate_superblocks;
use crate::execution_profile::ExecutionProfile;

use crate::{
    blocks::{BasicBlock, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    memory_optimizer::MemoryBusInteraction,
    range_constraint_optimizer::RangeConstraintHandler,
    Apc, InstructionHandler, PowdrConfig, VmConfig,
};

#[derive(Serialize, Deserialize)]
pub struct ApcWithStats<F, I, A, V, S> {
    apc: Arc<Apc<F, I, A, V>>,
    stats: Option<S>,
}
impl<F, I, A, V, S> ApcWithStats<F, I, A, V, S> {
    pub fn with_stats(mut self, stats: S) -> Self {
        self.stats = Some(stats);
        self
    }

    #[allow(clippy::type_complexity)]
    pub fn into_parts(self) -> (Arc<Apc<F, I, A, V>>, Option<S>) {
        (self.apc, self.stats)
    }
}

impl<F, I, A, V, S> From<Arc<Apc<F, I, A, V>>> for ApcWithStats<F, I, A, V, S> {
    fn from(apc: Arc<Apc<F, I, A, V>>) -> Self {
        Self { apc, stats: None }
    }
}

pub trait PgoAdapter {
    type Adapter: Adapter;

    fn filter_blocks_and_create_apcs_with_pgo(
        &self,
        blocks: Vec<AdapterBasicBlock<Self::Adapter>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        let filtered_blocks: Vec<_> = blocks
            .into_iter()
            .filter(|block| !Self::Adapter::should_skip_block(block))
            .collect();

        // generate superblocks if profiling data is available
        let (blocks, execution_count) = if let Some(prof) = self.profiling_data() {
            let max_superblock_len = 3;
            // generate_superblocks already filters out unexecuted blocks and single instruction blocks
            let (blocks, count) = generate_superblocks(&prof.pc_list, &filtered_blocks, max_superblock_len);
            (blocks, Some(count))
        } else {
            (filtered_blocks, None)
        };

        self.create_apcs_with_pgo(blocks, execution_count, config, vm_config, labels)
    }

    fn create_apcs_with_pgo(
        &self,
        blocks: Vec<AdapterBasicBlock<Self::Adapter>>,
        block_exec_count: Option<HashMap<usize, u32>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>>;

    fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        self.profiling_data().map(|prof| prof.pc_count[&pc])
    }

    fn profiling_data(&self) -> Option<&ExecutionProfile> {
        None
    }
}

pub trait Adapter: Sized
where
    Self::InstructionHandler:
        InstructionHandler<Field = Self::Field, Instruction = Self::Instruction>,
{
    type Field: Serialize + for<'de> Deserialize<'de> + Send + Sync + Clone;
    type PowdrField: FieldElement;
    type InstructionHandler: InstructionHandler + Sync;
    type BusInteractionHandler: BusInteractionHandler<Self::PowdrField>
        + Clone
        + IsBusStateful<Self::PowdrField>
        + RangeConstraintHandler<Self::PowdrField>
        + Sync;
    type Program: Program<Self::Instruction> + Send;
    type Instruction: Instruction<Self::Field> + Serialize + for<'de> Deserialize<'de> + Send + Sync;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash>: MemoryBusInteraction<
        Self::PowdrField,
        V,
    >;
    type CustomBusTypes: Clone + Display + Sync + Eq + PartialEq;
    type ApcStats: Send + Sync;
    type AirId: Eq + Hash + Send + Sync;
    type ExecutionState: ExecutionState;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    fn should_skip_block(_block: &BasicBlock<Self::Instruction>) -> bool {
        false
    }
}

pub type AdapterApcWithStats<A> = ApcWithStats<
    <A as Adapter>::Field,
    <A as Adapter>::Instruction,
    <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
    <<A as Adapter>::ExecutionState as ExecutionState>::Value,
    <A as Adapter>::ApcStats,
>;
pub type ApcStats<A> = <A as Adapter>::ApcStats;
pub type AdapterApc<A> = Apc<
    <A as Adapter>::Field,
    <A as Adapter>::Instruction,
    <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
    <<A as Adapter>::ExecutionState as ExecutionState>::Value,
>;
pub type AdapterApcOverPowdrField<A> = Apc<
    <A as Adapter>::PowdrField,
    <A as Adapter>::Instruction,
    <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
    <<A as Adapter>::ExecutionState as ExecutionState>::Value,
>;
pub type AdapterVmConfig<'a, A> = VmConfig<
    'a,
    <A as Adapter>::InstructionHandler,
    <A as Adapter>::BusInteractionHandler,
    <A as Adapter>::CustomBusTypes,
>;
pub type AdapterExecutionState<A> = <A as Adapter>::ExecutionState;
pub type AdapterOptimisticConstraints<A> = OptimisticConstraints<
    <<A as Adapter>::ExecutionState as ExecutionState>::RegisterAddress,
    <<A as Adapter>::ExecutionState as ExecutionState>::Value,
>;
pub type AdapterBasicBlock<A> = BasicBlock<<A as Adapter>::Instruction>;
