use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::collections::BTreeMap;
use std::hash::Hash;
use std::{fmt::Display, sync::Arc};

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::blocks::{detect_superblocks, ExecutionBlocks, SuperBlock};
use crate::empirical_constraints::EmpiricalConstraints;
use crate::evaluation::EvaluationResult;
use crate::execution::{ExecutionState, OptimisticConstraints};
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
    stats: S,
    evaluation_result: EvaluationResult,
}
impl<F, I, A, V, S> ApcWithStats<F, I, A, V, S> {
    pub fn new(apc: Arc<Apc<F, I, A, V>>, stats: S, evaluation_result: EvaluationResult) -> Self {
        Self {
            apc,
            stats,
            evaluation_result,
        }
    }

    #[allow(clippy::type_complexity)]
    pub fn into_parts(self) -> (Arc<Apc<F, I, A, V>>, S, EvaluationResult) {
        (self.apc, self.stats, self.evaluation_result)
    }

    pub fn apc(&self) -> &Apc<F, I, A, V> {
        &self.apc
    }

    pub fn stats(&self) -> &S {
        &self.stats
    }

    pub fn evaluation_result(&self) -> EvaluationResult {
        self.evaluation_result
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
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        let blocks = if let Some(prof) = self.execution_profile() {
            detect_superblocks::<Self::Adapter>(config, &prof.pc_list, blocks)
        } else {
            let superblocks = blocks
                .into_iter()
                .map(SuperBlock::from)
                .filter(|sb| !Self::Adapter::should_skip_block(sb))
                // filter invalid APC candidates
                .filter(|sb| sb.statements().count() > 1)
                .collect();
            ExecutionBlocks::new_without_pgo(superblocks)
        };

        self.create_apcs_with_pgo(blocks, config, vm_config, labels, empirical_constraints)
    }

    fn create_apcs_with_pgo(
        &self,
        exec_blocks: AdapterExecutionBlocks<Self::Adapter>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
        empirical_constraints: EmpiricalConstraints,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>>;

    fn execution_profile(&self) -> Option<&ExecutionProfile> {
        None
    }

    fn pc_execution_count(&self, pc: u64) -> Option<u32> {
        self.execution_profile()
            .and_then(|prof| prof.pc_count.get(&pc).cloned())
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
    type CustomBusTypes: Clone
        + Display
        + Sync
        + Eq
        + PartialEq
        + Serialize
        + for<'de> Deserialize<'de>;
    type ApcStats: Send + Sync;
    type AirId: Eq + Hash + Send + Sync;
    type ExecutionState: ExecutionState;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    /// Given the autoprecompile and the original instructions, return the stats
    fn apc_stats(
        apc: Arc<AdapterApc<Self>>,
        instruction_handler: &Self::InstructionHandler,
    ) -> Self::ApcStats;

    fn should_skip_block(_block: &SuperBlock<Self::Instruction>) -> bool {
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
pub type AdapterSuperBlock<A> = SuperBlock<<A as Adapter>::Instruction>;
pub type AdapterExecutionBlocks<A> = ExecutionBlocks<<A as Adapter>::Instruction>;
