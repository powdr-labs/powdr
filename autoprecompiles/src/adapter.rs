use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::fmt::Display;
use std::hash::Hash;

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{BasicBlock, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    memory_optimizer::MemoryBusInteraction,
    range_constraint_optimizer::RangeConstraintHandler,
    Apc, InstructionHandler, PowdrConfig, VmConfig,
};

pub struct AdapterApcWithStats<A: Adapter> {
    apc: AdapterApc<A>,
    stats: Option<ApcStats<A>>,
}
impl<A: Adapter> AdapterApcWithStats<A> {
    pub fn with_stats(mut self, stats: ApcStats<A>) -> Self {
        self.stats = Some(stats);
        self
    }

    pub fn into_parts(self) -> (AdapterApc<A>, Option<ApcStats<A>>) {
        (self.apc, self.stats)
    }
}

impl<A: Adapter> From<AdapterApc<A>> for AdapterApcWithStats<A> {
    fn from(apc: AdapterApc<A>) -> Self {
        Self { apc, stats: None }
    }
}

pub trait PgoAdapter {
    type Adapter: Adapter;

    fn filter_blocks_and_create_apcs_with_pgo(
        &self,
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        let filtered_blocks = blocks
            .into_iter()
            .filter(|block| !Self::Adapter::should_skip_block(block))
            .collect();
        self.create_apcs_with_pgo(filtered_blocks, config, vm_config)
    }

    fn create_apcs_with_pgo(
        &self,
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>>;

    fn pc_execution_count(&self, _pc: u64) -> Option<u32> {
        None
    }
}

pub trait Adapter: Sized {
    type Field: Serialize + for<'de> Deserialize<'de> + Send + Clone;
    type PowdrField: FieldElement;
    type InstructionHandler: InstructionHandler<Self::Field, Self::Instruction> + Sync;
    type BusInteractionHandler: BusInteractionHandler<Self::PowdrField>
        + Clone
        + IsBusStateful<Self::PowdrField>
        + RangeConstraintHandler<Self::PowdrField>
        + Sync;
    type Program: Program<Self::Instruction> + Send;
    type Instruction: Instruction<Self::Field> + Serialize + for<'de> Deserialize<'de> + Send;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash>: MemoryBusInteraction<
        Self::PowdrField,
        V,
    >;
    type CustomBusTypes: Clone + Display + Sync + Eq + PartialEq;
    type ApcStats: Send + Sync;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    fn should_skip_block(_block: &BasicBlock<Self::Instruction>) -> bool {
        false
    }
}

pub type ApcStats<A> = <A as Adapter>::ApcStats;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type AdapterVmConfig<'a, A> = VmConfig<
    'a,
    <A as Adapter>::InstructionHandler,
    <A as Adapter>::BusInteractionHandler,
    <A as Adapter>::CustomBusTypes,
>;
