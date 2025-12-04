use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::collections::BTreeMap;
use std::hash::Hash;
use std::{fmt::Display, sync::Arc};

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{BasicBlock, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    memory_optimizer::MemoryBusInteraction,
    range_constraint_optimizer::RangeConstraintHandler,
    Apc, InstructionHandler, PowdrConfig, VmConfig,
};

#[derive(Serialize, Deserialize)]
pub struct ApcWithStats<F, I, S> {
    apc: Arc<Apc<F, I>>,
    stats: Option<S>,
}
impl<F, I, S> ApcWithStats<F, I, S> {
    pub fn with_stats(mut self, stats: S) -> Self {
        self.stats = Some(stats);
        self
    }

    pub fn into_parts(self) -> (Arc<Apc<F, I>>, Option<S>) {
        (self.apc, self.stats)
    }
}

impl<F, I, S> From<Arc<Apc<F, I>>> for ApcWithStats<F, I, S> {
    fn from(apc: Arc<Apc<F, I>>) -> Self {
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
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>> {
        let filtered_blocks = blocks
            .into_iter()
            .filter(|block| !Self::Adapter::should_skip_block(block))
            .collect();
        self.create_apcs_with_pgo(filtered_blocks, config, vm_config, labels)
    }

    fn create_apcs_with_pgo(
        &self,
        blocks: Vec<BasicBlock<<Self::Adapter as Adapter>::Instruction>>,
        config: &PowdrConfig,
        vm_config: AdapterVmConfig<Self::Adapter>,
        labels: BTreeMap<u64, Vec<String>>,
    ) -> Vec<AdapterApcWithStats<Self::Adapter>>;

    fn pc_execution_count(&self, _pc: u64) -> Option<u32> {
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

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    fn should_skip_block(_block: &BasicBlock<Self::Instruction>) -> bool {
        false
    }
}

pub type AdapterApcWithStats<A> =
    ApcWithStats<<A as Adapter>::Field, <A as Adapter>::Instruction, <A as Adapter>::ApcStats>;
pub type ApcStats<A> = <A as Adapter>::ApcStats;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type AdapterVmConfig<'a, A> = VmConfig<
    'a,
    <A as Adapter>::InstructionHandler,
    <A as Adapter>::BusInteractionHandler,
    <A as Adapter>::CustomBusTypes,
>;
