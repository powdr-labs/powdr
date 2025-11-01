use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::collections::BTreeMap;
use std::hash::Hash;
use std::{fmt::Display, sync::Arc};

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::evaluation::{AirMetrics, ApcPerformanceReport};
use crate::{
    blocks::{BasicBlock, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    memory_optimizer::MemoryBusInteraction,
    range_constraint_optimizer::RangeConstraintHandler,
    Apc, InstructionHandler, PowdrConfig, VmConfig,
};

#[derive(Serialize, Deserialize)]
pub struct ApcWithReport<F, I> {
    apc: Arc<Apc<F, I>>,
    report: ApcPerformanceReport,
}
impl<F, I> ApcWithReport<F, I> {
    pub fn new(apc: Arc<Apc<F, I>>, report: ApcPerformanceReport) -> Self {
        Self { apc, report }
    }

    pub fn into_parts(self) -> (Arc<Apc<F, I>>, ApcPerformanceReport) {
        (self.apc, self.report)
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
    ) -> Vec<AdapterApcWithReport<Self::Adapter>> {
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
    ) -> Vec<AdapterApcWithReport<Self::Adapter>>;

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
    type AirId: Eq + Hash + Send + Sync;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    fn should_skip_block(_block: &BasicBlock<Self::Instruction>) -> bool {
        false
    }

    fn get_apc_metrics(apc: Arc<AdapterApc<Self>>, max_constraint_degree: usize) -> AirMetrics;
}

pub type AdapterApcWithReport<A> = ApcWithReport<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type AdapterVmConfig<'a, A> = VmConfig<
    'a,
    <A as Adapter>::InstructionHandler,
    <A as Adapter>::BusInteractionHandler,
    <A as Adapter>::CustomBusTypes,
>;
