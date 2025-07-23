use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use std::fmt::Display;
use std::hash::Hash;

use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{Candidate, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    memory_optimizer::MemoryBusInteraction,
    Apc, InstructionHandler, VmConfig,
};

pub trait Adapter: Sized {
    type Field: Serialize + for<'de> Deserialize<'de> + Send + Clone;
    type PowdrField: FieldElement;
    type InstructionHandler: InstructionHandler<Self::Field, Self::Instruction> + Sync;
    type BusInteractionHandler: BusInteractionHandler<Self::PowdrField>
        + Clone
        + IsBusStateful<Self::PowdrField>
        + Sync;
    type Candidate: Candidate<Self> + Send;
    type Program: Program<Self::Instruction> + Send;
    type Instruction: Instruction<Self::Field> + Serialize + for<'de> Deserialize<'de> + Send;
    type MemoryBusInteraction<T: FieldElement, V: Ord + Clone + Eq + Display + Hash>: MemoryBusInteraction<T, V>;
    type CustomBusTypes: Clone + Display + Sync + Eq + PartialEq;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;
}

pub type ApcStats<A> = <<A as Adapter>::Candidate as Candidate<A>>::ApcStats;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type AdapterVmConfig<'a, A> = VmConfig<
    'a,
    <A as Adapter>::InstructionHandler,
    <A as Adapter>::BusInteractionHandler,
    <A as Adapter>::CustomBusTypes,
>;
