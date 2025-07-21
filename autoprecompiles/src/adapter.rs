use powdr_constraint_solver::{
    constraint_system::BusInteractionHandler, grouped_expression::GroupedExpression,
};
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{Candidate, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    expression::AlgebraicReference,
    memory_optimizer::MemoryBusInteraction,
    Apc, InstructionHandler,
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
    type MemoryBusInteraction: MemoryBusInteraction<Self::PowdrField, AlgebraicReference>;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;
}

pub type ApcStats<A> = <<A as Adapter>::Candidate as Candidate<A>>::ApcStats;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
pub type GroupedExpressionPowdr<A> =
    GroupedExpression<<A as Adapter>::PowdrField, AlgebraicReference>;
