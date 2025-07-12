use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{Candidate, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    InstructionMachineHandler,
};

pub trait Adapter: Sized {
    type Field: Serialize + for<'de> Deserialize<'de> + Send;
    type PowdrField: FieldElement;
    type InstructionMachineHandler: InstructionMachineHandler<Self::Field> + Clone + Sync;
    type BusInteractionHandler: BusInteractionHandler<Self::PowdrField>
        + Clone
        + IsBusStateful<Self::PowdrField>
        + Sync;
    type Candidate: Candidate<Self> + Send;
    type Program: Program<Self::Field, Self::Instruction> + Send;
    type Instruction: Instruction<Self::Field> + Serialize + for<'de> Deserialize<'de> + Send;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;
}
