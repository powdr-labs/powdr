use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{Candidate, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    InstructionMachineHandler, SymbolicInstructionStatement,
};

pub trait Adapter: Sized {
    type Field;
    type PowdrField: FieldElement;
    type InstructionMachineHandler: InstructionMachineHandler<Self::PowdrField> + Clone + Sync;
    type BusInteractionHandler: BusInteractionHandler<Self::PowdrField>
        + Clone
        + IsBusStateful<Self::PowdrField>
        + Sync;
    type Candidate: Candidate<Self> + Send;
    type Program: Program<Self::Field, Self::Instruction> + Send;
    type Instruction: Instruction<Self::Field> + Serialize + for<'de> Deserialize<'de> + Send;

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    fn into_symbolic_instruction(
        instr: &Self::Instruction,
    ) -> SymbolicInstructionStatement<Self::PowdrField>;
}
