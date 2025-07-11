use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::FieldElement;

use crate::{
    blocks::{Candidate, Program},
    constraint_optimizer::IsBusStateful,
    InstructionMachineHandler, SymbolicInstructionStatement,
};

pub trait Adapter<P: FieldElement>: Sized {
    type Field;
    type InstructionMachineHandler: InstructionMachineHandler<P> + Clone + Sync;
    type BusInteractionHandler: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Sync;
    type Candidate: Candidate<P, Self> + Send;
    type Program: Program<Self::Field> + Send;

    fn into_field(e: P) -> Self::Field;

    fn from_field(e: Self::Field) -> P;

    fn into_symbolic_instruction(
        instr: &<Self::Program as Program<Self::Field>>::Instruction,
    ) -> SymbolicInstructionStatement<P>;
}
