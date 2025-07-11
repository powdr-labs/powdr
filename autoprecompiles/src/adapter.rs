use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{blocks::{Candidate, Instruction, Program}, constraint_optimizer::IsBusStateful, InstructionMachineHandler};

pub trait Adapter<P: FieldElement>: Sized {
    type Field: From<P> + Into<P>;
    type InstructionMachineHandler: InstructionMachineHandler<P> + Clone + Sync;
    type BusInteractionHandler: BusInteractionHandler<P> + Clone + IsBusStateful<P> + Sync;
    type Candidate: Candidate<P, Self> + Send;
    type Program: Program<Self::Field> + Send;
}