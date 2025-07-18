use powdr_constraint_solver::constraint_system::BusInteractionHandler;
use powdr_number::FieldElement;
use serde::{Deserialize, Serialize};

use crate::{
    blocks::{Candidate, Instruction, Program},
    constraint_optimizer::IsBusStateful,
    Apc, InstructionHandler,
};

pub trait Adapter: Sized + Sync {
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

    fn into_field(e: Self::PowdrField) -> Self::Field;

    fn from_field(e: Self::Field) -> Self::PowdrField;

    /// Returns the base program counter.
    fn base_pc(&self) -> u64;

    /// Returns the step size of the program counter.
    fn pc_step(&self) -> u32;

    /// Creates a new instance of the adapter from the given program.
    fn new(program: &Self::Program) -> Self;
}

pub type ApcStats<A> = <<A as Adapter>::Candidate as Candidate<A>>::ApcStats;
pub type AdapterApc<A> = Apc<<A as Adapter>::Field, <A as Adapter>::Instruction>;
