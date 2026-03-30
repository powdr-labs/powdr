use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::InstructionHandler;
use powdr_number::BabyBearField;

use crate::instruction::{LeanVmInstruction, LeanVmOpcode};
use crate::symbolic_machines::build_execution_machine;

pub use powdr_autoprecompiles::DegreeBound;

// TODO: Is this correct?
pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: 5,
    bus_interactions: 2,
};

/// LeanVM has a single AIR for the execution table.
/// All instructions share the same SymbolicMachine — opcode selectors gate
/// which constraints are active.
pub struct LeanVmInstructionHandler {
    /// The single execution table AIR, shared by all instructions.
    machine: SymbolicMachine<BabyBearField>,
    degree_bound: DegreeBound,
}

impl LeanVmInstructionHandler {
    pub fn new(degree_bound: DegreeBound) -> Self {
        Self {
            machine: build_execution_machine(),
            degree_bound,
        }
    }
}

impl InstructionHandler for LeanVmInstructionHandler {
    type Field = BabyBearField;
    type Instruction = LeanVmInstruction<BabyBearField>;
    type AirId = LeanVmOpcode;

    fn degree_bound(&self) -> DegreeBound {
        self.degree_bound
    }

    fn get_instruction_air_and_id(
        &self,
        instruction: &Self::Instruction,
    ) -> (Self::AirId, &SymbolicMachine<Self::Field>) {
        // All instructions use the same single AIR.
        (instruction.opcode, &self.machine)
    }

    fn get_instruction_air_stats(&self, _instruction: &Self::Instruction) -> AirStats {
        AirStats::new(&self.machine)
    }
}
