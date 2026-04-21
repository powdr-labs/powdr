use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::InstructionHandler;
use powdr_number::KoalaBearField;

use crate::instruction::LeanVmInstruction;
use crate::symbolic_machines::build_execution_machine;

pub use powdr_autoprecompiles::DegreeBound;

// TODO: According to Claude, it's 2 for bus interactions, 5 for execution constraints,
// 6 for extension ops, and 9 for Poseidon.
// For now, we set both to 9, so that the optimizer has a chance to optimize Poseidon constraints.
// Once we optimize away memory accesses to store temporary values, we can lower the bus
// interaction degree bound.
pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: 9,
    bus_interactions: 9,
};

/// LeanVM has a single AIR for the execution table.
/// All instructions share the same SymbolicMachine — opcode selectors gate
/// which constraints are active.
pub struct LeanVmInstructionHandler {
    /// The single execution table AIR, shared by all instructions.
    machine: SymbolicMachine<KoalaBearField>,
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
    type Field = KoalaBearField;
    type Instruction = LeanVmInstruction;
    type AirId = String;

    fn degree_bound(&self) -> DegreeBound {
        self.degree_bound
    }

    fn get_instruction_air_and_id(
        &self,
        _instruction: &Self::Instruction,
    ) -> (Self::AirId, &SymbolicMachine<Self::Field>) {
        // All instructions use the same single AIR.
        ("Main".to_string(), &self.machine)
    }

    fn get_instruction_air_stats(&self, _instruction: &Self::Instruction) -> AirStats {
        AirStats::new(&self.machine)
    }
}
