use std::collections::HashMap;

use powdr_autoprecompiles::evaluation::AirStats;
use powdr_autoprecompiles::symbolic_machine::SymbolicMachine;
use powdr_autoprecompiles::InstructionHandler;
use powdr_number::BabyBearField;

use crate::instruction::{LeanVmInstruction, LeanVmOpcode};
use crate::symbolic_machines::build_machine;

pub use powdr_autoprecompiles::DegreeBound;

pub const DEFAULT_DEGREE_BOUND: DegreeBound = DegreeBound {
    identities: 4,
    bus_interactions: 2,
};

pub struct LeanVmInstructionHandler {
    machines: HashMap<LeanVmOpcode, SymbolicMachine<BabyBearField>>,
    degree_bound: DegreeBound,
}

impl LeanVmInstructionHandler {
    pub fn new(degree_bound: DegreeBound) -> Self {
        let opcodes = [
            LeanVmOpcode::Add,
            LeanVmOpcode::Mul,
            LeanVmOpcode::Deref,
            LeanVmOpcode::Jump,
        ];
        let machines = opcodes
            .into_iter()
            .map(|op| (op, build_machine(op)))
            .collect();
        Self {
            machines,
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
        let opcode = instruction.opcode;
        (opcode, &self.machines[&opcode])
    }

    fn get_instruction_air_stats(&self, instruction: &Self::Instruction) -> AirStats {
        AirStats::new(&self.machines[&instruction.opcode])
    }
}
