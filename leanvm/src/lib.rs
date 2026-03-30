pub mod bus_interaction_handler;
pub mod instruction;
pub mod instruction_handler;
pub mod memory_bus_interaction;
pub mod symbolic_machines;
pub mod test_utils;
pub mod wom_memory_bus_interaction;

use std::fmt::Display;
use std::hash::Hash;
use std::sync::Arc;

use powdr_autoprecompiles::adapter::{Adapter, AdapterApc};
use powdr_autoprecompiles::blocks::Program;
use powdr_autoprecompiles::bus_map::{BusMap, BusType};
use powdr_autoprecompiles::execution::ExecutionState;
use powdr_number::BabyBearField;
use serde::{Deserialize, Serialize};

use bus_interaction_handler::LeanVmBusInteractionHandler;
use instruction::{LeanVmInstruction, LeanVmOpcode};
use instruction_handler::LeanVmInstructionHandler;
use memory_bus_interaction::LeanVmMemoryBusInteraction;
use symbolic_machines::{EXEC_BUS_ID, MEMORY_BUS_ID, PC_LOOKUP_BUS_ID};
use wom_memory_bus_interaction::LeanVmWomMemoryBusInteraction;

// Re-exports for tests
pub use instruction::{add, deref, jump, mul};
pub use instruction_handler::DEFAULT_DEGREE_BOUND;

/// Empty enum — LeanVM has no custom bus types beyond the standard ones.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum LeanVmCustomBusType {}

impl Display for LeanVmCustomBusType {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {}
    }
}

/// Stub program type (not needed for snapshot tests).
pub struct LeanVmProgram;

impl Program<LeanVmInstruction<BabyBearField>> for LeanVmProgram {
    fn base_pc(&self) -> u64 {
        0
    }

    fn instructions(&self) -> Box<dyn Iterator<Item = LeanVmInstruction<BabyBearField>> + '_> {
        Box::new(std::iter::empty())
    }

    fn length(&self) -> u32 {
        0
    }
}

/// Minimal execution state stub.
pub struct LeanVmExecutionState;

impl ExecutionState for LeanVmExecutionState {
    type RegisterAddress = ();
    type Value = u64;

    fn pc(&self) -> Self::Value {
        unimplemented!()
    }

    fn value_limb(_value: Self::Value, _limb_index: usize) -> Self::Value {
        unimplemented!()
    }

    fn reg(&self, _address: &Self::RegisterAddress) -> Self::Value {
        unimplemented!()
    }

    fn global_clk(&self) -> usize {
        unimplemented!()
    }
}

pub struct LeanVmAdapter;

impl Adapter for LeanVmAdapter {
    // TODO: Change to KoalaBearField
    type Field = BabyBearField;
    type PowdrField = BabyBearField;
    type InstructionHandler = LeanVmInstructionHandler;
    type BusInteractionHandler = LeanVmBusInteractionHandler;
    type Program = LeanVmProgram;
    type Instruction = LeanVmInstruction<BabyBearField>;
    type MemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> = LeanVmMemoryBusInteraction<V>;
    type WomMemoryBusInteraction<V: Ord + Clone + Eq + Display + Hash> =
        LeanVmWomMemoryBusInteraction<V>;
    type CustomBusTypes = LeanVmCustomBusType;
    type ApcStats = ();
    type AirId = String;
    type ExecutionState = LeanVmExecutionState;

    fn into_field(e: Self::PowdrField) -> Self::Field {
        e
    }

    fn from_field(e: Self::Field) -> Self::PowdrField {
        e
    }

    fn apc_stats(
        _apc: Arc<AdapterApc<Self>>,
        _instruction_handler: &Self::InstructionHandler,
    ) -> Self::ApcStats {
    }

    fn is_branching(instr: &Self::Instruction) -> bool {
        instr.opcode == LeanVmOpcode::Jump
    }

    fn is_allowed(_instr: &Self::Instruction) -> bool {
        true
    }
}

pub fn leanvm_bus_map() -> BusMap<LeanVmCustomBusType> {
    BusMap::from_id_type_pairs(vec![
        (EXEC_BUS_ID, BusType::ExecutionBridge),
        (MEMORY_BUS_ID, BusType::Memory),
        (PC_LOOKUP_BUS_ID, BusType::PcLookup),
    ])
}
