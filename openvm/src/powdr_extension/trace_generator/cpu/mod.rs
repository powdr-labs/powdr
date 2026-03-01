/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;
/// The shared periphery chips used by the PowdrTraceGenerator
mod periphery;

pub use inventory::create_dummy_chip_complex;
pub use periphery::{new_periphery_instances, SharedPeripheryChipsCpu};
