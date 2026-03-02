/// The inventory of the PowdrExecutor, which contains the executors for each opcode.
mod inventory;

pub use inventory::create_dummy_chip_complex;
pub use powdr_openvm_common::trace_generator::cpu::periphery::{
    new_periphery_instances, SharedPeripheryChipsCpu,
};
