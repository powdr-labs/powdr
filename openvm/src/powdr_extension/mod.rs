/// The core logic of our extension
pub mod chip;
/// The executor for the powdr instructions
pub mod executor;
/// The opcodes for the powdr instructions, which is used in the chip implementation and contains the opcode ID
pub mod opcode;
/// The integration of our extension with the VM
mod vm;

mod plonk;

pub use opcode::PowdrOpcode;
pub use vm::{PowdrExtension, PowdrExtensionExecutor, PowdrPrecompile};
