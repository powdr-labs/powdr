/// The core logic of our extension
pub mod chip;
/// The opcodes for the powdr instructions, which is used in the chip implementation and contains the opcode ID
pub mod opcode;
/// The integration of our extension with the VM
mod vm;

mod plonk_chip;

pub use opcode::PowdrOpcode;
pub use vm::{OriginalInstruction, PowdrExecutor, PowdrExtension, PowdrPeriphery, PowdrPrecompile};
