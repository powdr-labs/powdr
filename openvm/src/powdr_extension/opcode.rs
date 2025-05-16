use openvm_instructions::LocalOpcode;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Deserialize, Serialize)]
pub struct PowdrOpcode {
    pub class_offset: usize,
}

impl LocalOpcode for PowdrOpcode {
    // This offset must not be accessed, since we want many opcodes of the same type to have different class_offsets.
    // This is because each opcode has its own air.
    const CLASS_OFFSET: usize = unreachable!();

    fn from_usize(value: usize) -> Self {
        Self {
            class_offset: value,
        }
    }

    // The local offset is always 0, since we want to have many opcodes over the same air.
    fn local_usize(&self) -> usize {
        0
    }

    // The global opcode is based on `class_offset`, *NOT* on the static `CLASS_OFFSET`.
    fn global_opcode(&self) -> openvm_instructions::VmOpcode {
        openvm_instructions::VmOpcode::from_usize(self.class_offset)
    }
}
