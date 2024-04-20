#![no_std]

/// We allow syscall implementations to use any of the parameter/temporary RISC-V registers: `a0-a7,t0-t6`.
/// Any of these can be used for passing arguments to the syscall, except for `t0(x5)` which is reserved for the syscall number.
/// By convention, arguments and return values are used in this given order
/// (e.g., if 7 registers are needed, use reg indexes `0..6`).
/// These are all _caller_ saved registers.
/// Thus, registers directly used as input/output to the ecall shouldn't need any special handling.
/// The syscall implementation itself may use the other registers for temporary storage.
/// * Since this usage is NOT visible from llvm, these must be explicitly saved and restored *
/// For an example, see the `poseidon_gl` syscall implementation in the riscv runtime.
pub static SYSCALL_REGISTERS: [&str; 14] = [
    "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x6", "x7", "x28", "x29", "x30", "x31",
];

// NB. Must be kept in sync with conversion trait implementations
/// Powdr RISCV syscalls
#[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
#[repr(u32)]
pub enum Syscall {
    Input = 0,
    DataIdentifier = 1,
    PrintChar = 2,
    PoseidonGL = 3,
}

impl core::fmt::Display for Syscall {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Syscall::Input => write!(f, "input"),
            Syscall::DataIdentifier => write!(f, "data_identifier"),
            Syscall::PrintChar => write!(f, "print_char"),
            Syscall::PoseidonGL => write!(f, "poseidon_gl"),
        }
    }
}

impl core::str::FromStr for Syscall {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "input" => Ok(Syscall::Input),
            "data_identifier" => Ok(Syscall::DataIdentifier),
            "print_char" => Ok(Syscall::PrintChar),
            "poseidon_gl" => Ok(Syscall::PoseidonGL),
            _ => Err(()),
        }
    }
}

impl From<Syscall> for u32 {
    fn from(val: Syscall) -> Self {
        val as u32
    }
}

impl TryFrom<u32> for Syscall {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Syscall::Input),
            1 => Ok(Syscall::DataIdentifier),
            2 => Ok(Syscall::PrintChar),
            3 => Ok(Syscall::PoseidonGL),
            _ => Err(()),
        }
    }
}
