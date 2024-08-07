#![no_std]

/// For parameter passing and return values in `ecall`, we allow use of all "caller saved" registers in RISC-V: `a0-a7,t0-t6`.
/// Register `t0` is reserved for passing the syscall number.
/// By convention, arguments and return values are used in this given order
/// (e.g., if 7 registers are needed, use reg indexes `0..6`).
/// Registers directly used as input/output to the ecall shouldn't need any special handling.
/// Any other register used inside the syscall implementation *must be explicitly saved and restored*,
/// as they are not visible from LLVM.
/// For an example, see the `poseidon_gl` syscall implementation in the riscv runtime.
pub static SYSCALL_REGISTERS: [&str; 14] = [
    "x10", "x11", "x12", "x13", "x14", "x15", "x16", "x17", "x6", "x7", "x28", "x29", "x30", "x31",
];

macro_rules! syscalls {
    ($(($num:expr, $identifier:ident, $name:expr)),* $(,)?) => {
        #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
        #[repr(u32)]
        pub enum Syscall {
            $($identifier = $num),*
        }

        impl core::fmt::Display for Syscall {
            fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
                write!(f, "{}", match self {
                    $(Syscall::$identifier => $name),*
                })
            }
        }

        impl core::str::FromStr for Syscall {
            type Err = ();
            fn from_str(input: &str) -> Result<Self, Self::Err> {
                match input {
                    $($name => Ok(Syscall::$identifier)),*,
                    _ => Err(()),
                }
            }
        }

        impl From<Syscall> for u32 {
            fn from(syscall: Syscall) -> Self {
                syscall as Self
            }
        }

        impl core::convert::TryFrom<u32> for Syscall {
            type Error = ();
            fn try_from(value: u32) -> Result<Self, Self::Error> {
                match value {
                    $($num => Ok(Syscall::$identifier)),*,
                    _ => Err(()),
                }
            }
        }
    }
}

// Generate `Syscall` enum with supported syscalls and their numbers.
syscalls!(
    (0, Input, "input"),
    (1, DataIdentifier, "data_identifier"),
    (2, Output, "output"),
    (3, PoseidonGL, "poseidon_gl"),
    (4, Affine256, "affine_256"),
    (5, EcAdd, "ec_add"),
    (6, EcDouble, "ec_double"),
    (8, Mod256, "mod_256"),
    (7, KeccakF, "keccakf"),
);
