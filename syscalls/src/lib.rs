#![no_std]

macro_rules! syscalls {
    ($(($num:expr, $identifier:ident, $name:expr)),* $(,)?) => {
        /// We use repr(u8) to make sure the enum discriminant will fit into the
        /// 12 bits of the immediate field of the `addi` instruction,
        #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
        #[repr(u8)]
        pub enum Syscall {
            $($identifier = $num),*
        }

        impl Syscall {
            pub fn name(&self) -> &'static str {
                match self {
                    $(Syscall::$identifier => $name),*
                }
            }
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

        impl From<Syscall> for u8 {
            fn from(syscall: Syscall) -> Self {
                syscall as Self
            }
        }

        impl core::convert::TryFrom<u8> for Syscall {
            type Error = ();
            fn try_from(value: u8) -> Result<Self, Self::Error> {
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
    (1, Input, "input"),
    (2, Output, "output"),
    (3, PoseidonGL, "poseidon_gl"),
    (4, Affine256, "affine_256"),
    (5, EcAdd, "ec_add"),
    (6, EcDouble, "ec_double"),
    (7, KeccakF, "keccakf"),
    (8, Mod256, "mod_256"),
    (9, Halt, "halt"),
    (10, Poseidon2GL, "poseidon2_gl"),
    (11, NativeHash, "native_hash"),
    (12, CommitPublic, "commit_public"),
    (13, InvertGL, "invert_gl"),
    (14, SplitGLVec, "split_gl_vec"),
    (15, MergeGL, "merge_gl"),
);
