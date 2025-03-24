#![no_std]

macro_rules! syscalls {
    ($(($num:expr, $identifier:ident, $name:expr, $input_count:expr, $output_count:expr)),* $(,)?) => {
        /// We use repr(u8) to make sure the enum discriminant will fit into the
        /// 12 bits of the immediate field of the `addi` instruction,
        #[derive(Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
        #[repr(u8)]
        pub enum Syscall {
            $($identifier = $num),*
        }

        impl Syscall {
            pub const fn name(&self) -> &'static str {
                match self {
                    $(Syscall::$identifier => $name),*
                }
            }

            pub const fn input_count(&self) -> usize {
                match self {
                    $(Syscall::$identifier => $input_count),*
                }
            }

            pub const fn output_count(&self) -> usize {
                match self {
                    $(Syscall::$identifier => $output_count),*
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
    (1, Input, "input", 2, 1),
    (2, Output, "output", 2, 0),
    (3, PoseidonGL, "poseidon_gl", 1, 0),
    (4, Affine256, "affine_256", 4, 0),
    (5, EcAdd, "ec_add", 3, 0),
    (6, EcDouble, "ec_double", 2, 0),
    (7, KeccakF, "keccakf", 2, 0),
    (8, Mod256, "mod_256", 3, 0),
    (9, Halt, "halt", 0, 0),
    (10, Poseidon2GL, "poseidon2_gl", 2, 0),
    (11, NativeHash, "native_hash", 1, 0),
    (12, CommitPublic, "commit_public", 2, 0),
    (13, InvertGL, "invert_gl", 2, 2),
    (14, SplitGLVec, "split_gl_vec", 2, 0),
    (15, MergeGL, "merge_gl", 3, 0),
);
