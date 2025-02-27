use core::{arch::asm, mem::MaybeUninit};
use powdr_riscv_syscalls::Syscall;

pub const PRIME: u64 = 0xffffffff00000001;

/// Goldilocks field element.
///
/// Not a legal value in RISC-V, as it uses a supposedly
/// 32-bit memory word to store a full field element.
///
/// But it is more efficient when handled by Powdr machines.
///
/// TODO: remove the other Golilocks and the functions that use it,
/// and replace it with this one.
///
/// TODO: it might be necessary to do some Pin black magic to prevent
/// the compiler from trying to move this value using normal RISC-V
/// instructions.
#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct OpaqueGoldilocks(u32);

impl From<u32> for OpaqueGoldilocks {
    fn from(value: u32) -> Self {
        // This is a trivial conversion because the valid u32 values are
        // a subset of the valid Goldilocks values.
        Self(value)
    }
}

impl From<Goldilocks> for OpaqueGoldilocks {
    fn from(value: Goldilocks) -> Self {
        let low = value.0 as u32;
        let high = (value.0 >> 32) as u32;
        unsafe {
            let mut output = MaybeUninit::uninit();
            ecall!(Syscall::MergeGL,
                in("a0") low,
                in("a1") high,
                in("a2") output.as_mut_ptr());
            output.assume_init()
        }
    }
}

/// Extract the Goldilocks values from the OpaqueGoldilocks values.
pub fn extract_opaque_vec8(vec: &[OpaqueGoldilocks; 8]) -> [u64; 8] {
    unsafe {
        let mut output: MaybeUninit<[u64; 8]> = MaybeUninit::uninit();
        ecall!(Syscall::SplitGLVec, in("a0") vec, in("a1") output.as_mut_ptr());
        output.assume_init()
    }
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Goldilocks(
    /// Canonical representation, only Goldilocks values are valid.
    u64,
);

impl Goldilocks {
    /// Panics if the value is not in the field (i.e. panics if value >= PRIME).
    pub fn new(value: u64) -> Self {
        assert!(value < PRIME);
        Self(value)
    }

    /// Precondition for safety: value must be in the field (i.e. value < PRIME).
    pub unsafe fn new_unchecked(value: u64) -> Self {
        Self(value)
    }

    /// Returns the inverse of the field element.
    pub fn inverse(self) -> Self {
        let mut low = self.0 as u32;
        let mut high = (self.0 >> 32) as u32;
        unsafe {
            ecall!(Syscall::InvertGL,
                inout("a0") low,
                inout("a1") high);
            Self::new_unchecked((high as u64) << 32 | low as u64)
        }
    }

    // TODO: maybe add other operations we can accelerate with ecalls:
    // - add
    // - mul
    // - neg
}

impl From<Goldilocks> for u64 {
    /// Return the canonical u64 representation of the field element.
    fn from(value: Goldilocks) -> u64 {
        value.0
    }
}
