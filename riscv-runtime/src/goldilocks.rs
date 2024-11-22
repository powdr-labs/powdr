use core::arch::asm;
use powdr_riscv_syscalls::Syscall;

pub const PRIME: u64 = 0xffffffff00000001;

#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Goldilocks(
    // TODO: maybe represent this as a u32, which ends up as a full word in Powdr.
    // (the conversion to and from u64 would require ecalls)
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
